mod codec;

use std::{
    cell::Cell as Counter,
    collections::{BTreeMap, HashMap},
    ops::Range,
};

use cx_hmir::{
    HMIRDefID,
    ty::{HMIRTypeID, aggregate::HMIRBitfield},
};
use cx_log::{CXResult, catalogue::mir, error::CXError};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    log::hmir_error,
    symbol,
    value::{
        HMIRPlace, HMIRRegionID, HMIRValueKind,
        address::{Addressable, HMIRAddress},
        constant::HMIRConstant,
    },
};

pub(crate) use codec::{field_offset, layout, placement};

thread_local! {
    static NEXT_REGION: Counter<usize> = const { Counter::new(0) };
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Memory {
    regions: HashMap<HMIRRegionID, Image>,
    globals: HashMap<HMIRDefID, HMIRRegionID>,
}

#[derive(Debug, Clone)]
struct Image {
    bytes: Vec<u8>,
    init: Vec<bool>,
    cells: BTreeMap<usize, Cell>,
}

#[derive(Debug, Clone)]
struct Cell {
    size: usize,
    value: HMIRConstant,
}

#[derive(Debug, Clone, Copy)]
enum Fault {
    Dangling,
    Bounds {
        offset: i64,
        size: usize,
        capacity: usize,
    },
    Uninitialized,
    Fragment,
    Opaque,
    Forged,
    Discriminant,
}

impl Fault {
    fn error(self, span: &TokenRange) -> CXError {
        let reason = match self {
            Self::Dangling => "the storage it addresses is not available here".into(),
            Self::Bounds {
                offset,
                size,
                capacity,
            } => format!(
                "{size} bytes at offset {offset} are outside of the {capacity}-byte allocation"
            ),
            Self::Uninitialized => "reading uninitialized bytes".into(),
            Self::Fragment => "reading part of a stored address".into(),
            Self::Opaque => "reading a stored address as plain bytes".into(),
            Self::Forged => "reading plain bytes as an address".into(),
            Self::Discriminant => "reading an invalid tagged union discriminant".into(),
        };
        hmir_error(span, &mir::COMPTIME_INVALID_ACCESS, reason)
    }
}

impl Image {
    fn zeroed(size: usize) -> Self {
        Self {
            bytes: vec![0; size],
            init: vec![true; size],
            cells: BTreeMap::new(),
        }
    }

    fn uninitialized(size: usize) -> Self {
        Self {
            bytes: vec![0; size],
            init: vec![false; size],
            cells: BTreeMap::new(),
        }
    }

    fn len(&self) -> usize {
        self.bytes.len()
    }

    fn intersecting(&self, range: Range<usize>) -> impl Iterator<Item = (usize, &Cell)> {
        self.cells
            .range(..range.end)
            .rev()
            .take_while(move |(start, cell)| *start + cell.size > range.start)
            .map(|(start, cell)| (*start, cell))
    }

    fn bytes(&self, range: Range<usize>) -> Result<&[u8], Fault> {
        if self.intersecting(range.clone()).next().is_some() {
            return Err(Fault::Opaque);
        }
        if !self.init[range.clone()].iter().all(|init| *init) {
            return Err(Fault::Uninitialized);
        }
        Ok(&self.bytes[range])
    }

    fn put(&mut self, offset: usize, bytes: &[u8]) {
        self.bytes[offset..offset + bytes.len()].copy_from_slice(bytes);
    }

    fn cell(&self, offset: usize, size: usize) -> Option<&HMIRConstant> {
        self.cells
            .get(&offset)
            .filter(|cell| cell.size == size)
            .map(|cell| &cell.value)
    }

    fn put_cell(&mut self, offset: usize, size: usize, value: HMIRConstant) {
        self.cells.insert(offset, Cell { size, value });
    }

    fn slice(&self, range: Range<usize>) -> Result<Image, Fault> {
        let mut cells = BTreeMap::new();
        for (start, cell) in self.intersecting(range.clone()) {
            if start < range.start || start + cell.size > range.end {
                return Err(Fault::Fragment);
            }
            cells.insert(start - range.start, cell.clone());
        }
        Ok(Image {
            bytes: self.bytes[range.clone()].to_vec(),
            init: self.init[range].to_vec(),
            cells,
        })
    }

    fn overwrite(&mut self, offset: usize, source: &Image) {
        let range = offset..offset + source.len();
        let stale = self
            .intersecting(range.clone())
            .map(|(start, cell)| (start, cell.size))
            .collect::<Vec<_>>();
        for (start, size) in stale {
            self.cells.remove(&start);
            for byte in (start..start + size).filter(|byte| !range.contains(byte)) {
                self.init[byte] = false;
            }
        }
        self.bytes[range.clone()].copy_from_slice(&source.bytes);
        self.init[range].copy_from_slice(&source.init);
        for (start, cell) in &source.cells {
            self.cells.insert(offset + start, cell.clone());
        }
    }
}

impl Memory {
    fn allocate(&mut self, image: Image) -> HMIRRegionID {
        let region = NEXT_REGION.with(|next| {
            let id = next.get();
            next.set(id + 1);
            HMIRRegionID::new(id)
        });
        self.regions.insert(region, image);
        region
    }

    fn range(&self, region: HMIRRegionID, offset: i64, size: usize) -> Result<Range<usize>, Fault> {
        let capacity = self.regions.get(&region).ok_or(Fault::Dangling)?.len();
        usize::try_from(offset)
            .ok()
            .filter(|start| start.checked_add(size).is_some_and(|end| end <= capacity))
            .map(|start| start..start + size)
            .ok_or(Fault::Bounds {
                offset,
                size,
                capacity,
            })
    }

    fn load(&self, region: HMIRRegionID, offset: i64, size: usize) -> Result<Image, Fault> {
        let range = self.range(region, offset, size)?;
        self.regions[&region].slice(range)
    }

    fn store(&mut self, region: HMIRRegionID, offset: i64, image: &Image) -> Result<(), Fault> {
        let range = self.range(region, offset, image.len())?;
        self.regions
            .get_mut(&region)
            .ok_or(Fault::Dangling)?
            .overwrite(range.start, image);
        Ok(())
    }
}

impl LocalLoweringContext<'_, '_, '_> {
    pub(crate) fn allocate(&mut self, ty: HMIRTypeID, span: &TokenRange) -> CXResult<HMIRRegionID> {
        let size = layout(self.env_mut(), ty, span)?.size();
        let image = Image::uninitialized(size);
        Ok(self.memory.borrow_mut().allocate(image))
    }

    pub(crate) fn load(
        &mut self,
        address: &HMIRAddress,
        ty: HMIRTypeID,
        bitfield: Option<HMIRBitfield>,
        span: &TokenRange,
    ) -> CXResult<HMIRConstant> {
        let region = self.region(address, false, span)?;
        let size = layout(self.env_mut(), ty, span)?.size();
        let image = self
            .memory
            .borrow()
            .load(region, address.offset(), size)
            .map_err(|fault| fault.error(span))?;
        match bitfield {
            Some(bitfield) => codec::extract(self.env(), ty, &image, bitfield, span),
            None => codec::decode(self.env_mut(), ty, &image, 0, span),
        }
    }

    pub(crate) fn store(
        &mut self,
        address: &HMIRAddress,
        ty: HMIRTypeID,
        value: &HMIRConstant,
        bitfield: Option<HMIRBitfield>,
        span: &TokenRange,
    ) -> CXResult<()> {
        let region = self.region(address, true, span)?;
        let image = match bitfield {
            Some(bitfield) => {
                let size = layout(self.env_mut(), ty, span)?.size();
                let unit = self
                    .memory
                    .borrow()
                    .load(region, address.offset(), size)
                    .map_err(|fault| fault.error(span))?;
                codec::insert(&unit, value, bitfield)
            }
            None => codec::encode(self.env_mut(), ty, value, span)?,
        };
        self.memory
            .borrow_mut()
            .store(region, address.offset(), &image)
            .map_err(|fault| fault.error(span))
    }

    pub(crate) fn copy(
        &mut self,
        target: &HMIRAddress,
        source: &HMIRAddress,
        ty: HMIRTypeID,
        span: &TokenRange,
    ) -> CXResult<()> {
        let size = layout(self.env_mut(), ty, span)?.size();
        let from = self.region(source, false, span)?;
        let to = self.region(target, true, span)?;
        let mut memory = self.memory.borrow_mut();
        memory
            .load(from, source.offset(), size)
            .and_then(|image| memory.store(to, target.offset(), &image))
            .map_err(|fault| fault.error(span))
    }

    fn region(
        &mut self,
        address: &HMIRAddress,
        write: bool,
        span: &TokenRange,
    ) -> CXResult<HMIRRegionID> {
        match address.base() {
            Addressable::Place(HMIRPlace::Region(region)) => Ok(*region),
            Addressable::Place(HMIRPlace::Global(_)) if write => Err(hmir_error(
                span,
                &mir::COMPTIME_INVALID_OPERATION,
                "writing to a runtime global".into(),
            )),
            Addressable::Place(HMIRPlace::Global(def)) => self.global(*def, span),
            _ => Err(hmir_error(
                span,
                &mir::COMPTIME_INVALID_OPERATION,
                "accessing storage through this address".into(),
            )),
        }
    }

    fn global(&mut self, def: HMIRDefID, span: &TokenRange) -> CXResult<HMIRRegionID> {
        if let Some(region) = self.memory.borrow().globals.get(&def) {
            return Ok(*region);
        }
        let value = symbol::initialize(self, def)?;
        let HMIRValueKind::Constant(constant) = value.kind() else {
            unreachable!("a global initializer is frozen");
        };
        let image = codec::encode(self.env_mut(), value.ty(), constant, span)?;
        let mut memory = self.memory.borrow_mut();
        let region = memory.allocate(image);
        memory.globals.insert(def, region);
        Ok(region)
    }
}
