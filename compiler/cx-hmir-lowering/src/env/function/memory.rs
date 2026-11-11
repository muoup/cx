mod codec;

use rustc_hash::FxHashMap;
use std::{collections::BTreeMap, ops::Range};

use cx_hmir::{
    HMIRDefID,
    ty::{HMIRTypeID, aggregate::HMIRBitfield},
};
use cx_log::{CXResult, catalogue::mir, error::CXError};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::symbol,
    log::hmir_error,
    lower::declaration,
    value::{
        HMIRPlaceID, HMIRValueKind,
        address::{Addressable, HMIRAddress},
        immediate::HMIRImmediate,
    },
};

pub(crate) use codec::{field_offset, layout, placement};

// Regions are places of the comptime evaluation; globals and literals read at comptime are regions of their own
#[derive(Debug, Default)]
pub(crate) struct Memory {
    regions: FxHashMap<HMIRPlaceID, Image>,
    globals: FxHashMap<HMIRDefID, HMIRPlaceID>,
    literals: FxHashMap<String, HMIRPlaceID>,
    next: usize,
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
    value: HMIRValueKind,
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
    Nested,
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
            Self::Nested => "reading an address of comptime storage as part of a value".into(),
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

    fn cell(&self, offset: usize, size: usize) -> Option<&HMIRValueKind> {
        self.cells
            .get(&offset)
            .filter(|cell| cell.size == size)
            .map(|cell| &cell.value)
    }

    fn put_cell(&mut self, offset: usize, size: usize, value: HMIRValueKind) {
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
    fn allocate(&mut self, image: Image) -> HMIRPlaceID {
        let region = HMIRPlaceID::new(self.next);
        self.next += 1;
        self.regions.insert(region, image);
        region
    }

    fn literal(&mut self, text: &str) -> HMIRPlaceID {
        if let Some(region) = self.literals.get(text) {
            return *region;
        }
        let mut image = Image::zeroed(text.len() + 1);
        image.put(0, text.as_bytes());
        let region = self.allocate(image);
        self.literals.insert(text.to_string(), region);
        region
    }

    fn range(&self, region: HMIRPlaceID, offset: i64, size: usize) -> Result<Range<usize>, Fault> {
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

    fn load(&self, location: &HMIRAddress<HMIRPlaceID>, size: usize) -> Result<Image, Fault> {
        let range = self.range(*location.base(), location.offset(), size)?;
        self.regions[location.base()].slice(range)
    }

    fn store(&mut self, location: &HMIRAddress<HMIRPlaceID>, image: &Image) -> Result<(), Fault> {
        let range = self.range(*location.base(), location.offset(), image.len())?;
        self.regions
            .get_mut(location.base())
            .ok_or(Fault::Dangling)?
            .overwrite(range.start, image);
        Ok(())
    }
}

impl LocalLoweringContext<'_, '_, '_> {
    pub(crate) fn allocate(&mut self, ty: HMIRTypeID, span: &TokenRange) -> CXResult<HMIRPlaceID> {
        let size = layout(self.env_mut(), ty, span)?.size();
        let image = Image::uninitialized(size);
        Ok(self.memory.borrow_mut().allocate(image))
    }

    // The region and offset an address designates; a static address is only read through
    pub(crate) fn locate(
        &mut self,
        address: &HMIRValueKind,
        write: bool,
        span: &TokenRange,
    ) -> CXResult<HMIRAddress<HMIRPlaceID>> {
        let invalid = |operation: &str| {
            Err(hmir_error(
                span,
                &mir::COMPTIME_INVALID_OPERATION,
                operation.into(),
            ))
        };
        let address = match address {
            HMIRValueKind::Reference(local) => return Ok(local.clone()),
            HMIRValueKind::Immediate(HMIRImmediate::Int(0)) => {
                let operation = "dereference".into();
                return Err(hmir_error(span, &mir::COMPTIME_UNDEFINED_ARITHMETIC, operation));
            }
            HMIRValueKind::Immediate(HMIRImmediate::Address(address)) => address,
            _ => return invalid("accessing storage through this address"),
        };
        let region = match address.base() {
            Addressable::Def(_) | Addressable::Literal(_) if write => {
                return invalid("writing to storage outside of this evaluation");
            }
            Addressable::Def(def)
                if !declaration::is_function(self.env().symbols().resolve_definition(*def)) =>
            {
                self.global(*def, span)?
            }
            Addressable::Literal(text) => self.memory.borrow_mut().literal(text),
            _ => return invalid("accessing storage through this address"),
        };
        Ok(HMIRAddress::new(region, address.offset()))
    }

    pub(crate) fn load(
        &mut self,
        location: &HMIRAddress<HMIRPlaceID>,
        ty: HMIRTypeID,
        bitfield: Option<HMIRBitfield>,
        span: &TokenRange,
    ) -> CXResult<HMIRValueKind> {
        let size = layout(self.env_mut(), ty, span)?.size();
        let image = self
            .memory
            .borrow()
            .load(location, size)
            .map_err(|fault| fault.error(span))?;
        match bitfield {
            Some(bitfield) => codec::extract(self.env(), ty, &image, bitfield, span),
            None => codec::decode(self.env_mut(), ty, &image, span),
        }
    }

    pub(crate) fn store(
        &mut self,
        location: &HMIRAddress<HMIRPlaceID>,
        ty: HMIRTypeID,
        value: &HMIRValueKind,
        bitfield: Option<HMIRBitfield>,
        span: &TokenRange,
    ) -> CXResult<()> {
        let image = match bitfield {
            Some(bitfield) => {
                let size = layout(self.env_mut(), ty, span)?.size();
                let unit = self
                    .memory
                    .borrow()
                    .load(location, size)
                    .map_err(|fault| fault.error(span))?;
                codec::insert(&unit, value, bitfield)
            }
            None => codec::encode(self.env_mut(), ty, value, span)?,
        };
        self.memory
            .borrow_mut()
            .store(location, &image)
            .map_err(|fault| fault.error(span))
    }

    fn global(&mut self, def: HMIRDefID, span: &TokenRange) -> CXResult<HMIRPlaceID> {
        if let Some(region) = self.memory.borrow().globals.get(&def) {
            return Ok(*region);
        }
        let value = symbol::initialize(self, def)?;
        let image = codec::encode(self.env_mut(), value.ty(), value.kind(), span)?;
        let mut memory = self.memory.borrow_mut();
        let region = memory.allocate(image);
        memory.globals.insert(def, region);
        Ok(region)
    }
}
