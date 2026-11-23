mod address;
mod codec;

use rustc_hash::FxHashMap;
use std::{cell::RefCell, collections::BTreeMap, fmt, ops::Range, rc::Rc};

use chl_hmir::{
    HMIRDefID,
    ty::{HMIRTypeID, aggregate::HMIRBitfield},
};
use chl_log::{CHLResult, catalogue::mir, error::CHLError};
use chl_tokens::TokenRange;

use self::{address::AddressSpace, codec::Codec};

use crate::{
    env::function::LocalLoweringContext,
    eval::symbol,
    log::hmir_error,
    lower::declaration,
    value::{
        HMIRValueKind,
        address::{Addressable, HMIRAddress},
        immediate::HMIRImmValue,
        place::{HMIRPageID, HMIRPlaceBrand, HMIRPlaceID},
    },
};

pub(crate) use codec::{layout, member_layout};

// Comptime storage is kept in pages, each place a region of one. Globals and literals read at comptime live in the
// static page, which is never released
#[derive(Debug)]
pub(crate) struct Memory {
    pages: FxHashMap<HMIRPageID, Vec<Region>>,
    addresses: AddressSpace,
    globals: FxHashMap<HMIRDefID, HMIRPlaceID>,
    literals: FxHashMap<String, HMIRPlaceID>,
    next: usize,
}

const STATICS: HMIRPageID = HMIRPageID::new(0);

// A page of comptime memory, released once its owner drops it
pub(crate) struct Page {
    id: HMIRPageID,
    memory: Rc<RefCell<Memory>>,
}

#[derive(Debug)]
struct Region {
    image: Image,
    alignment: usize,
}

#[derive(Debug)]
struct Image {
    bytes: Vec<u8>,
    states: Vec<ByteState>,
    cells: BTreeMap<usize, Cell>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ByteState {
    Uninitialized,
    Data,
    Address,
}

#[derive(Debug, Clone)]
struct Cell {
    ty: HMIRTypeID,
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
    Opaque,
    InvalidAddress,
    AddressSpaceFull,
    Discriminant,
    Nested,
    Relocation,
}

impl Fault {
    fn error(self, span: &TokenRange) -> CHLError {
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
            Self::Opaque => "accessing an opaque value through an incompatible type".into(),
            Self::InvalidAddress => "the bytes do not encode a known address".into(),
            Self::AddressSpaceFull => "the virtual address space is exhausted".into(),
            Self::Discriminant => "reading an invalid tagged union discriminant".into(),
            Self::Nested => "reading an address of comptime storage as part of a value".into(),
            Self::Relocation => "materializing a union without its stored address".into(),
        };
        hmir_error(span, &mir::COMPTIME_INVALID_ACCESS, reason)
    }
}

impl Image {
    fn new(size: usize, state: ByteState) -> Self {
        Self {
            bytes: vec![0; size],
            states: vec![state; size],
            cells: BTreeMap::new(),
        }
    }

    fn len(&self) -> usize {
        self.bytes.len()
    }

    fn bytes(&self, range: Range<usize>) -> Result<&[u8], Fault> {
        if self.cells.range(range.clone()).next().is_some() {
            return Err(Fault::Opaque);
        }
        if self.states[range.clone()].contains(&ByteState::Uninitialized) {
            return Err(Fault::Uninitialized);
        }
        Ok(&self.bytes[range])
    }

    fn put(&mut self, offset: usize, bytes: &[u8]) {
        self.bytes[offset..offset + bytes.len()].copy_from_slice(bytes);
    }

    fn slice(&self, range: Range<usize>) -> Image {
        Image {
            bytes: self.bytes[range.clone()].to_vec(),
            states: self.states[range.clone()].to_vec(),
            cells: self
                .cells
                .range(range.clone())
                .map(|(offset, cell)| (offset - range.start, cell.clone()))
                .collect(),
        }
    }

    fn overwrite(&mut self, offset: usize, source: Image) {
        let range = offset..offset + source.len();
        let stale = self
            .cells
            .range(range.clone())
            .map(|(start, _)| *start)
            .collect::<Vec<_>>();
        for start in stale {
            self.cells.remove(&start);
        }
        self.bytes[range.clone()].copy_from_slice(&source.bytes);
        self.states[range].copy_from_slice(&source.states);
        self.cells.extend(
            source
                .cells
                .into_iter()
                .map(|(start, cell)| (offset + start, cell)),
        );
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self {
            pages: FxHashMap::from_iter([(STATICS, Vec::new())]),
            addresses: AddressSpace::default(),
            globals: FxHashMap::default(),
            literals: FxHashMap::default(),
            next: STATICS.index() + 1,
        }
    }
}

impl Memory {
    // Page ids are never reused, so a place that outlives its page cannot reach a later one
    fn page(&mut self) -> HMIRPageID {
        let page = HMIRPageID::new(self.next);
        self.next += 1;
        self.pages.insert(page, Vec::new());
        page
    }

    fn allocate(&mut self, page: HMIRPageID, image: Image, alignment: usize) -> HMIRPlaceID {
        let regions = self
            .pages
            .get_mut(&page)
            .expect("allocating in a released page");
        regions.push(Region { image, alignment });
        HMIRPlaceID::new(HMIRPlaceBrand::comptime(page), regions.len() - 1)
    }

    fn region(&self, place: HMIRPlaceID) -> Result<&Region, Fault> {
        let page = place.brand().page().ok_or(Fault::Dangling)?;
        let regions = self.pages.get(&page).ok_or(Fault::Dangling)?;
        regions.get(place.index()).ok_or(Fault::Dangling)
    }

    fn region_mut(&mut self, place: HMIRPlaceID) -> Result<&mut Region, Fault> {
        let page = place.brand().page().ok_or(Fault::Dangling)?;
        let regions = self.pages.get_mut(&page).ok_or(Fault::Dangling)?;
        regions.get_mut(place.index()).ok_or(Fault::Dangling)
    }

    fn literal(&mut self, text: &str) -> HMIRPlaceID {
        if let Some(region) = self.literals.get(text) {
            return *region;
        }
        let mut image = Image::new(text.len() + 1, ByteState::Data);
        image.put(0, text.as_bytes());
        let region = self.allocate(STATICS, image, 1);
        self.literals.insert(text.to_string(), region);
        region
    }

    fn range(&self, region: HMIRPlaceID, offset: i64, size: usize) -> Result<Range<usize>, Fault> {
        let capacity = self.region(region)?.image.len();
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
        Ok(self.region(*location.base())?.image.slice(range))
    }

    fn store(&mut self, location: &HMIRAddress<HMIRPlaceID>, image: Image) -> Result<(), Fault> {
        let range = self.range(*location.base(), location.offset(), image.len())?;
        self.region_mut(*location.base())?
            .image
            .overwrite(range.start, image);
        Ok(())
    }
}

impl Page {
    pub(crate) fn new(memory: &Rc<RefCell<Memory>>) -> Self {
        let id = memory.borrow_mut().page();
        Self {
            id,
            memory: Rc::clone(memory),
        }
    }

    pub(crate) fn id(&self) -> HMIRPageID {
        self.id
    }
}

impl Drop for Page {
    fn drop(&mut self) {
        let mut memory = self.memory.borrow_mut();
        if let Some(regions) = memory.pages.remove(&self.id) {
            let brand = HMIRPlaceBrand::comptime(self.id);
            for index in 0..regions.len() {
                memory.addresses.release(HMIRPlaceID::new(brand, index));
            }
        }
    }
}

impl fmt::Debug for Page {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

impl LocalLoweringContext<'_, '_, '_> {
    pub(crate) fn allocate(&mut self, ty: HMIRTypeID, span: &TokenRange) -> CHLResult<HMIRPlaceID> {
        let layout = layout(self.env_mut(), ty, span)?;
        let image = Image::new(layout.size(), ByteState::Uninitialized);
        Ok(self
            .memory
            .borrow_mut()
            .allocate(self.comptime().page(), image, layout.alignment()))
    }

    // The region and offset an address designates; a static address is only read through
    pub(crate) fn locate(
        &mut self,
        address: &HMIRValueKind,
        write: bool,
        span: &TokenRange,
    ) -> CHLResult<HMIRAddress<HMIRPlaceID>> {
        let invalid = |operation: &str| {
            Err(hmir_error(
                span,
                &mir::COMPTIME_INVALID_OPERATION,
                operation.into(),
            ))
        };
        let address = match address {
            HMIRValueKind::Reference(local) => return Ok(local.clone()),
            HMIRValueKind::Immediate(HMIRImmValue::Int(0)) => {
                let operation = "dereference".into();
                return Err(hmir_error(
                    span,
                    &mir::COMPTIME_UNDEFINED_ARITHMETIC,
                    operation,
                ));
            }
            HMIRValueKind::Immediate(HMIRImmValue::Address(address)) => address,
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
    ) -> CHLResult<HMIRValueKind> {
        let (location, size, bitfield) = self.access(location, ty, bitfield, span)?;
        let image = self
            .memory
            .borrow()
            .load(&location, size)
            .map_err(|fault| fault.error(span))?;
        match bitfield {
            Some(bitfield) => codec::extract(self.env(), ty, &image, bitfield, span),
            None => Codec::new(self.env, &self.memory, span).decode(ty, &image),
        }
    }

    pub(crate) fn store(
        &mut self,
        location: &HMIRAddress<HMIRPlaceID>,
        ty: HMIRTypeID,
        value: &HMIRValueKind,
        bitfield: Option<HMIRBitfield>,
        span: &TokenRange,
    ) -> CHLResult<()> {
        let (location, size, bitfield) = self.access(location, ty, bitfield, span)?;
        let image = match bitfield {
            Some(bitfield) => {
                let unit = self
                    .memory
                    .borrow()
                    .load(&location, size)
                    .map_err(|fault| fault.error(span))?;
                codec::insert(unit, value, bitfield)
            }
            None => Codec::new(self.env, &self.memory, span).encode(ty, value)?,
        };
        self.memory
            .borrow_mut()
            .store(&location, image)
            .map_err(|fault| fault.error(span))
    }

    pub(crate) fn copy(
        &mut self,
        target: &HMIRAddress<HMIRPlaceID>,
        source: &HMIRAddress<HMIRPlaceID>,
        ty: HMIRTypeID,
        span: &TokenRange,
    ) -> CHLResult<()> {
        let size = layout(self.env_mut(), ty, span)?.size();
        let mut memory = self.memory.borrow_mut();
        memory
            .load(source, size)
            .and_then(|image| memory.store(target, image))
            .map_err(|fault| fault.error(span))
    }

    fn access(
        &mut self,
        location: &HMIRAddress<HMIRPlaceID>,
        ty: HMIRTypeID,
        bitfield: Option<HMIRBitfield>,
        span: &TokenRange,
    ) -> CHLResult<(HMIRAddress<HMIRPlaceID>, usize, Option<HMIRBitfield>)> {
        Ok(match bitfield {
            Some(bitfield) => {
                let (window, field) = codec::bitfield_window(bitfield);
                let location = location
                    .offset_by(window.start as i64)
                    .ok_or_else(|| hmir_error(span, &mir::COMPTIME_POINTER_OVERFLOW, ()))?;
                (location, window.len(), Some(field))
            }
            None => (
                location.clone(),
                layout(self.env_mut(), ty, span)?.size(),
                None,
            ),
        })
    }

    // The variant the tagged union of type 'ty' at 'location' holds
    pub(crate) fn variant(
        &mut self,
        location: &HMIRAddress<HMIRPlaceID>,
        ty: HMIRTypeID,
        span: &TokenRange,
    ) -> CHLResult<usize> {
        let size = layout(self.env_mut(), ty, span)?.size();
        let image = self
            .memory
            .borrow()
            .load(location, size)
            .map_err(|fault| fault.error(span))?;
        Codec::new(self.env, &self.memory, span).discriminant(ty, &image, 0)
    }

    fn global(&mut self, def: HMIRDefID, span: &TokenRange) -> CHLResult<HMIRPlaceID> {
        if let Some(region) = self.memory.borrow().globals.get(&def) {
            return Ok(*region);
        }
        let value = symbol::initialize(self, def)?;
        let ty = value.ty();
        let alignment = layout(self.env_mut(), ty, span)?.alignment();
        let image = Codec::new(self.env, &self.memory, span).encode(ty, value.kind())?;
        let mut memory = self.memory.borrow_mut();
        let region = memory.allocate(STATICS, image, alignment);
        memory.globals.insert(def, region);
        Ok(region)
    }
}
