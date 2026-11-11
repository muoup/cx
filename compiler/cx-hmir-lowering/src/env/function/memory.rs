mod codec;

use rustc_hash::FxHashMap;
use std::{cell::RefCell, collections::BTreeMap, fmt, ops::Range, rc::Rc};

use cx_hmir::{
    HMIRDefID,
    ty::{HMIRTypeID, HMIRTypeKind, aggregate::HMIRBitfield, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::mir, error::CXError};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::symbol,
    log::hmir_error,
    lower::declaration,
    value::{
        HMIRValueKind,
        address::{Addressable, HMIRAddress},
        immediate::HMIRImmediate,
        place::{HMIRPageID, HMIRPlaceBrand, HMIRPlaceID},
    },
};

pub(crate) use codec::{field_offset, layout, placement};

// Comptime storage is kept in pages, each place a region of one. Globals and literals read at comptime live in the
// static page, which is never released
#[derive(Debug)]
pub(crate) struct Memory {
    pages: FxHashMap<HMIRPageID, Vec<Image>>,
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

impl Default for Memory {
    fn default() -> Self {
        Self {
            pages: FxHashMap::from_iter([(STATICS, Vec::new())]),
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

    fn allocate(&mut self, page: HMIRPageID, image: Image) -> HMIRPlaceID {
        let regions = self.pages.get_mut(&page).expect("allocating in a released page");
        regions.push(image);
        HMIRPlaceID::new(HMIRPlaceBrand::comptime(page), regions.len() - 1)
    }

    fn region(&self, place: HMIRPlaceID) -> Result<&Image, Fault> {
        let page = place.brand().page().ok_or(Fault::Dangling)?;
        let regions = self.pages.get(&page).ok_or(Fault::Dangling)?;
        regions.get(place.index()).ok_or(Fault::Dangling)
    }

    fn region_mut(&mut self, place: HMIRPlaceID) -> Result<&mut Image, Fault> {
        let page = place.brand().page().ok_or(Fault::Dangling)?;
        let regions = self.pages.get_mut(&page).ok_or(Fault::Dangling)?;
        regions.get_mut(place.index()).ok_or(Fault::Dangling)
    }

    fn literal(&mut self, text: &str) -> HMIRPlaceID {
        if let Some(region) = self.literals.get(text) {
            return *region;
        }
        let mut image = Image::zeroed(text.len() + 1);
        image.put(0, text.as_bytes());
        let region = self.allocate(STATICS, image);
        self.literals.insert(text.to_string(), region);
        region
    }

    fn range(&self, region: HMIRPlaceID, offset: i64, size: usize) -> Result<Range<usize>, Fault> {
        let capacity = self.region(region)?.len();
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
        self.region(*location.base())?.slice(range)
    }

    fn store(&mut self, location: &HMIRAddress<HMIRPlaceID>, image: &Image) -> Result<(), Fault> {
        let range = self.range(*location.base(), location.offset(), image.len())?;
        self.region_mut(*location.base())?.overwrite(range.start, image);
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
        self.memory.borrow_mut().pages.remove(&self.id);
    }
}

impl fmt::Debug for Page {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

impl LocalLoweringContext<'_, '_, '_> {
    pub(crate) fn allocate(&mut self, ty: HMIRTypeID, span: &TokenRange) -> CXResult<HMIRPlaceID> {
        let size = layout(self.env_mut(), ty, span)?.size();
        let image = Image::uninitialized(size);
        let page = self.comptime().page();
        Ok(self.memory.borrow_mut().allocate(page, image))
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

    pub(crate) fn copy(
        &mut self,
        target: &HMIRAddress<HMIRPlaceID>,
        source: &HMIRAddress<HMIRPlaceID>,
        ty: HMIRTypeID,
        span: &TokenRange,
    ) -> CXResult<()> {
        let size = layout(self.env_mut(), ty, span)?.size();
        let mut memory = self.memory.borrow_mut();
        memory
            .load(source, size)
            .and_then(|image| memory.store(target, &image))
            .map_err(|fault| fault.error(span))
    }

    // The variant the tagged union of type 'ty' at 'location' holds
    pub(crate) fn variant(
        &mut self,
        location: &HMIRAddress<HMIRPlaceID>,
        ty: HMIRTypeID,
        span: &TokenRange,
    ) -> CXResult<usize> {
        let HMIRTypeKind::TaggedUnion { fields } = self.env().resolve_kind(ty).clone() else {
            unreachable!("only a tagged union holds a variant")
        };
        let size = layout(self.env_mut(), ty, span)?.size();
        let image = self
            .memory
            .borrow()
            .load(location, size)
            .map_err(|fault| fault.error(span))?;
        codec::discriminant(self.env_mut(), &fields, &image, 0, span)
    }

    fn global(&mut self, def: HMIRDefID, span: &TokenRange) -> CXResult<HMIRPlaceID> {
        if let Some(region) = self.memory.borrow().globals.get(&def) {
            return Ok(*region);
        }
        let value = symbol::initialize(self, def)?;
        let image = codec::encode(self.env_mut(), value.ty(), value.kind(), span)?;
        let mut memory = self.memory.borrow_mut();
        let region = memory.allocate(STATICS, image);
        memory.globals.insert(def, region);
        Ok(region)
    }
}
