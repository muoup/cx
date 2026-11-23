use rustc_hash::FxHashMap;
use std::{cell::RefCell, collections::BTreeMap};

use chl_hmir::ty::context::HMIRTypeContext;
use chl_log::CHLResult;
use chl_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, resolver::demand},
    eval::{symbol, types},
    lower::declaration,
    value::{
        HMIRValueKind,
        address::{Addressable, HMIRAddress},
        immediate::HMIRImmValue,
        place::HMIRPlaceID,
    },
};

use super::{Fault, Memory, codec::layout};

#[derive(Debug, Default)]
pub(super) struct AddressSpace {
    last: u128,
    bases: FxHashMap<Base, u128>,
    ranges: BTreeMap<u128, (Base, usize)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Base {
    Local(HMIRPlaceID),
    Symbol(Addressable),
    Fixed(HMIRValueKind),
}

impl AddressSpace {
    fn base(&self, base: &Base) -> Option<(u128, usize)> {
        let start = *self.bases.get(base)?;
        Some((start, self.ranges[&start].1))
    }

    fn reserve(
        &mut self,
        base: Base,
        size: usize,
        alignment: usize,
        width: usize,
    ) -> Result<u128, Fault> {
        let start = self
            .last
            .checked_add(1)
            .and_then(|next| next.checked_next_multiple_of(alignment as u128))
            .ok_or(Fault::AddressSpaceFull)?;
        let end = start
            .checked_add(size as u128)
            .ok_or(Fault::AddressSpaceFull)?;
        if end > u128::MAX >> ((16 - width) * 8) {
            return Err(Fault::AddressSpaceFull);
        }
        self.last = end;
        self.bases.insert(base.clone(), start);
        self.ranges.insert(start, (base, size));
        Ok(start)
    }

    pub(super) fn release(&mut self, local: HMIRPlaceID) {
        if let Some(start) = self.bases.remove(&Base::Local(local)) {
            self.ranges.remove(&start);
        }
    }

    pub(super) fn decode(&self, bytes: &[u8]) -> Result<HMIRValueKind, Fault> {
        let mut buffer = [0; 16];
        buffer[..bytes.len()].copy_from_slice(bytes);
        let address = u128::from_le_bytes(buffer);
        if address == 0 {
            return Ok(HMIRImmValue::Int(0).into());
        }
        let Some((&start, (base, size))) = self.ranges.range(..=address).next_back() else {
            return Err(Fault::InvalidAddress);
        };
        let offset = address - start;
        if offset > *size as u128 {
            return Err(Fault::InvalidAddress);
        }
        let offset = i64::try_from(offset).map_err(|_| Fault::InvalidAddress)?;
        Ok(match base {
            Base::Local(place) => HMIRAddress::new(*place, offset).into(),
            Base::Symbol(symbol) => {
                HMIRImmValue::Address(HMIRAddress::new(symbol.clone(), offset)).into()
            }
            Base::Fixed(value) => value.clone(),
        })
    }
}

pub(super) fn encode(
    env: &mut HMIREnvironment,
    memory: &RefCell<Memory>,
    value: &HMIRValueKind,
    span: &TokenRange,
) -> CHLResult<[u8; 16]> {
    let fault = |fault: Fault| fault.error(span);
    let (mut base, mut offset) = match value {
        HMIRValueKind::Reference(address) => (Base::Local(*address.base()), address.offset()),
        HMIRValueKind::Immediate(HMIRImmValue::Address(address)) => {
            (Base::Symbol(address.base().clone()), address.offset())
        }
        HMIRValueKind::Immediate(HMIRImmValue::Int(0)) => return Ok([0; 16]),
        HMIRValueKind::Application(_)
        | HMIRValueKind::Immediate(
            HMIRImmValue::Application(_) | HMIRImmValue::Constructor { .. },
        ) => (Base::Fixed(value.clone()), 0),
        _ => return Err(fault(Fault::InvalidAddress)),
    };
    let reserved = memory.borrow().addresses.base(&base);
    let (start, size) = if let Some(range) = reserved {
        range
    } else {
        let (size, alignment) = match extent(env, memory, &base, span)? {
            Some(extent) => extent,
            None => {
                base = Base::Fixed(value.clone());
                offset = 0;
                (0, 1)
            }
        };
        let mut memory = memory.borrow_mut();
        let start = match memory.addresses.base(&base) {
            Some((start, _)) => start,
            None => memory
                .addresses
                .reserve(base, size, alignment, env.architecture().pointer_size())
                .map_err(fault)?,
        };
        (start, size)
    };
    let offset = usize::try_from(offset)
        .ok()
        .filter(|offset| *offset <= size)
        .ok_or_else(|| fault(Fault::InvalidAddress))?;
    Ok((start + offset as u128).to_le_bytes())
}

fn extent(
    env: &mut HMIREnvironment,
    memory: &RefCell<Memory>,
    base: &Base,
    span: &TokenRange,
) -> CHLResult<Option<(usize, usize)>> {
    Ok(match base {
        Base::Local(place) => {
            let memory = memory.borrow();
            let region = memory.region(*place).map_err(|fault| fault.error(span))?;
            Some((region.image.len(), region.alignment))
        }
        Base::Symbol(Addressable::Def(def))
            if !declaration::is_function(env.symbols().resolve_definition(*def)) =>
        {
            let ty = symbol::value_type(env, *def)?;
            demand::force(env, ty)?;
            if !types::is_complete(env, ty) {
                return Ok(None);
            }
            let layout = layout(env, ty, span)?;
            Some((layout.size(), layout.alignment()))
        }
        Base::Symbol(Addressable::Literal(text)) => Some((text.len() + 1, 1)),
        _ => Some((0, 1)),
    })
}
