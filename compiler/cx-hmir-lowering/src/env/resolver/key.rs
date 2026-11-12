use std::mem::{Discriminant, discriminant};

use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, HMIRUndefined, aggregate::HMIRTypeSemantics};
use smallvec::SmallVec;

// The structure of a type, laid out by position so that each kind fills its lists in a fixed shape
#[derive(Debug, PartialEq, Eq, Hash)]
pub(super) struct TypeKey {
    kind: Discriminant<HMIRTypeKind>,
    semantics: HMIRTypeSemantics,
    types: SmallVec<[HMIRTypeID; 4]>,
    scalars: SmallVec<[u64; 4]>,
    names: Vec<Option<String>>,
}

pub(super) fn key(kind: &HMIRTypeKind, semantics: HMIRTypeSemantics) -> TypeKey {
    let mut key = TypeKey {
        kind: discriminant(kind),
        semantics,
        types: SmallVec::new(),
        scalars: SmallVec::new(),
        names: Vec::new(),
    };
    match kind {
        HMIRTypeKind::Int(int) => {
            key.scalars.push(int.width() as u64);
            key.scalars.push(int.signed() as u64);
        }
        HMIRTypeKind::Float(float) => key.scalars.push(float.width().bytes() as u64),
        HMIRTypeKind::PointerTo(inner) | HMIRTypeKind::Nominal(inner) | HMIRTypeKind::Staged(inner) => {
            key.types.push(*inner)
        }
        HMIRTypeKind::ReferenceTo { inner, bitfield } => {
            key.types.push(*inner);
            if let Some(bitfield) = bitfield {
                key.scalars.push(bitfield.offset as u64);
                key.scalars.push(bitfield.width as u64);
            }
        }
        HMIRTypeKind::Array { element, length } => {
            key.types.push(*element);
            key.scalars.push(*length);
        }
        HMIRTypeKind::Function { signature, decays } => {
            key.scalars.push(*decays as u64);
            key.scalars.push(signature.stage() as u64);
            key.scalars.push(signature.contract().is_safe() as u64);
            key.scalars.push(signature.is_variadic() as u64);
            for param in signature.params() {
                key.scalars.push(param.comptime() as u64);
                key.types.push(*param.ty());
            }
            key.types.push(*signature.return_type());
        }
        HMIRTypeKind::Struct { fields }
        | HMIRTypeKind::Union { fields }
        | HMIRTypeKind::TaggedUnion { fields } => {
            for field in fields {
                key.types.push(*field.ty());
                key.names.push(field.name().map(|name| name.as_string()));
                key.scalars.push(field.bit_width().map_or(0, |width| width as u64 + 1));
            }
        }
        HMIRTypeKind::Undefined(HMIRUndefined::Array { element }) => key.types.push(*element),
        HMIRTypeKind::Undefined(HMIRUndefined::Forward(tag)) => {
            key.scalars.push(tag.map_or(0, |tag| tag as u64 + 1))
        }
        HMIRTypeKind::Builtin {
            name,
            size,
            alignment,
        } => {
            key.names.push(Some(name.clone()));
            key.scalars.push(*size as u64);
            key.scalars.push(*alignment as u64);
        }
        HMIRTypeKind::Void
        | HMIRTypeKind::Unreachable
        | HMIRTypeKind::Str
        | HMIRTypeKind::Type
        | HMIRTypeKind::Dependent => {}
    }
    key
}
