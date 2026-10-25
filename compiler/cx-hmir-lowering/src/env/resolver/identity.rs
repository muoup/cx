use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    mem::discriminant,
};

use cx_hmir::ty::{
    HMIRTypeID, HMIRTypeKind, aggregate::HMIRMoveSemantics, context::HMIRTypeContext,
};

use crate::{
    env::HMIREnvironment,
    symbol::composite::alike,
    value::scalar::int,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

impl HMIREnvironment<'_, '_> {
    fn canonical_type(&mut self, ty: HMIRTypeID) -> HMIRTypeID {
        let mut hasher = DefaultHasher::new();
        if fingerprint(self, ty, &mut hasher).is_none() {
            return ty;
        }
        let hash = hasher.finish();
        if let Some(candidates) = self.resolver().identities.get(&hash) {
            for candidate in candidates {
                if alike(self, *candidate, ty, true) {
                    return *candidate;
                }
            }
        }
        self.resolver_mut()
            .identities
            .entry(hash)
            .or_default()
            .push(ty);
        ty
    }

    pub fn canonical_value(&mut self, value: HMIRValue) -> HMIRValue {
        let ty = self.canonical_type(value.ty());
        let HMIRValueKind::Constant(constant) = value.kind() else {
            return value;
        };
        HMIRValue::constant(ty, self.canonical_constant(constant.clone(), ty))
    }

    fn canonical_constant(&mut self, value: HMIRConstant, ty: HMIRTypeID) -> HMIRConstant {
        let kind = self.resolve_kind(ty).clone();
        match (value, &kind) {
            (HMIRConstant::Type(ty), _) => HMIRConstant::Type(self.canonical_type(ty)),
            (HMIRConstant::Int(value), HMIRTypeKind::Int(integer)) => {
                HMIRConstant::Int(int::normalize(value, *integer))
            }
            (HMIRConstant::Aggregate(items), _) => HMIRConstant::Aggregate(
                items
                    .into_iter()
                    .enumerate()
                    .map(|(index, item)| {
                        let field = match &kind {
                            HMIRTypeKind::Array { element, .. } => *element,
                            HMIRTypeKind::Struct { fields } => *fields[index].ty(),
                            _ => unreachable!("an aggregate constant is an array or structure"),
                        };
                        self.canonical_constant(item, field)
                    })
                    .collect(),
            ),
            (HMIRConstant::Union { member, value }, HMIRTypeKind::Union { fields }) => {
                HMIRConstant::Union {
                    member,
                    value: Box::new(self.canonical_constant(*value, *fields[member].ty())),
                }
            }
            (
                HMIRConstant::TaggedUnion { variant, payload },
                HMIRTypeKind::TaggedUnion { fields },
            ) => HMIRConstant::TaggedUnion {
                variant,
                payload: Box::new(self.canonical_constant(*payload, *fields[variant].ty())),
            },
            (HMIRConstant::Curried { callee, args }, _) => HMIRConstant::Curried {
                callee: Box::new(self.canonical_value(*callee)),
                args: args
                    .into_iter()
                    .map(|arg| self.canonical_value(arg))
                    .collect(),
            },
            (value, _) => value,
        }
    }
}

fn fingerprint(env: &HMIREnvironment, ty: HMIRTypeID, hash: &mut impl Hasher) -> Option<()> {
    if !env.resolver().settled(ty) {
        return None;
    }
    let underlying = env.underlying(ty);
    let kind = env.resolve_kind(ty);
    let mut semantics = env.semantics(ty);
    if matches!(
        kind,
        HMIRTypeKind::Struct { .. }
            | HMIRTypeKind::Union { .. }
            | HMIRTypeKind::TaggedUnion { .. }
            | HMIRTypeKind::Opaque { .. }
            | HMIRTypeKind::Staged(_)
    ) {
        let base = env.semantics(underlying);
        semantics.constant &= !base.constant;
        semantics.unsafe_move &= !base.unsafe_move;
        if semantics.move_semantics == base.move_semantics {
            semantics.move_semantics = HMIRMoveSemantics::POD;
        }
        true.hash(hash);
        underlying.hash(hash);
        semantics.hash(hash);
        return Some(());
    }
    false.hash(hash);
    semantics.hash(hash);
    discriminant(kind).hash(hash);
    match kind {
        HMIRTypeKind::Int(int) => int.hash(hash),
        HMIRTypeKind::Float(float) => float.hash(hash),
        HMIRTypeKind::PointerTo(inner) => fingerprint(env, *inner, hash)?,
        HMIRTypeKind::ReferenceTo { inner, bitfield } => {
            bitfield.hash(hash);
            fingerprint(env, *inner, hash)?;
        }
        HMIRTypeKind::Array { element, length } => {
            length.hash(hash);
            fingerprint(env, *element, hash)?;
        }
        HMIRTypeKind::Function(signature) => {
            signature.stage().hash(hash);
            signature.is_variadic().hash(hash);
            signature.contract().is_safe().hash(hash);
            signature.params().len().hash(hash);
            for param in signature.params() {
                param.comptime().hash(hash);
                fingerprint(env, *param.ty(), hash)?;
            }
            fingerprint(env, *signature.return_type(), hash)?;
        }
        HMIRTypeKind::Nominal(_) => unreachable!("underlying types are not aliases"),
        _ => {}
    }
    Some(())
}
