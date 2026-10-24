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
        let kind = match value.kind() {
            HMIRValueKind::Constant(constant) => {
                self.canonical_constant(constant.clone(), Some(ty))
            }
            _ => return value,
        };
        HMIRValue::constant(ty, kind)
    }

    fn canonical_constant(
        &mut self,
        value: HMIRConstant,
        expected: Option<HMIRTypeID>,
    ) -> HMIRConstant {
        match value {
            HMIRConstant::Type(ty) => HMIRConstant::Type(self.canonical_type(ty)),
            HMIRConstant::Int { value, ty } => {
                let ty = self.canonical_type(expected.unwrap_or(ty));
                let value = match self.resolve_kind(ty) {
                    HMIRTypeKind::Int(integer) => int::normalize(value, *integer),
                    _ => value,
                };
                HMIRConstant::Int { value, ty }
            }
            HMIRConstant::Float { value, ty } => HMIRConstant::Float {
                value,
                ty: self.canonical_type(expected.unwrap_or(ty)),
            },
            HMIRConstant::Null(ty) => {
                HMIRConstant::Null(self.canonical_type(expected.unwrap_or(ty)))
            }
            HMIRConstant::Aggregate { ty, fields } => {
                let ty = self.canonical_type(expected.unwrap_or(ty));
                let kind = self.resolve_kind(ty).clone();
                let sparse = matches!(
                    kind,
                    HMIRTypeKind::Struct { .. } | HMIRTypeKind::Array { .. }
                );
                let mut fields = fields
                    .into_iter()
                    .map(|(index, value)| {
                        let field = match &kind {
                            HMIRTypeKind::Array { element, .. } => Some(*element),
                            HMIRTypeKind::Struct { fields }
                            | HMIRTypeKind::Union { fields }
                            | HMIRTypeKind::TaggedUnion { fields } => {
                                fields.get(index).map(|field| *field.ty())
                            }
                            _ => None,
                        };
                        (index, Box::new(self.canonical_constant(*value, field)))
                    })
                    .collect::<Vec<_>>();
                fields.sort_by_key(|(index, _)| *index);
                fields.dedup_by_key(|(index, _)| *index);
                if sparse {
                    fields.retain(|(_, value)| !zero(value));
                }
                HMIRConstant::Aggregate { ty, fields }
            }
            HMIRConstant::Address { place, path, ty } => HMIRConstant::Address {
                place,
                path,
                ty: self.canonical_type(expected.unwrap_or(ty)),
            },
            HMIRConstant::LabelAddress {
                function,
                label,
                ty,
            } => HMIRConstant::LabelAddress {
                function,
                label,
                ty: self.canonical_type(expected.unwrap_or(ty)),
            },
            HMIRConstant::Function { def, args } => HMIRConstant::Function {
                def,
                args: args
                    .into_iter()
                    .map(|arg| self.canonical_constant(arg, None))
                    .collect(),
            },
            value => value,
        }
    }
}

fn zero(value: &HMIRConstant) -> bool {
    match value {
        HMIRConstant::Unit | HMIRConstant::Int { value: 0, .. } | HMIRConstant::Null(_) => true,
        HMIRConstant::Float { value, .. } => *value == 0.0.into(),
        HMIRConstant::Aggregate { fields, .. } => fields.is_empty(),
        _ => false,
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
            | HMIRTypeKind::StagedExpr { .. }
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
        HMIRTypeKind::PointerTo(inner) | HMIRTypeKind::ReferenceTo(inner) => {
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
        HMIRTypeKind::FunctionItem(def) => def.hash(hash),
        HMIRTypeKind::Nominal(_) => unreachable!("underlying types are not aliases"),
        _ => {}
    }
    Some(())
}
