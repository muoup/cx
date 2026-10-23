use cx_hmir::{
    HMIRDefID,
    ty::{
        HMIRExprTypeKind, HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics,
        context::HMIRTypeContext,
    },
};
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, resolver::HMIRResolver},
    symbol::SymbolState,
};

#[derive(Debug, Clone, Copy)]
pub(crate) struct ProvisionalType {
    pub ty: HMIRTypeID,
    pub reservation: HMIRTypeID,
}

#[derive(Debug)]
pub(crate) struct TypeFrame {
    pub def: HMIRDefID,
    pub constructors: Vec<TypeConstructor>,
}

#[derive(Debug, Clone)]
pub(crate) struct TypeConstructor {
    kind: ConstructorKind,
    semantics: HMIRTypeSemantics,
    span: TokenRange,
}

#[derive(Debug, Clone, Copy)]
enum ConstructorKind {
    Pointer,
    Reference,
    Qualify,
}

impl ProvisionalType {
    pub fn available(self, env: &HMIREnvironment, id: HMIRDefID) -> bool {
        if !env.resolver().is_incomplete(self.reservation)
            || !matches!(
                env.resolve_kind(self.reservation),
                HMIRTypeKind::Function(_)
            )
        {
            return true;
        }
        env.resolver()
            .active_symbols
            .iter()
            .skip_while(|active| **active != id)
            .any(|active| {
                let Some(SymbolState::InProgress(Some(provisional))) =
                    env.resolver().symbol_state(*active)
                else {
                    return false;
                };
                matches!(
                    env.resolve_kind(provisional.reservation),
                    HMIRTypeKind::Opaque { .. }
                )
            })
    }
}

impl TypeConstructor {
    pub fn new(
        kind: &HMIRExprTypeKind,
        semantics: HMIRTypeSemantics,
        span: &TokenRange,
    ) -> Option<Self> {
        let kind = match kind {
            HMIRExprTypeKind::PointerTo(_) => ConstructorKind::Pointer,
            HMIRExprTypeKind::ReferenceTo(_) => ConstructorKind::Reference,
            HMIRExprTypeKind::Nominal(_) => ConstructorKind::Qualify,
            _ => return None,
        };
        Some(Self {
            kind,
            semantics,
            span: span.clone(),
        })
    }

    pub fn apply(self, resolver: &mut HMIRResolver, inner: HMIRTypeID) -> HMIRTypeID {
        let mut semantics = self.semantics;
        let kind = match self.kind {
            ConstructorKind::Pointer => HMIRTypeKind::PointerTo(inner),
            ConstructorKind::Reference => HMIRTypeKind::ReferenceTo(inner),
            ConstructorKind::Qualify => {
                let current = resolver
                    .realized_type(inner)
                    .map(|ty| *ty.semantics())
                    .unwrap_or_default();
                semantics = current.apply(semantics);
                if current == semantics {
                    return inner;
                }
                HMIRTypeKind::Nominal(inner)
            }
        };
        resolver.realize_type(HMIRType::new(kind, semantics, self.span, None))
    }
}
