use std::collections::HashMap;

use cx_hmir::{
    HMIRDef, HMIRDefID, HMIRUnit,
    ty::{HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRMoveSemantics},
};
use cx_namespace::module::NamespacePath;
use cx_tokens::TokenRange;

pub(crate) struct HMIRUnitBuilder {
    namespace: NamespacePath,
    types: HMIRTypeBuilder,
    defs: HashMap<HMIRDefID, HMIRDef>,
}

#[derive(Default)]
pub(crate) struct HMIRTypeBuilder {
    types: HashMap<HMIRTypeID, HMIRType>,
    primitives: Vec<HMIRTypeID>,
}

impl HMIRUnitBuilder {
    pub(crate) fn new(namespace: NamespacePath) -> Self {
        Self {
            namespace,
            types: HMIRTypeBuilder::default(),
            defs: HashMap::new(),
        }
    }

    pub(crate) fn types_mut(&mut self) -> &mut HMIRTypeBuilder {
        &mut self.types
    }

    pub(crate) fn push_def(&mut self, def: HMIRDef) -> HMIRDefID {
        let id = HMIRDefID::new(self.defs.len());
        self.defs.insert(id, def);
        id
    }

    pub(crate) fn finish(self) -> HMIRUnit {
        HMIRUnit::new(self.namespace, self.types.types, self.defs)
    }
}

impl HMIRTypeBuilder {
    // Declares a type of its own; structurally equal declarations stay distinct
    pub(crate) fn declare(
        &mut self,
        debug_name: Option<String>,
        kind: HMIRTypeKind,
        semantics: HMIRMoveSemantics,
        unsafe_move: bool,
        span: &TokenRange,
    ) -> HMIRTypeID {
        let id = HMIRTypeID::new(self.types.len());
        let ty = HMIRType::new(id, debug_name, kind, semantics, unsafe_move, span.clone());

        self.types.insert(id, ty);
        id
    }

    pub(crate) fn primitive(&mut self, kind: HMIRTypeKind) -> HMIRTypeID {
        let existing = self
            .primitives
            .iter()
            .find(|id| same_primitive(self.types[id].kind(), &kind));
        if let Some(existing) = existing {
            return *existing;
        }

        let span = TokenRange::internal();
        let id = self.declare(None, kind, HMIRMoveSemantics::POD, false, &span);

        self.primitives.push(id);
        id
    }
}

fn same_primitive(a: &HMIRTypeKind, b: &HMIRTypeKind) -> bool {
    match (a, b) {
        (HMIRTypeKind::Void, HMIRTypeKind::Void)
        | (HMIRTypeKind::Unreachable, HMIRTypeKind::Unreachable)
        | (HMIRTypeKind::Type, HMIRTypeKind::Type)
        | (HMIRTypeKind::Str, HMIRTypeKind::Str) => true,

        (HMIRTypeKind::Int(a), HMIRTypeKind::Int(b)) => a == b,
        (HMIRTypeKind::Float(a), HMIRTypeKind::Float(b)) => a == b,

        (
            HMIRTypeKind::Opaque {
                size: a_size,
                alignment: a_alignment,
            },
            HMIRTypeKind::Opaque {
                size: b_size,
                alignment: b_alignment,
            },
        ) => a_size == b_size && a_alignment == b_alignment,

        _ => false,
    }
}
