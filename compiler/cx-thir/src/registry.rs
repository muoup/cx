use std::collections::HashMap;

use cx_target::ArchitectureConfig;
use cx_util::namespace::QualifiedName;

use crate::{
    thir::contextual_eq::TypeContextEqual,
    thir::data::{THIRType, THIRTypeID},
    type_context::THIRTypeContext,
};

//
// After the evaluation and completion of the MIRUnit, this struct contains all necessary context to interpret
// the complete meaning of its contents. For instance, prototypes are not necessary to provide here as a map as
// they are either tacked onto the function definition nodes or in the types applied to the AST nodes, however
// mapping type ids is required as later steps need to be able to interpret type definitions.
//
#[derive(Debug, Clone)]
pub struct THIRDecomposedRegistry {
    architecture: ArchitectureConfig,
    typeid_map: HashMap<THIRTypeID, THIRType>,
    intrinsic_types: HashMap<String, THIRTypeID>,
    identified_types: HashMap<QualifiedName, THIRTypeID>,
    named_types: HashMap<String, THIRTypeID>,
    type_id_bound: u64,
}

impl THIRDecomposedRegistry {
    pub fn new(
        architecture: ArchitectureConfig,
        typeid_map: HashMap<THIRTypeID, THIRType>,
        intrinsic_types: HashMap<String, THIRTypeID>,
        type_id_bound: u64,
    ) -> Self {
        let mut identified_types = HashMap::new();
        let mut named_types = HashMap::new();
        for (id, ty) in &typeid_map {
            if let Some(name) = &ty.lookup_identifier {
                identified_types.entry(name.clone()).or_insert(*id);
            }
            if let Some(name) = &ty.strong_identifier {
                named_types.entry(name.clone()).or_insert(*id);
            }
        }
        Self {
            architecture,
            typeid_map,
            intrinsic_types,
            identified_types,
            named_types,
            type_id_bound,
        }
    }

    pub fn intrinsic_type_id(&self, name: &str) -> Option<THIRTypeID> {
        self.intrinsic_types.get(name).copied()
    }

    pub fn type_id(&self, ty: &THIRType) -> Option<THIRTypeID> {
        let identified = ty
            .lookup_identifier
            .as_ref()
            .and_then(|name| self.identified_types.get(name));
        let named = ty
            .strong_identifier
            .as_ref()
            .and_then(|name| self.named_types.get(name));

        identified
            .copied()
            .into_iter()
            .chain(named.copied())
            .find(|id| {
                self.typeid_map
                    .get(id)
                    .is_some_and(|registered| registered.contextual_eq(ty, self))
            })
            .or_else(|| {
                self.intrinsic_types
                    .values()
                    .copied()
                    .filter(|id| {
                        self.typeid_map
                            .get(id)
                            .is_some_and(|registered| registered.contextual_eq(ty, self))
                    })
                    .min()
            })
    }

    pub fn type_id_bound(&self) -> u64 {
        self.type_id_bound
    }
}

impl THIRTypeContext for THIRDecomposedRegistry {
    fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    fn resolve_type_id(&self, id: THIRTypeID) -> &THIRType {
        self.typeid_map
            .get(&id)
            .unwrap_or_else(|| panic!("Invalid id {id} in MIRDecomposedRegistry!"))
    }

    fn try_resolve_type_id(&self, id: THIRTypeID) -> Option<&THIRType> {
        self.typeid_map.get(&id)
    }
}
