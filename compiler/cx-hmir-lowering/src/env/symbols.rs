pub mod lookup;

use std::{collections::HashMap, ops::Deref, sync::Arc};

use cx_hir::ast::types::HIRTagKind;
use cx_hmir::{HMIRDef, HMIRDefID, HMIRUnit, HMIRUnitID, unit::symbols::HMIRDeclaration};
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_pipeline_data::hmir::HMIRUnitRegistry;

use crate::env::symbols::lookup::{HMIRLookup, ScopeLookup};

///
/// Handles accessing the HMIR environment. An HMIR environment contains both the HMIR module itself, as well as
/// public interfaces of imported modules. For things like comptime functions, the symbol environment is used as
/// a lazy loader that handles caching and loading such that external symbols can be used as if they were local.
///
#[derive(Debug)]
pub struct HMIRSymbolEnv<'global, 'hmir> {
    registry: &'global HMIRUnitRegistry,
    local_unit: &'hmir HMIRUnit,

    units: HashMap<HMIRUnitID, Arc<HMIRUnit>>,
}

#[derive(Debug, Clone)]
pub enum HMIRUnitHandle<'hmir> {
    Local(&'hmir HMIRUnit),
    Imported(Arc<HMIRUnit>),
}

impl<'global, 'hmir> HMIRSymbolEnv<'global, 'hmir> {
    pub fn new(registry: &'global HMIRUnitRegistry, local_unit: &'hmir HMIRUnit) -> Self {
        Self {
            registry,
            local_unit,

            units: HashMap::new(),
        }
    }

    pub fn local_unit(&self) -> &'hmir HMIRUnit {
        self.local_unit
    }

    pub fn unit(&mut self, namespace: &NamespacePath) -> Option<HMIRUnitHandle<'hmir>> {
        if namespace == self.local_unit.namespace() {
            return Some(HMIRUnitHandle::Local(self.local_unit));
        }

        let unit = self.registry.get(namespace)?;
        Some(HMIRUnitHandle::Imported(
            self.units.entry(unit.id()).or_insert(unit).clone(),
        ))
    }

    pub fn owner(&mut self, id: HMIRUnitID) -> Option<HMIRUnitHandle<'hmir>> {
        if id == self.local_unit.id() {
            return Some(HMIRUnitHandle::Local(self.local_unit));
        }

        if let Some(unit) = self.units.get(&id) {
            return Some(HMIRUnitHandle::Imported(unit.clone()));
        }

        let unit = self.registry.owner(id)?;
        self.units.insert(id, unit.clone());
        Some(HMIRUnitHandle::Imported(unit))
    }

    pub fn resolve_definition(&self, def_id: HMIRDefID) -> &HMIRDef {
        self.loaded_owner(def_id)
            .resolve_def(def_id)
            .expect("Definition not found in its owning unit")
    }

    pub fn declarations(&self, def_id: HMIRDefID) -> &[HMIRDeclaration] {
        self.loaded_owner(def_id).symbols().declarations(def_id)
    }

    pub fn lookup(
        &mut self,
        source: HMIRUnitID,
        name: &QualifiedName,
        tag: Option<HIRTagKind>,
    ) -> HMIRLookup {
        let Some(scope) = self.scope(source, tag) else {
            return HMIRLookup::NotFound;
        };

        scope.resolve(name)
    }

    fn loaded_owner(&self, def_id: HMIRDefID) -> &HMIRUnit {
        if def_id.brand() == self.local_unit.id() {
            return self.local_unit;
        }

        self.units
            .get(&def_id.brand())
            .expect("Definition's owning unit was never loaded")
    }

    fn scope(
        &mut self,
        source: HMIRUnitID,
        tag: Option<HIRTagKind>,
    ) -> Option<ScopeLookup<'global, 'hmir>> {
        let source = self.owner(source)?;
        let mut imports = Vec::<HMIRUnitHandle>::new();
        for import in source.symbols().imports() {
            let Some(unit) = self.unit(import) else {
                continue;
            };
            if unit.id() != source.id() && imports.iter().all(|other| other.id() != unit.id()) {
                imports.push(unit);
            }
        }

        Some(ScopeLookup {
            registry: self.registry,
            local: self.local_unit,
            source,
            imports,
            tag,
        })
    }
}

impl Deref for HMIRUnitHandle<'_> {
    type Target = HMIRUnit;

    fn deref(&self) -> &HMIRUnit {
        match self {
            HMIRUnitHandle::Local(unit) => unit,
            HMIRUnitHandle::Imported(unit) => unit,
        }
    }
}