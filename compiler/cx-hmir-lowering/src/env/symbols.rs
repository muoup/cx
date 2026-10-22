pub mod lookup;

use std::{collections::HashMap, ops::Deref, sync::Arc};

use cx_hir::ast::types::HIRTagKind;
use cx_hmir::{HMIRDef, HMIRDefID, HMIRUnit, unit::symbols::HMIRDeclaration};
use cx_namespace::{
    lookup::{QualifiedLookup, QualifiedLookupResult},
    module::{NamespacePath, QualifiedName},
};
use cx_pipeline_data::db::ModuleMap;

use crate::env::symbols::lookup::{Candidate, HMIRLookup, HMIRSymbolGroup, ScopeLookup};

///
/// Handles accessing the HMIR environment. An HMIR environment contains both the HMIR module itself, as well as
/// public interfaces of imported modules. For things like comptime functions, the symbol environment is used as
/// a lazy loader that handles caching and loading such that external symbols can be used as if they were local.
///
#[derive(Debug)]
pub struct HMIRSymbolEnv<'global, 'hmir> {
    module_map: &'global ModuleMap<HMIRUnit>,
    local_unit: &'hmir HMIRUnit,

    units: HashMap<NamespacePath, Arc<HMIRUnit>>,
    externals: Vec<(NamespacePath, HMIRDefID)>,
    external_ids: HashMap<(NamespacePath, HMIRDefID), HMIRDefID>,
    lookups: HashMap<LookupKey, HMIRLookup>,
}

#[derive(Debug, Clone)]
pub enum HMIRUnitHandle<'hmir> {
    Local(&'hmir HMIRUnit),
    Imported(Arc<HMIRUnit>),
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct LookupKey {
    owner: NamespacePath,
    lexical_namespace: NamespacePath,
    name: QualifiedName,
    tag: Option<HIRTagKind>,
}

const EXTERNAL_BIT: usize = 1 << 63;

impl<'global, 'hmir> HMIRSymbolEnv<'global, 'hmir> {
    pub fn new(module_map: &'global ModuleMap<HMIRUnit>, local_unit: &'hmir HMIRUnit) -> Self {
        Self {
            module_map,
            local_unit,

            units: HashMap::new(),
            externals: Vec::new(),
            external_ids: HashMap::new(),
            lookups: HashMap::new(),
        }
    }

    pub fn local_unit(&self) -> &'hmir HMIRUnit {
        self.local_unit
    }

    pub fn unit(&mut self, namespace: &NamespacePath) -> Option<HMIRUnitHandle<'hmir>> {
        if namespace == self.local_unit.namespace() {
            return Some(HMIRUnitHandle::Local(self.local_unit));
        }

        if let Some(unit) = self.units.get(namespace) {
            return Some(HMIRUnitHandle::Imported(unit.clone()));
        }

        let unit = self.module_map.lock().get(namespace)?.clone();
        self.units.insert(namespace.clone(), unit.clone());
        Some(HMIRUnitHandle::Imported(unit))
    }

    pub fn identity(&self, def_id: HMIRDefID) -> (&NamespacePath, HMIRDefID) {
        if !is_external_id(def_id) {
            return (self.local_unit.namespace(), def_id);
        }

        let (unit, def) = &self.externals[def_id.index() & !EXTERNAL_BIT];
        (unit, *def)
    }

    pub fn resolve_definition(&self, def_id: HMIRDefID) -> &HMIRDef {
        let (unit, def) = self.owning_unit(def_id);
        unit.resolve_def(def)
            .expect("Definition not found in its owning unit")
    }

    pub fn declarations(&self, def_id: HMIRDefID) -> &[HMIRDeclaration] {
        let (unit, def) = self.owning_unit(def_id);
        unit.symbols().declarations(def)
    }

    pub fn lookup(
        &mut self,
        owner: &NamespacePath,
        lexical_namespace: &NamespacePath,
        name: &QualifiedName,
        tag: Option<HIRTagKind>,
    ) -> HMIRLookup {
        let key = LookupKey {
            owner: owner.clone(),
            lexical_namespace: lexical_namespace.clone(),
            name: name.clone(),
            tag,
        };
        if let Some(cached) = self.lookups.get(&key) {
            return cached.clone();
        }

        if lexical_namespace.clone().strip_prefix(owner).is_none() {
            return HMIRLookup::NotFound;
        }
        let Some(scope) = self.scope(owner, tag) else {
            return HMIRLookup::NotFound;
        };

        let result = self.scoped_lookup(&scope, lexical_namespace, name, tag);
        self.lookups.insert(key, result.clone());
        result
    }

    fn intern(&mut self, unit: &NamespacePath, def: HMIRDefID) -> HMIRDefID {
        if unit == self.local_unit.namespace() {
            return def;
        }

        let key = (unit.clone(), def);
        if let Some(id) = self.external_ids.get(&key) {
            return *id;
        }

        let id = HMIRDefID::new(self.externals.len() | EXTERNAL_BIT);
        self.externals.push(key.clone());
        self.external_ids.insert(key, id);
        id
    }

    fn owning_unit(&self, def_id: HMIRDefID) -> (&HMIRUnit, HMIRDefID) {
        let (unit, def) = self.identity(def_id);
        
        if !is_external_id(def_id) {
            return (self.local_unit, def);
        }
        
        (&self.units[unit], def)
    }

    fn scope(
        &mut self,
        owner: &NamespacePath,
        tag: Option<HIRTagKind>,
    ) -> Option<ScopeLookup<'hmir>> {
        let owner = self.unit(owner)?;
        let mut units = vec![owner.clone()];
        for import in owner.symbols().imports() {
            units.push(self.unit(import)?);
        }
        Some(ScopeLookup { units, tag })
    }

    fn scoped_lookup(
        &mut self,
        scope: &ScopeLookup,
        lexical_namespace: &NamespacePath,
        name: &QualifiedName,
        tag: Option<HIRTagKind>,
    ) -> HMIRLookup {
        match scope.qualified_lookup(lexical_namespace, name) {
            QualifiedLookupResult::NotFound => HMIRLookup::NotFound,
            QualifiedLookupResult::Found {
                resolved_name,
                value,
            } => HMIRLookup::Found {
                incompatible_tag: value.incompatible_tag(tag),
                group: self.group(resolved_name, &value),
            },
            QualifiedLookupResult::Ambiguous { candidates } => HMIRLookup::Ambiguous(
                candidates
                    .into_iter()
                    .filter_map(|candidate| {
                        let value = scope.lookup_exact(lexical_namespace, &candidate)?;
                        Some(self.group(candidate, &value))
                    })
                    .collect(),
            ),
        }
    }

    fn group(&mut self, name: QualifiedName, candidate: &Candidate) -> HMIRSymbolGroup {
        let unit = candidate.unit.namespace();
        let defs = candidate
            .defs
            .iter()
            .map(|def| self.intern(unit, *def))
            .collect();
        HMIRSymbolGroup::new(name, defs)
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

fn is_external_id(def_id: HMIRDefID) -> bool {
    def_id.index() & EXTERNAL_BIT != 0
}