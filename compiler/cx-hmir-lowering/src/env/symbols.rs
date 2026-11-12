use cx_hir::ast::{modifiers::VisibilityMode, types::HIRTagKind};
use rustc_hash::FxHashMap;
use std::{ops::Deref, sync::Arc};

use cx_hmir::{HMIRDef, HMIRDefID, HMIRUnit, HMIRUnitID, unit::symbols::HMIRLookupClass};
use cx_namespace::{lookup::candidates, module::{NamespacePath, QualifiedName}};
use cx_pipeline_data::hmir::HMIRUnitRegistry;

///
/// Handles accessing the HMIR environment. An HMIR environment contains both the HMIR module itself, as well as
/// public interfaces of imported modules. For things like comptime functions, the symbol environment is used as
/// a lazy loader that handles caching and loading such that external symbols can be used as if they were local.
///
#[derive(Debug)]
pub struct HMIRSymbolEnv<'global, 'hmir> {
    registry: &'global HMIRUnitRegistry,
    local_unit: &'hmir HMIRUnit,

    units: FxHashMap<HMIRUnitID, Arc<HMIRUnit>>,
    lookups: FxHashMap<(HMIRUnitID, Option<HIRTagKind>), FxHashMap<QualifiedName, Vec<HMIRDefID>>>,
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

            units: FxHashMap::default(),
            lookups: FxHashMap::default(),
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

    fn loaded_owner(&self, def_id: HMIRDefID) -> &HMIRUnit {
        if def_id.brand() == self.local_unit.id() {
            return self.local_unit;
        }

        self.units
            .get(&def_id.brand())
            .expect("Definition's owning unit was never loaded")
    }

    // Every unit a lookup can see has finished HMIR before this unit is lowered, so a found result never changes
    pub fn lookup(
        &mut self,
        source: HMIRUnitID,
        name: &QualifiedName,
        tag: Option<HIRTagKind>,
    ) -> Vec<HMIRDefID> {
        if let Some(ids) = self.lookups.get(&(source, tag)).and_then(|names| names.get(name)) {
            return ids.clone();
        }
        let ids = self.search(source, name, tag);
        if !ids.is_empty() {
            self.lookups
                .entry((source, tag))
                .or_default()
                .insert(name.clone(), ids.clone());
        }
        ids
    }

    fn search(
        &mut self,
        source: HMIRUnitID,
        name: &QualifiedName,
        tag: Option<HIRTagKind>,
    ) -> Vec<HMIRDefID> {
        let Some(source) = self.owner(source) else {
            return Vec::new();
        };
        let mut scope = vec![source.clone()];
        for import in source.symbols().imports() {
            if let Some(unit) = self.unit(import)
                && scope.iter().all(|other| other.id() != unit.id())
            {
                scope.push(unit);
            }
        }
        let mut found = Vec::new();
        for name in candidates(source.namespace(), name, source.symbols().aliases()) {
            for class in [HMIRLookupClass::Ordinary, HMIRLookupClass::Tag] {
                if tag.is_some() && class == HMIRLookupClass::Ordinary {
                    continue;
                }
                let mut exact = Vec::new();
                let mut defs = self.registry.symbols(&name, class);
                defs.retain(|id| id.brand() != self.local_unit.id() && id.brand() != source.id());
                defs.extend(source.symbols().symbol(&name, class));
                if source.id() != self.local_unit.id() {
                    defs.extend(self.local_unit.symbols().symbol(&name, class));
                }
                for id in defs {
                    let Some(unit) = scope.iter().find(|unit| unit.id() == id.brand()) else {
                        continue;
                    };
                    let def = unit
                        .resolve_def(id)
                        .expect("indexed symbol belongs to its unit");
                    let own = unit.id() == source.id();
                    if !own
                        && !def
                            .declarations()
                            .iter()
                            .any(|decl| match decl.visibility() {
                                VisibilityMode::Public => true,
                                VisibilityMode::Package => name
                                    .namespace
                                    .clone()
                                    .strip_prefix(source.namespace())
                                    .is_some(),
                                _ => false,
                            })
                    {
                        continue;
                    }
                    let level = if !own {
                        0
                    } else if name.namespace == *source.namespace() {
                        2
                    } else {
                        1
                    };
                    let matched = def.declarations().iter().any(|decl| decl.tag() == tag);
                    exact.push(((level, matched), id));
                }
                if !exact.is_empty() {
                    found.extend(exact);
                    break;
                }
            }
        }
        let best = found.iter().map(|(rank, _)| *rank).max();
        let mut ids = found
            .into_iter()
            .filter(|(rank, _)| Some(*rank) == best)
            .map(|(_, id)| id)
            .collect::<Vec<_>>();
        ids.sort();
        ids.dedup();
        ids
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
