use cx_hir::ast::{modifiers::VisibilityMode, types::HIRTagKind};
use cx_hmir::{HMIRDefID, HMIRUnitID, unit::symbols::HMIRLookupClass};
use cx_namespace::{lookup::candidates, module::QualifiedName};

use super::HMIRSymbolEnv;

impl HMIRSymbolEnv<'_, '_> {
    pub fn lookup(
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
