use cx_hir::ast::{modifiers::VisibilityMode, types::HIRTagKind};
use cx_hmir::{
    HMIRDefID, HMIRUnit, HMIRUnitID,
    unit::symbols::{HMIRDeclaration, HMIRLookupClass},
};
use cx_namespace::{
    lookup::{QualifiedLookup, QualifiedLookupResult},
    module::{NamespacePath, QualifiedName},
};
use cx_pipeline_data::hmir::HMIRUnitRegistry;

use crate::env::symbols::HMIRUnitHandle;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HMIRSymbolGroup {
    name: QualifiedName,
    defs: Vec<HMIRDefID>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HMIRLookup {
    NotFound,
    Found {
        group: HMIRSymbolGroup,
        incompatible_tag: bool,
    },
    Ambiguous(Vec<HMIRSymbolGroup>),
}

pub(crate) struct ScopeLookup<'global, 'hmir> {
    pub(crate) registry: &'global HMIRUnitRegistry,
    pub(crate) local: &'hmir HMIRUnit,
    pub(crate) source: HMIRUnitHandle<'hmir>,
    pub(crate) imports: Vec<HMIRUnitHandle<'hmir>>,
    pub(crate) tag: Option<HIRTagKind>,
}

pub(crate) struct Candidate<'l> {
    unit: &'l HMIRUnit,
    name: QualifiedName,
    class: HMIRLookupClass,
    defs: Vec<HMIRDefID>,
    rank: (u8, bool),
}

impl HMIRSymbolGroup {
    fn new(name: QualifiedName, defs: Vec<HMIRDefID>) -> Self {
        Self { name, defs }
    }

    pub fn name(&self) -> &QualifiedName {
        &self.name
    }

    pub fn defs(&self) -> &[HMIRDefID] {
        &self.defs
    }
}

impl Candidate<'_> {
    fn declarations(&self) -> impl Iterator<Item = &HMIRDeclaration> {
        self.defs
            .iter()
            .flat_map(|id| self.unit.symbols().declarations(*id))
            .filter(|declaration| {
                *declaration.name() == self.name && declaration.class() == self.class
            })
    }

    fn incompatible_tag(&self, tag: Option<HIRTagKind>) -> bool {
        tag.is_some()
            && self
                .declarations()
                .any(|declaration| declaration.tag() != tag)
    }

    fn group(self) -> HMIRSymbolGroup {
        HMIRSymbolGroup::new(self.name, self.defs)
    }
}

impl ScopeLookup<'_, '_> {
    pub(crate) fn resolve(&self, name: &QualifiedName) -> HMIRLookup {
        let module = self.source.namespace();
        match self.qualified_lookup(module, name) {
            QualifiedLookupResult::NotFound => HMIRLookup::NotFound,
            QualifiedLookupResult::Found { value, .. } if value.len() == 1 => {
                let candidate = value.into_iter().next().unwrap();
                HMIRLookup::Found {
                    incompatible_tag: candidate.incompatible_tag(self.tag),
                    group: candidate.group(),
                }
            }
            QualifiedLookupResult::Found { value, .. } => {
                HMIRLookup::Ambiguous(value.into_iter().map(Candidate::group).collect())
            }
            QualifiedLookupResult::Ambiguous { candidates } => HMIRLookup::Ambiguous(
                candidates
                    .iter()
                    .filter_map(|candidate| self.lookup_exact(module, candidate))
                    .flatten()
                    .map(Candidate::group)
                    .collect(),
            ),
        }
    }

    fn unit(&self, id: HMIRUnitID) -> Option<&HMIRUnit> {
        std::iter::once(&self.source)
            .chain(&self.imports)
            .find(|unit| unit.id() == id)
            .map(|unit| &**unit)
    }

    fn candidates(&self, name: &QualifiedName, class: HMIRLookupClass) -> Vec<Candidate<'_>> {
        let local = self.local.symbols().group(name, class);
        let mut groups = self
            .registry
            .groups(name, class)
            .into_iter()
            .filter(|group| group[0].brand() != self.local.id())
            .collect::<Vec<_>>();
        if !local.is_empty() {
            groups.push(local.to_vec());
        }

        let mut candidates = groups
            .into_iter()
            .filter_map(|defs| {
                let mut candidate = Candidate {
                    unit: self.unit(defs[0].brand())?,
                    name: name.clone(),
                    class,
                    defs,
                    rank: (0, false),
                };
                if !self.visible(&candidate) {
                    return None;
                }
                candidate.rank = self.rank(&candidate);
                Some(candidate)
            })
            .collect::<Vec<_>>();

        let best = candidates.iter().map(|candidate| candidate.rank).max();
        candidates.retain(|candidate| Some(candidate.rank) == best);
        candidates
    }

    fn visible(&self, candidate: &Candidate) -> bool {
        candidate.unit.id() == self.source.id()
            || candidate.declarations().any(|declaration| match declaration.visibility() {
                VisibilityMode::Public => true,
                VisibilityMode::Package => candidate
                    .name
                    .namespace
                    .clone()
                    .strip_prefix(self.source.namespace())
                    .is_some(),
                _ => false,
            })
    }

    fn rank(&self, candidate: &Candidate) -> (u8, bool) {
        let level = if candidate.unit.id() != self.source.id() {
            0
        } else if candidate.name.namespace == *self.source.namespace() {
            2
        } else {
            1
        };
        let tagged = candidate
            .declarations()
            .any(|declaration| declaration.tag() == self.tag);
        (level, tagged)
    }
}

impl<'l> QualifiedLookup for &'l ScopeLookup<'_, '_> {
    type Output = Vec<Candidate<'l>>;

    fn lookup_exact(
        &self,
        _lexical_namespace: &NamespacePath,
        name: &QualifiedName,
    ) -> Option<Self::Output> {
        let this: &'l ScopeLookup = self;
        if this.tag.is_none() {
            let ordinary = this.candidates(name, HMIRLookupClass::Ordinary);
            if !ordinary.is_empty() {
                return Some(ordinary);
            }
        }
        let tagged = this.candidates(name, HMIRLookupClass::Tag);
        (!tagged.is_empty()).then_some(tagged)
    }

    fn resolve_aliases(
        &self,
        _lexical_namespace: &NamespacePath,
        namespace: &NamespacePath,
    ) -> Vec<NamespacePath> {
        self.source.symbols().aliases(namespace).to_vec()
    }

    fn priority(
        &self,
        _lexical_namespace: &NamespacePath,
        _name: &QualifiedName,
        candidates: &Self::Output,
    ) -> (u8, bool) {
        candidates[0].rank
    }
}
