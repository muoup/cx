use cx_hir::ast::{modifiers::VisibilityMode, types::HIRTagKind};
use cx_hmir::{
    HMIRDefID, HMIRUnit,
    unit::symbols::{HMIRDeclaration, HMIRLookupClass},
};
use cx_namespace::{
    lookup::QualifiedLookup,
    module::{NamespacePath, QualifiedName},
};

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

pub(crate) struct ScopeLookup<'hmir> {
    pub(crate) units: Vec<HMIRUnitHandle<'hmir>>,
    pub(crate) tag: Option<HIRTagKind>,
}

pub(crate) struct Candidate<'l> {
    pub(crate) unit: &'l HMIRUnit,
    pub(crate) name: QualifiedName,
    pub(crate) class: HMIRLookupClass,
    pub(crate) defs: &'l [HMIRDefID],
}

impl HMIRSymbolGroup {
    pub(crate) fn new(name: QualifiedName, defs: Vec<HMIRDefID>) -> Self {
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

    pub(crate) fn incompatible_tag(&self, tag: Option<HIRTagKind>) -> bool {
        tag.is_some()
            && self
                .declarations()
                .any(|declaration| declaration.tag() != tag)
    }
}

impl ScopeLookup<'_> {
    fn owner(&self) -> &HMIRUnit {
        &self.units[0]
    }

    fn is_friend(&self, lexical_namespace: &NamespacePath, namespace: &NamespacePath) -> bool {
        lexical_namespace == self.owner().namespace()
            && self.owner().symbols().friends().contains(namespace)
    }

    fn unit_of(&self, namespace: &NamespacePath) -> Option<&HMIRUnit> {
        self.units
            .iter()
            .filter(|unit| namespace.clone().strip_prefix(unit.namespace()).is_some())
            .max_by_key(|unit| unit.namespace().segments().len())
            .map(|unit| &**unit)
    }

    fn visible(
        &self,
        lexical_namespace: &NamespacePath,
        candidate: &Candidate,
        name: &QualifiedName,
    ) -> bool {
        candidate.declarations().any(|declaration| {
            declaration.visibility() == VisibilityMode::Public
                || name.namespace == *lexical_namespace
                || self.is_friend(lexical_namespace, &name.namespace)
                || (declaration.visibility() == VisibilityMode::Package
                    && name
                        .namespace
                        .clone()
                        .strip_prefix(lexical_namespace)
                        .is_some())
        })
    }
}

impl<'l> QualifiedLookup for &'l ScopeLookup<'_> {
    type Output = Candidate<'l>;

    fn lookup_exact(
        &self,
        lexical_namespace: &NamespacePath,
        name: &QualifiedName,
    ) -> Option<Self::Output> {
        let this: &'l ScopeLookup = self;
        let unit = this.unit_of(&name.namespace)?;
        let group = |class| {
            let candidate = Candidate {
                unit,
                name: name.clone(),
                class,
                defs: unit.symbols().group(name, class),
            };
            (!candidate.defs.is_empty() && this.visible(lexical_namespace, &candidate, name))
                .then_some(candidate)
        };

        if self.tag.is_none()
            && let Some(candidate) = group(HMIRLookupClass::Ordinary)
        {
            return Some(candidate);
        }
        group(HMIRLookupClass::Tag)
    }

    fn resolve_aliases(
        &self,
        _lexical_namespace: &NamespacePath,
        namespace: &NamespacePath,
    ) -> Vec<NamespacePath> {
        self.owner().symbols().aliases(namespace).to_vec()
    }

    fn priority(
        &self,
        lexical_namespace: &NamespacePath,
        name: &QualifiedName,
        candidate: &Self::Output,
    ) -> (u8, bool) {
        (
            if name.namespace == *lexical_namespace {
                2
            } else {
                u8::from(self.is_friend(lexical_namespace, &name.namespace))
            },
            candidate
                .declarations()
                .any(|declaration| declaration.tag() == self.tag),
        )
    }
}
