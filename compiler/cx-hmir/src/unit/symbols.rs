use std::collections::HashMap;

use cx_hir::{
    ast::{
        modifiers::{HIRSymbolNameScheme, VisibilityMode},
        types::HIRTagKind,
    },
    registry::ExportNameMode,
};
use cx_namespace::module::{NamespacePath, QualifiedName};

use crate::HMIRDefID;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRLookupClass {
    Ordinary,
    Tag,
}

#[derive(Debug, Clone)]
pub struct HMIRDeclaration {
    name: QualifiedName,
    tag: Option<HIRTagKind>,
    visibility: VisibilityMode,
    naming: Option<HIRSymbolNameScheme>,
}

#[derive(Debug, Clone, Default)]
pub struct HMIRSymbolTable {
    imports: Vec<NamespacePath>,
    aliases: HashMap<NamespacePath, Vec<NamespacePath>>,
    export_mode: ExportNameMode,

    declarations: HashMap<HMIRDefID, Vec<HMIRDeclaration>>,
    groups: HashMap<(QualifiedName, HMIRLookupClass), Vec<HMIRDefID>>,
}

impl From<Option<HIRTagKind>> for HMIRLookupClass {
    fn from(other: Option<HIRTagKind>) -> Self {
        match other {
            Some(_) => Self::Tag,
            None => Self::Ordinary,
        }
    }
}

impl HMIRDeclaration {
    pub fn new(
        name: QualifiedName,
        tag: Option<HIRTagKind>,
        visibility: VisibilityMode,
        naming: Option<HIRSymbolNameScheme>,
    ) -> Self {
        Self {
            name,
            tag,
            visibility,
            naming,
        }
    }

    pub fn name(&self) -> &QualifiedName {
        &self.name
    }

    pub fn tag(&self) -> Option<HIRTagKind> {
        self.tag
    }

    pub fn class(&self) -> HMIRLookupClass {
        HMIRLookupClass::from(self.tag)
    }

    pub fn visibility(&self) -> VisibilityMode {
        self.visibility
    }

    pub fn naming(&self) -> Option<HIRSymbolNameScheme> {
        self.naming
    }
}

impl HMIRSymbolTable {
    pub fn new(
        imports: Vec<NamespacePath>,
        aliases: HashMap<NamespacePath, Vec<NamespacePath>>,
        export_mode: ExportNameMode,
        declarations: Vec<(HMIRDefID, HMIRDeclaration)>,
    ) -> Self {
        let mut groups = HashMap::<_, Vec<_>>::new();
        let mut by_def = HashMap::<_, Vec<_>>::new();
        for (id, declaration) in declarations {
            let group = groups
                .entry((declaration.name.clone(), declaration.class()))
                .or_default();
            if !group.contains(&id) {
                group.push(id);
            }
            by_def.entry(id).or_default().push(declaration);
        }

        Self {
            imports,
            aliases,
            export_mode,
            declarations: by_def,
            groups,
        }
    }

    pub fn imports(&self) -> &[NamespacePath] {
        &self.imports
    }

    pub fn aliases(&self, prefix: &NamespacePath) -> &[NamespacePath] {
        self.aliases.get(prefix).map_or(&[], Vec::as_slice)
    }

    pub fn export_mode(&self) -> ExportNameMode {
        self.export_mode
    }

    pub fn declarations(&self, id: HMIRDefID) -> &[HMIRDeclaration] {
        self.declarations.get(&id).map_or(&[], Vec::as_slice)
    }

    pub fn group(&self, name: &QualifiedName, class: HMIRLookupClass) -> &[HMIRDefID] {
        self.groups
            .get(&(name.clone(), class))
            .map_or(&[], Vec::as_slice)
    }

    pub fn groups(&self) -> impl Iterator<Item = (&QualifiedName, HMIRLookupClass, &[HMIRDefID])> {
        self.groups
            .iter()
            .map(|((name, class), defs)| (name, *class, defs.as_slice()))
    }
}
