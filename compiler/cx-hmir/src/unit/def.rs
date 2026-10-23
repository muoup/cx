use cx_hir::ast::{
    modifiers::{HIRSymbolNameScheme, VisibilityMode},
    types::HIRTagKind,
};
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::branded_id;

use crate::{
    ty::HMIRTypeID,
    unit::{
        function::HMIRFunction,
        global::{HMIRComptimeGlobal, HMIRGlobal},
        id::HMIRUnitID,
    },
};

branded_id!(HMIRDefID, HMIRUnitID, "def");

#[derive(Debug, Clone)]
pub struct HMIRDef {
    name: QualifiedName,
    declarations: Vec<HMIRDeclaration>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum HMIRDeclarationRole {
    Declaration,
    Tentative,
    Definition,
}

#[derive(Debug, Clone)]
pub struct HMIRDeclaration {
    span: TokenRange,
    role: HMIRDeclarationRole,
    tag: Option<HIRTagKind>,
    visibility: VisibilityMode,
    naming: Option<HIRSymbolNameScheme>,
    kind: Option<HMIRDefKind>,
}

#[derive(Debug, Clone)]
pub enum HMIRDefKind {
    Function(Box<HMIRFunction>),
    Global(Box<HMIRGlobal>),
    ComptimeGlobal(Box<HMIRComptimeGlobal>),
    TypeDef(Box<HMIRComptimeGlobal>),
    Type(HMIRTypeID),
}

impl HMIRDef {
    pub fn new(name: QualifiedName, declarations: Vec<HMIRDeclaration>) -> Self {
        Self { name, declarations }
    }

    pub fn name(&self) -> &QualifiedName {
        &self.name
    }

    pub fn declarations(&self) -> &[HMIRDeclaration] {
        &self.declarations
    }
}

impl HMIRDeclaration {
    pub fn new(
        span: TokenRange,
        role: HMIRDeclarationRole,
        tag: Option<HIRTagKind>,
        visibility: VisibilityMode,
        naming: Option<HIRSymbolNameScheme>,
        kind: Option<HMIRDefKind>,
    ) -> Self {
        Self {
            span,
            role,
            tag,
            visibility,
            naming,
            kind,
        }
    }

    pub fn span(&self) -> &TokenRange {
        &self.span
    }

    pub fn role(&self) -> HMIRDeclarationRole {
        self.role
    }

    pub fn tag(&self) -> Option<HIRTagKind> {
        self.tag
    }

    pub fn visibility(&self) -> VisibilityMode {
        self.visibility
    }

    pub fn naming(&self) -> Option<HIRSymbolNameScheme> {
        self.naming
    }

    pub fn kind(&self) -> Option<&HMIRDefKind> {
        self.kind.as_ref()
    }
}
