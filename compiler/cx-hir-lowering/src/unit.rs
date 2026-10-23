use std::cell::RefCell;

use cx_hir::ast::{
    modifiers::{HIRSymbolNameScheme, VisibilityMode},
    types::HIRTagKind,
};
use cx_hmir::{
    HMIRDeclaration, HMIRDeclarationRole, HMIRDef, HMIRDefID, HMIRDefKind, HMIRUnitID,
    unit::def::HMIRDeclarationPoint,
};
use cx_namespace::{mangling::mangle_namespace_symbol, module::QualifiedName};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

pub(crate) struct UnitContext {
    unit: HMIRUnitID,
    root_exports: bool,
    // Function-level statics become defs placed after the planned ones
    planned: usize,
    statics: RefCell<Vec<HMIRDef>>,
}

pub(crate) fn def_name(name: QualifiedName, tag: Option<HIRTagKind>) -> QualifiedName {
    match tag {
        Some(tag) => QualifiedName::new(
            name.namespace,
            CXIdent::from(format!("{} {}", tag.prefix(), name.name).as_str()),
        ),
        None => name,
    }
}

impl UnitContext {
    pub(crate) fn new(unit: HMIRUnitID, root_exports: bool, planned: usize) -> Self {
        Self {
            unit,
            root_exports,
            planned,
            statics: RefCell::new(Vec::new()),
        }
    }

    pub(crate) fn next_static(&self) -> HMIRDefID {
        HMIRDefID::new(self.unit, self.planned + self.statics.borrow().len())
    }

    pub(crate) fn push_static(
        &self,
        name: QualifiedName,
        span: &TokenRange,
        kind: HMIRDefKind,
        point: Option<HMIRDeclarationPoint>,
    ) {
        let point = point.unwrap_or_else(|| {
            HMIRDeclarationPoint::new(self.unit, self.planned + self.statics.borrow().len())
        });
        let declaration = HMIRDeclaration::new(
            point,
            span.clone(),
            HMIRDeclarationRole::Definition,
            None,
            VisibilityMode::Private,
            None,
            Some(kind),
        );
        self.statics
            .borrow_mut()
            .push(HMIRDef::new(name, vec![declaration]));
    }

    pub(crate) fn take_statics(&self) -> Vec<HMIRDef> {
        self.statics.take()
    }

    pub(crate) fn link_name(&self, name: &QualifiedName, scheme: HIRSymbolNameScheme) -> CXIdent {
        if scheme == HIRSymbolNameScheme::Unmangled || name.namespace.is_root() || self.root_exports
        {
            return name.name.clone();
        }
        CXIdent::new(mangle_namespace_symbol(name))
    }
}
