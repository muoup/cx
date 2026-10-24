use std::{cell::RefCell, collections::HashMap};

use cx_hir::ast::{
    modifiers::{HIRSymbolNameScheme, VisibilityMode},
    types::HIRTagKind,
};
use cx_hmir::{
    HMIRDeclaration, HMIRDeclarationRole, HMIRDef, HMIRDefID, HMIRDefKind, HMIRUnitID,
    unit::{def::HMIRDeclarationPoint, symbols::HMIRLookupClass},
};
use cx_namespace::{mangling::mangle_namespace_symbol, module::QualifiedName};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::plan::PlannedSymbol;

pub(crate) struct UnitContext {
    unit: HMIRUnitID,
    root_exports: bool,
    // Function-level statics become defs placed after the planned ones
    planned: usize,
    externs: RefCell<HashMap<QualifiedName, (HMIRDefID, Vec<HMIRDeclaration>)>>,
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
    pub(crate) fn new(unit: HMIRUnitID, root_exports: bool, planned: &[PlannedSymbol<'_>]) -> Self {
        Self {
            unit,
            root_exports,
            planned: planned.len(),
            externs: RefCell::new(
                planned
                    .iter()
                    .enumerate()
                    .filter(|(_, symbol)| symbol.class() == HMIRLookupClass::Ordinary)
                    .map(|(index, symbol)| {
                        (
                            symbol.name().clone(),
                            (HMIRDefID::new(unit, index), Vec::new()),
                        )
                    })
                    .collect(),
            ),
            statics: RefCell::new(Vec::new()),
        }
    }

    pub(crate) fn push_extern(
        &self,
        name: QualifiedName,
        span: &TokenRange,
        kind: HMIRDefKind,
        point: Option<HMIRDeclarationPoint>,
    ) -> HMIRDefID {
        let next = self.next_static();
        let mut externs = self.externs.borrow_mut();
        let (id, declarations) = externs.entry(name.clone()).or_insert_with(|| {
            self.statics
                .borrow_mut()
                .push(HMIRDef::new(name, Vec::new()));
            (next, Vec::new())
        });
        let role = match &kind {
            HMIRDefKind::Global(global) if global.initializer().is_some() => {
                HMIRDeclarationRole::Definition
            }
            _ => HMIRDeclarationRole::Declaration,
        };
        declarations.push(HMIRDeclaration::new(
            point.unwrap_or_else(|| HMIRDeclarationPoint::new(self.unit, next.index())),
            span.clone(),
            role,
            None,
            VisibilityMode::Private,
            Some(HIRSymbolNameScheme::Unmangled),
            Some(kind),
        ));
        *id
    }

    pub(crate) fn take_externs(&self) -> impl Iterator<Item = (HMIRDefID, Vec<HMIRDeclaration>)> {
        self.externs
            .take()
            .into_values()
            .filter(|(_, declarations)| !declarations.is_empty())
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
