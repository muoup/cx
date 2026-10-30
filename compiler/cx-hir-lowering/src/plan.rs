use rustc_hash::FxHashMap;

use cx_hir::ast::{
    HIR, HIRStmt,
    expression::HIRExpression,
    function::{HIRFunctionBody, HIRFunctionKind, HIRFunctionPrototype},
    global_var::{HIREnumVariant, HIRGlobalVariable},
    modifiers::{HIRSymbolNameScheme, LinkageMode, VisibilityMode},
    types::{HIRTagKind, HIRType, HIRTypeKind, HIRTypeLookup},
};
use cx_hmir::{
    HMIRDeclaration, HMIRDeclarationRole, HMIRDefKind, HMIRUnitID,
    unit::{def::HMIRDeclarationPoint, symbols::HMIRLookupClass},
};
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

pub(crate) enum DefSource<'h> {
    Type {
        ty: &'h HIRType,
    },
    Function {
        prototype: &'h HIRFunctionPrototype,
        body: Option<&'h HIRFunctionBody>,
    },
    Global {
        comptime: bool,
        ty: &'h HIRType,
        initializer: Option<&'h HIRExpression>,
        linkage: LinkageMode,
        naming: HIRSymbolNameScheme,
    },
    EnumVariant {
        variants: &'h [HIREnumVariant],
        index: usize,
    },
}

pub(crate) struct PlannedDeclaration<'h> {
    point: HMIRDeclarationPoint,
    ordered: bool,
    namespace: NamespacePath,
    span: TokenRange,
    role: HMIRDeclarationRole,
    tag: Option<HIRTagKind>,
    visibility: VisibilityMode,
    naming: Option<HIRSymbolNameScheme>,
    source: Option<DefSource<'h>>,
}

pub(crate) struct PlannedSymbol<'h> {
    name: QualifiedName,
    class: HMIRLookupClass,
    declarations: Vec<PlannedDeclaration<'h>>,
}

impl<'h> PlannedDeclaration<'h> {
    pub(crate) fn point(&self) -> HMIRDeclarationPoint {
        self.point
    }
    pub(crate) fn ordered(&self) -> bool {
        self.ordered
    }

    pub(crate) fn namespace(&self) -> &NamespacePath {
        &self.namespace
    }

    pub(crate) fn span(&self) -> &TokenRange {
        &self.span
    }

    pub(crate) fn tag(&self) -> Option<HIRTagKind> {
        self.tag
    }

    pub(crate) fn source(&self) -> Option<&DefSource<'h>> {
        self.source.as_ref()
    }

    pub(crate) fn lowered(&self, kind: Option<HMIRDefKind>) -> HMIRDeclaration {
        HMIRDeclaration::new(
            self.point,
            self.span.clone(),
            self.role,
            self.tag,
            self.visibility,
            self.naming,
            kind,
        )
    }
}

impl<'h> PlannedSymbol<'h> {
    pub(crate) fn name(&self) -> &QualifiedName {
        &self.name
    }

    pub(crate) fn class(&self) -> HMIRLookupClass {
        self.class
    }

    pub(crate) fn declarations(&self) -> &[PlannedDeclaration<'h>] {
        &self.declarations
    }
}

fn function_name(base: &NamespacePath, kind: &HIRFunctionKind) -> QualifiedName {
    let key = kind.into_key();
    QualifiedName::new(base.clone().join(key.namespace), key.name)
}

fn is_forward_declaration(name: &CXIdent, tag: Option<HIRTagKind>, ty: &HIRType) -> bool {
    matches!(
        &ty.kind,
        HIRTypeKind::Identifier {
            name: referenced,
            lookup: HIRTypeLookup::Tag(referenced_tag),
            args: None,
        } if referenced.name == *name && tag == Some(*referenced_tag)
    )
}

pub(crate) fn plan_symbols<'h>(
    hir: &'h HIR,
    namespace: &NamespacePath,
    unit: HMIRUnitID,
) -> Vec<PlannedSymbol<'h>> {
    let mut symbols = Vec::<PlannedSymbol>::new();
    let mut indices = FxHashMap::default();
    for (ordinal, definition) in hir.definition_stmts.iter().enumerate() {
        let base = if definition.namespace.is_root() {
            &definition.namespace
        } else {
            namespace
        };
        let (HIRStmt::TypeDefinition { visibility, .. }
        | HIRStmt::FunctionDefinition { visibility, .. }
        | HIRStmt::GlobalVariableDefinition { visibility, .. }) = &definition.stmt;
        let mut declare = |name: QualifiedName,
                           tag: Option<HIRTagKind>,
                           naming: Option<HIRSymbolNameScheme>,
                           span: &TokenRange,
                           role: HMIRDeclarationRole,
                           source: Option<DefSource<'h>>| {
            let class = HMIRLookupClass::from(tag);
            let index = *indices.entry((name.clone(), class)).or_insert_with(|| {
                symbols.push(PlannedSymbol {
                    name,
                    class,
                    declarations: Vec::new(),
                });
                symbols.len() - 1
            });
            symbols[index].declarations.push(PlannedDeclaration {
                point: HMIRDeclarationPoint::new(unit, ordinal),
                ordered: definition.ordered,
                namespace: definition.namespace.clone(),
                span: span.clone(),
                role,
                tag,
                visibility: *visibility,
                naming,
                source,
            });
        };

        match &definition.stmt {
            HIRStmt::TypeDefinition {
                name: Some(name),
                ty,
                tag,
                ..
            } => {
                let (role, source) = if is_forward_declaration(name, *tag, ty) {
                    (HMIRDeclarationRole::Declaration, None)
                } else {
                    (
                        HMIRDeclarationRole::Definition,
                        Some(DefSource::Type { ty }),
                    )
                };
                declare(
                    QualifiedName::new(base.clone(), name.clone()),
                    *tag,
                    None,
                    &ty.range,
                    role,
                    source,
                )
            }
            HIRStmt::TypeDefinition { name: None, .. } => {}

            HIRStmt::FunctionDefinition {
                prototype, body, ..
            } => declare(
                function_name(base, &prototype.kind),
                None,
                Some(prototype.symbol_naming),
                &prototype.range,
                match body {
                    Some(_) => HMIRDeclarationRole::Definition,
                    None => HMIRDeclarationRole::Declaration,
                },
                Some(DefSource::Function {
                    prototype,
                    body: body.as_ref(),
                }),
            ),

            HIRStmt::GlobalVariableDefinition { variable, .. } => match variable {
                HIRGlobalVariable::Standard {
                    comptime,
                    name,
                    ty,
                    initializer,
                    linkage,
                    symbol_name_scheme,
                    ..
                } => declare(
                    QualifiedName::new(base.clone(), name.clone()),
                    None,
                    Some(*symbol_name_scheme),
                    &ty.range,
                    match (initializer, linkage) {
                        (Some(_), _) => HMIRDeclarationRole::Definition,
                        (None, LinkageMode::Extern) => HMIRDeclarationRole::Declaration,
                        (None, _) => HMIRDeclarationRole::Tentative,
                    },
                    Some(DefSource::Global {
                        comptime: *comptime,
                        ty,
                        initializer: initializer.as_ref(),
                        linkage: *linkage,
                        naming: *symbol_name_scheme,
                    }),
                ),
                HIRGlobalVariable::EnumDefinition(enumeration) => {
                    for (index, variant) in enumeration.variants.iter().enumerate() {
                        declare(
                            QualifiedName::new(base.clone(), variant.name.clone()),
                            None,
                            None,
                            &TokenRange::internal(),
                            HMIRDeclarationRole::Definition,
                            Some(DefSource::EnumVariant {
                                variants: &enumeration.variants,
                                index,
                            }),
                        );
                    }
                }
            },
        }
    }
    symbols
}
