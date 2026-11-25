use rustc_hash::FxHashMap;

use chl_hir::ast::{
    HIR, HIRStmt,
    expression::HIRExpression,
    function::{HIRFunctionBody, HIRFunctionKind, HIRFunctionPrototype},
    global_var::{HIREnumDefinition, HIRGlobalVariable},
    modifiers::{HIRSymbolNameScheme, LinkageMode, VisibilityMode},
    types::{HIRTagKind, HIRType, HIRTypeKind, HIRTypeLookup},
};
use chl_hmir::{
    HMIRDeclaration, HMIRDeclarationRole, HMIRDefKind, HMIRUnitID,
    unit::{def::HMIRDeclarationPoint, symbols::HMIRLookupClass},
};
use chl_log::{CHLResult, catalogue::typecheck};
use chl_namespace::module::{NamespacePath, QualifiedName};
use chl_tokens::TokenRange;
use chl_util::identifier::CHLIdent;

use crate::log::log_hir_error;

pub(crate) enum DefSource<'h> {
    Type {
        ty: &'h HIRType,
    },
    EnumType {
        underlying: Option<&'h HIRType>,
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
        explicit_extern: bool,
        naming: HIRSymbolNameScheme,
    },
    EnumVariant {
        enumeration: &'h HIREnumDefinition,
        index: usize,
        scoped_type: Option<QualifiedName>,
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
    overwrite: bool,
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
            self.overwrite,
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

fn is_forward_declaration(name: &CHLIdent, tag: Option<HIRTagKind>, ty: &HIRType) -> bool {
    matches!(
        &ty.kind,
        HIRTypeKind::Identifier {
            name: referenced,
            lookup: HIRTypeLookup::Tag(referenced_tag),
            args: None,
        } if referenced.name == *name && tag == Some(*referenced_tag)
    )
}

fn enum_is_scoped(enumeration: &HIREnumDefinition) -> CHLResult<bool> {
    let Some(base) = &enumeration.underlying else {
        return Ok(false);
    };
    let (representation, scoped) = match &base.kind {
        HIRTypeKind::Strong(representation) => (representation.as_ref(), true),
        _ => (base, false),
    };
    let is_int = base.specifiers == 0
        && representation.specifiers == 0
        && matches!(
            &representation.kind,
            HIRTypeKind::Identifier { name, lookup: HIRTypeLookup::Standard, args: None }
                if name.root_name_ref().is_some_and(|name| name.as_str() == "int")
        );
    if !is_int {
        return log_hir_error(
            &base.range,
            &typecheck::TYPE_REQUIREMENT,
            (
                "an enum base".into(),
                "`int` or `@strong int`".into(),
                Some(base.to_string()),
            ),
        );
    }
    if scoped && enumeration.name.is_none() {
        return log_hir_error(
            &enumeration.range,
            &typecheck::INVALID_CONTEXT,
            ("`@strong int`".into(), "an anonymous enum".into()),
        );
    }
    Ok(scoped)
}

pub(crate) fn plan_symbols<'h>(
    hir: &'h HIR,
    namespace: &NamespacePath,
    unit: HMIRUnitID,
) -> CHLResult<Vec<PlannedSymbol<'h>>> {
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
                overwrite: definition.overwrite,
            });
        };

        match &definition.stmt {
            HIRStmt::TypeDefinition {
                name: Some(name),
                ty,
                tag,
                range,
                symbol_naming,
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
                    Some(*symbol_naming),
                    range,
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
                    explicit_extern,
                    symbol_name_scheme,
                    range,
                    ..
                } => declare(
                    QualifiedName::new(base.clone(), name.clone()),
                    None,
                    Some(*symbol_name_scheme),
                    range,
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
                        explicit_extern: *explicit_extern,
                        naming: *symbol_name_scheme,
                    }),
                ),
                HIRGlobalVariable::EnumDefinition(enumeration) => {
                    let name = enumeration
                        .name
                        .as_ref()
                        .map(|name| QualifiedName::new(base.clone(), name.clone()));
                    let scoped_type = if enum_is_scoped(enumeration)? {
                        name.clone()
                    } else {
                        None
                    };
                    let members = match &scoped_type {
                        Some(ty) => base.clone().child(ty.name.clone()),
                        None => base.clone(),
                    };
                    for (index, variant) in enumeration.variants.iter().enumerate() {
                        declare(
                            QualifiedName::new(members.clone(), variant.name.clone()),
                            None,
                            Some(enumeration.symbol_naming),
                            &variant.range,
                            HMIRDeclarationRole::Definition,
                            Some(DefSource::EnumVariant {
                                enumeration,
                                index,
                                scoped_type: scoped_type.clone(),
                            }),
                        );
                    }
                    if let Some(name) = name {
                        declare(
                            name,
                            Some(HIRTagKind::Enum),
                            Some(enumeration.symbol_naming),
                            &enumeration.range,
                            HMIRDeclarationRole::Definition,
                            Some(DefSource::EnumType {
                                underlying: enumeration.underlying.as_ref(),
                            }),
                        );
                    }
                }
            },
        }
    }
    Ok(symbols)
}
