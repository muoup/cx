use std::collections::HashSet;

use cx_hir::ast::{
    HIR, HIRStmt,
    expression::HIRExpression,
    function::{HIRComptimeFnPrototype, HIRFunctionBody, HIRFunctionKind, HIRFunctionPrototype},
    global_var::{HIREnumVariant, HIRGlobalVariable},
    modifiers::{HIRSymbolNameScheme, LinkageMode},
    template::HIRTemplatePrototype,
    types::{HIRTagKind, HIRType, HIRTypeKind, HIRTypeLookup},
};
use cx_util::identifier::CXIdent;
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_tokens::TokenRange;

use crate::resolve::def_name;

pub(crate) enum DefSource<'h> {
    OpaqueType,
    Type {
        template: Option<&'h HIRTemplatePrototype>,
        ty: &'h HIRType,
    },
    Function {
        prototype: &'h HIRFunctionPrototype,
        template: Option<&'h HIRTemplatePrototype>,
        body: Option<&'h HIRFunctionBody>,
    },
    ComptimeFunction {
        prototype: &'h HIRComptimeFnPrototype,
        template: Option<&'h HIRTemplatePrototype>,
        body: &'h HIRFunctionBody,
    },
    Global {
        ty: &'h HIRType,
        mutable: bool,
        initializer: Option<&'h HIRExpression>,
        linkage: LinkageMode,
        naming: HIRSymbolNameScheme,
    },
    EnumVariant {
        variants: &'h [HIREnumVariant],
        index: usize,
    },
    Constructor {
        template: Option<&'h HIRTemplatePrototype>,
        union_type: &'h HIRType,
        payload: HIRType,
    },
}

pub(crate) struct PlannedDef<'h> {
    name: QualifiedName,
    namespace: NamespacePath,
    span: TokenRange,
    source: DefSource<'h>,
}

impl<'h> PlannedDef<'h> {
    pub(crate) fn new(
        name: QualifiedName,
        namespace: NamespacePath,
        span: TokenRange,
        source: DefSource<'h>,
    ) -> Self {
        Self {
            name,
            namespace,
            span,
            source,
        }
    }

    pub(crate) fn name(&self) -> &QualifiedName {
        &self.name
    }

    pub(crate) fn namespace(&self) -> &NamespacePath {
        &self.namespace
    }

    pub(crate) fn span(&self) -> &TokenRange {
        &self.span
    }

    pub(crate) fn source(&self) -> &DefSource<'h> {
        &self.source
    }
}

fn function_name(base: &NamespacePath, kind: &HIRFunctionKind) -> QualifiedName {
    let key = kind.into_key();
    QualifiedName::new(base.clone().join(key.namespace), key.name)
}

pub(crate) fn is_forward_declaration(name: &CXIdent, tag: Option<HIRTagKind>, ty: &HIRType) -> bool {
    matches!(
        &ty.kind,
        HIRTypeKind::Identifier {
            name: referenced,
            lookup: HIRTypeLookup::Tag(referenced_tag),
            template_input: None,
        } if referenced.name == *name && tag == Some(*referenced_tag)
    )
}

pub(crate) fn plan_defs<'h>(hir: &'h HIR, namespace: &NamespacePath) -> Vec<PlannedDef<'h>> {
    let defined_types = hir
        .definition_stmts
        .iter()
        .filter_map(|definition| match &definition.stmt {
            HIRStmt::TypeDefinition {
                name: Some(name),
                ty,
                tag,
                ..
            } if !is_forward_declaration(name, *tag, ty) => Some((name.clone(), *tag)),
            _ => None,
        })
        .collect::<HashSet<_>>();

    let defined_functions = hir
        .definition_stmts
        .iter()
        .filter_map(|definition| match &definition.stmt {
            HIRStmt::FunctionDefinition {
                prototype,
                body: Some(_),
                ..
            } => Some(prototype.kind.into_key()),
            _ => None,
        })
        .collect::<HashSet<_>>();

    let mut plans = Vec::new();
    for definition in &hir.definition_stmts {
        let base = if definition.namespace.is_root() {
            &definition.namespace
        } else {
            namespace
        };
        let mut plan = |name: QualifiedName, span: &TokenRange, source: DefSource<'h>| {
            plans.push(PlannedDef {
                name,
                namespace: definition.namespace.clone(),
                span: span.clone(),
                source,
            })
        };

        match &definition.stmt {
            HIRStmt::TypeDefinition {
                name: Some(name),
                ty,
                tag,
                ..
            } if is_forward_declaration(name, *tag, ty) => {
                if !defined_types.contains(&(name.clone(), *tag)) {
                    plan(
                        def_name(QualifiedName::new(base.clone(), name.clone()), *tag),
                        &ty.range,
                        DefSource::OpaqueType,
                    );
                }
            }
            HIRStmt::TypeDefinition {
                name: Some(name),
                template_prototype,
                ty,
                tag,
                ..
            } => plan(
                def_name(QualifiedName::new(base.clone(), name.clone()), *tag),
                &ty.range,
                DefSource::Type {
                    template: template_prototype.as_ref(),
                    ty,
                },
            ),
            HIRStmt::TypeDefinition { name: None, .. } => {}

            HIRStmt::FunctionDefinition {
                prototype,
                template_prototype,
                body,
                ..
            } => {
                let declaration_only = body.is_none()
                    && (template_prototype.is_some()
                        || defined_functions.contains(&prototype.kind.into_key()));
                if !declaration_only {
                    plan(
                        function_name(base, &prototype.kind),
                        &prototype.range,
                        DefSource::Function {
                            prototype,
                            template: template_prototype.as_ref(),
                            body: body.as_ref(),
                        },
                    );
                }
            }

            HIRStmt::ComptimeFunctionDefinition {
                prototype,
                template_prototype,
                body,
                ..
            } => plan(
                function_name(base, &prototype.kind),
                &prototype.range,
                DefSource::ComptimeFunction {
                    prototype,
                    template: template_prototype.as_ref(),
                    body,
                },
            ),

            HIRStmt::GlobalVariableDefinition { variable, .. } => match variable {
                HIRGlobalVariable::Standard {
                    name,
                    ty,
                    is_mutable,
                    initializer,
                    linkage,
                    symbol_name_scheme,
                } => plan(
                    QualifiedName::new(base.clone(), name.clone()),
                    &ty.range,
                    DefSource::Global {
                        ty,
                        mutable: *is_mutable,
                        initializer: initializer.as_ref(),
                        linkage: *linkage,
                        naming: *symbol_name_scheme,
                    },
                ),
                HIRGlobalVariable::EnumDefinition(enumeration) => {
                    for (index, variant) in enumeration.variants.iter().enumerate() {
                        plan(
                            QualifiedName::new(base.clone(), variant.name.clone()),
                            &TokenRange::internal(),
                            DefSource::EnumVariant {
                                variants: &enumeration.variants,
                                index,
                            },
                        );
                    }
                }
            },
        }
    }
    plans
}
