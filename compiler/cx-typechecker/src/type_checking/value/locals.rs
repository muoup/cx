use crate::{
    environment::TypeEnvironment,
    symbol::completion::{complete_type, assert_valid_type_component},
    type_checking::{initializer::typecheck_object_initializer, result::TypecheckResult},
};
use cx_hir::ast::{
    expression::HIRExpression,
    modifiers::{HIR_CONST, LinkageMode},
    types::HIRType,
};
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_namespace::module::NamespacePath;
use cx_namespace::module::QualifiedName;
use cx_thir::{
    thir::{
        expression::{THIRExpression, THIRExpressionKind, THIRLocalID},
        global::THIRGlobalVariable,
        r#type::{THIRArrayLength, THIRTypeKind},
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

pub(crate) fn typecheck_var_declaration(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    ty: &HIRType,
    name: &CXIdent,
    initial_value: Option<&HIRExpression>,
    linkage: LinkageMode,
) -> CXResult<TypecheckResult> {
    let ty = complete_type(env, namespace, ty)?;

    if !matches!(
        ty.kind,
        THIRTypeKind::Array {
            length: THIRArrayLength::Implicit,
            ..
        }
    ) {
        assert_valid_type_component(env, expr.token_range(), &ty, "a variable", true)?;
    }
    if initial_value.is_none()
        && linkage != LinkageMode::Extern
        && matches!(
            ty.kind,
            THIRTypeKind::Array {
                length: THIRArrayLength::Implicit,
                ..
            }
        )
    {
        return env.log_error(
            expr.token_range(),
            &catalogue::INCOMPLETE_TYPE,
            format!("{}", name),
        );
    }

    let expr = match linkage {
        LinkageMode::Extern => {
            let mem_type = env.symbols.mem_ref_to(ty.clone());
            let symbol_name = QualifiedName::new_raw(name.clone());

            let sym_expr = if let Some(symbol) = env.get_symbol(namespace, &symbol_name)? {
                let sym_expr = symbol
                    .as_expression()
                    .map_err(|err| env.complete_err(err, expr.token_range()))?;

                if !env.type_eq(&mem_type, &sym_expr.ty) {
                    return env.log_error(
                        expr.token_range(),
                        &catalogue::VARIABLE_REDECLARATION,
                        format!("{}", name),
                    );
                }

                sym_expr
            } else {
                env.items.push_generated_global(
                    THIRGlobalVariable::new(
                        name.clone(),
                        ty.clone(),
                        None,
                        LinkageMode::Extern,
                        true,
                    ),
                    false,
                );

                THIRExpression {
                    token_range: expr.token_range().clone(),
                    kind: THIRExpressionKind::GlobalVariable {
                        symbol: name.clone(),
                    },
                    ty: mem_type,
                }
            };

            env.symbols
                .insert_local_value(QualifiedName::new_raw(name.clone()), sym_expr.clone());
            sym_expr
        }

        LinkageMode::Static => {
            let function_name = env.current_function().symbol_name();
            let symbol_name = format!("_S{}_{}_{}", name.as_str().len(), name, function_name);
            let (global_type, initializer) = match initial_value {
                Some(initial_value) => {
                    let checked = typecheck_object_initializer(env, namespace, initial_value, &ty)?;
                    (checked.object_type, Some(checked.value))
                }
                None => (ty.clone(), None),
            };
            assert_valid_type_component(env, expr.token_range(), &global_type, "a variable", true)?;
            let is_const = ty.get_specifier(HIR_CONST) || {
                let mut element_type = env.symbols.array_inner(&global_type);
                let mut is_const = false;
                while let Some(element) = element_type {
                    if element.get_specifier(HIR_CONST) {
                        is_const = true;
                        break;
                    }
                    element_type = env.symbols.array_inner(element);
                }
                is_const
            };

            env.items.push_generated_global(
                THIRGlobalVariable::new(
                    CXIdent::new(symbol_name.clone()),
                    global_type.clone(),
                    initializer,
                    LinkageMode::Static,
                    !is_const,
                ),
                false,
            );

            let symbol = THIRExpression {
                token_range: expr.token_range().clone(),
                ty: env.symbols.mem_ref_to(global_type),
                kind: THIRExpressionKind::GlobalVariable {
                    symbol: CXIdent::new(symbol_name),
                },
            };

            env.symbols
                .insert_local_value(QualifiedName::new_raw(name.clone()), symbol.clone());

            symbol
        }

        LinkageMode::Standard => {
            let local_id = THIRLocalID::fresh();

            let (object_type, initial_value, adopting) = match initial_value {
                Some(init_expr) => {
                    let checked = typecheck_object_initializer(env, namespace, init_expr, &ty)?;
                    (
                        checked.object_type,
                        Some(Box::new(checked.value)),
                        checked.adopting,
                    )
                }
                None => (ty.clone(), None, false),
            };
            assert_valid_type_component(env, expr.token_range(), &object_type, "a variable", true)?;
            let mem_type = env.symbols.mem_ref_to(object_type.clone());

            let binding = THIRExpression {
                token_range: TokenRange::internal(),
                kind: match adopting {
                    true => THIRExpressionKind::AdoptRegion {
                        binding_name: name.clone(),
                        local_id,
                        ty: object_type.clone(),
                        initial_value: initial_value
                            .expect("adopting binding must have an initial value"),
                    },
                    false => THIRExpressionKind::CreateLocalVariable {
                        name: name.clone(),
                        local_id,
                        ty: object_type.clone(),
                        initial_value,
                    },
                },
                ty: mem_type.clone(),
            };

            env.symbols.insert_local_value(
                QualifiedName::new_raw(name.clone()),
                THIRExpression {
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::Variable {
                        name: name.clone(),
                        local_id,
                    },
                    ty: mem_type,
                },
            );

            binding
        }
    };

    Ok(TypecheckResult::from(expr))
}
