use crate::{
    environment::TypeEnvironment,
    symbol::{
        completion::{complete_type, ensure_valid_type_component},
        name_mangling::mangle_static_symbol,
    },
    type_checking::{
        coercion::implicit::implicit_cast, result::TypecheckResult, typechecker::typecheck_expr,
    },
};
use cx_hir::ast::{
    expression::HIRExpression,
    modifiers::{HIR_CONST, LinkageMode},
    types::HIRType,
};
use cx_log::CXResult;
use cx_thir::{
    NamespacePath,
    thir::{
        expression::{THIRExpression, THIRExpressionKind, THIRLocalID},
        global::THIRGlobalVariable,
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, module::QualifiedName};

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
    let mem_type = env.symbols.mem_ref_to(ty.clone());

    ensure_valid_type_component(env, expr.token_range(), &ty, "a variable", true)?;

    let expr = match linkage {
        LinkageMode::Extern => {
            let symbol_name = QualifiedName::new_raw(name.clone());

            let sym_expr = if let Some(symbol) = env.get_symbol(namespace, &symbol_name)? {
                let sym_expr = symbol
                    .as_expression()
                    .map_err(|err| env.complete_err(err, expr.token_range()))?;

                if !env.type_eq(&mem_type, &sym_expr._type) {
                    return env.log_error(
                        expr.token_range(),
                        format!(
                            "Attempting to redeclare variable '{}' with a different type.",
                            name
                        ),
                    );
                }

                sym_expr
            } else {
                env.items.push_generated_global(
                    THIRGlobalVariable {
                        name: name.clone(),
                        _type: ty.clone(),

                        is_mutable: true,
                        linkage: LinkageMode::Extern,
                        initializer: None,
                    },
                    false,
                );

                THIRExpression {
                    token_range: expr.token_range().clone(),
                    kind: THIRExpressionKind::GlobalVariable {
                        symbol: name.clone(),
                    },
                    _type: mem_type,
                }
            };

            env.symbols
                .insert_local_value(QualifiedName::new_raw(name.clone()), sym_expr.clone());
            sym_expr
        }

        LinkageMode::Static => {
            let symbol_name =
                mangle_static_symbol(name.as_str(), env.current_function().symbol_name());
            let (global_type, initializer) = match initial_value {
                Some(initial_value) => {
                    let init_tc = typecheck_expr(env, namespace, initial_value, Some(&ty))?;
                    let init_expr = init_tc
                        .standard_ready_coerce(env, expr.token_range())
                        .and_then(|value| implicit_cast(env, value, &ty))?;
                    let global_type = match &init_expr.kind {
                        THIRExpressionKind::ArrayInitializer { .. } => init_expr._type.clone(),
                        THIRExpressionKind::TypeConversion {
                            conversion: cx_thir::thir::expression::THIRCoercion::ReinterpretBits,
                            operand,
                        } if matches!(
                            operand.kind,
                            THIRExpressionKind::ArrayInitializer { .. }
                        ) =>
                        {
                            operand._type.clone()
                        }
                        _ => ty.clone(),
                    };
                    (global_type, Some(init_expr))
                }
                None => (ty.clone(), None),
            };
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
                THIRGlobalVariable {
                    name: CXIdent::new(symbol_name.clone()),
                    _type: global_type.clone(),

                    is_mutable: !is_const,
                    linkage: LinkageMode::Static,
                    initializer,
                },
                false,
            );

            let symbol = THIRExpression {
                token_range: expr.token_range().clone(),
                _type: env.symbols.mem_ref_to(global_type),
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

            let (initial_value, adopting) = match initial_value {
                Some(init_expr) => {
                    let init_tc = typecheck_expr(env, namespace, init_expr, Some(&ty))?;
                    let adopting = init_tc.is_adopting();
                    let init_expr = init_tc
                        .standard_ready_coerce(env, expr.token_range())
                        .and_then(|v| implicit_cast(env, v, &ty))?;
                    (Some(Box::new(init_expr)), adopting)
                }
                None => (None, false),
            };

            let binding = THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::CreateLocalVariable {
                    name: name.clone(),
                    local_id,
                    _type: ty.clone(),
                    initial_value,
                    adopting,
                },
                _type: mem_type.clone(),
            };

            env.symbols.insert_local_value(
                QualifiedName::new_raw(name.clone()),
                THIRExpression {
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::Variable {
                        name: name.clone(),
                        local_id,
                    },
                    _type: mem_type,
                },
            );

            binding
        }
    };

    Ok(TypecheckResult::from(expr))
}
