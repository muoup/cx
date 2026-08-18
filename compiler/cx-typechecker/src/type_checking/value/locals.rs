use crate::{
    environment::TypeEnvironment,
    symbol::{
        completion::complete_type,
        resolution::resolve_symbol_without_implicit_array_decay,
    },
    type_checking::{
        coercion::implicit::implicit_cast, result::TypecheckResult, typechecker::typecheck_expr,
        value::ensure_valid_allocation_type,
    },
};
use cx_hir::ast::{
    expression::HIRExpression,
    modifiers::LinkageMode,
    types::{HIRType, HIRTypeKind},
};
use cx_hir::symbols::HIRSymbolKind;
use cx_log::CXResult;
use cx_thir::{
    EnvironmentNamespace,
    symbol::MIRSymbol,
    thir::{
        data::THIRType,
        expression::{THIRExpression, THIRExpressionKind, THIRLocalID},
        global::{THIRGlobalVarKind, THIRGlobalVariable},
    },
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

pub(crate) fn typecheck_var_declaration(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
    ty: &HIRType,
    name: &CXIdent,
    initial_value: Option<&HIRExpression>,
    linkage: LinkageMode,
) -> CXResult<TypecheckResult> {
    let ty = complete_type(env, namespace, ty)?;

    let mem_type = env.symbols.mem_ref_to(ty.clone());

    if linkage == LinkageMode::Extern {
        let symbol_name = QualifiedName::new_raw(name.clone());
        let mut incomplete_array = false;
        let symbol = if let Some(symbol) = env.get_symbol(namespace, &symbol_name)? {
            symbol
        } else {
            let mut candidates = env
                .symbols
                .get_global_registry()
                .resolve_unmangled_global(name.as_str())
                .into_iter();
            if let Some((symbol_namespace, symbol_definition)) = candidates.next() {
                if candidates.next().is_some() {
                    return env.log_error(
                        expr.token_range(),
                        format!("External variable '{}' is ambiguous", name),
                    );
                }

                incomplete_array = matches!(
                    &symbol_definition.kind,
                    HIRSymbolKind::AddressableGlobal {
                        _type,
                        ..
                    } if matches!(_type.kind, HIRTypeKind::ImplicitSizedArray(_))
                );

                resolve_symbol_without_implicit_array_decay(
                    env,
                    namespace,
                    &EnvironmentNamespace::from(&symbol_namespace),
                    name,
                    &symbol_definition,
                )?
            } else {
                env.items.push_generated_global(THIRGlobalVariable {
                    is_mutable: true,
                    linkage: LinkageMode::Extern,
                    kind: THIRGlobalVarKind::Variable {
                        name: name.clone(),
                        _type: ty.clone(),
                        initializer: None,
                    },
                });

                MIRSymbol::Expression(THIRExpression {
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::GlobalVariable {
                        symbol: name.clone(),
                    },
                    _type: mem_type.clone(),
                })
            }
        };
        let global = symbol
            .as_expression()
            .map_err(|err| env.error(expr.token_range(), err.message().to_string()))?;
        let expected_array = mem_type
            .mem_ref_inner()
            .and_then(|inner| env.symbols.try_resolve_type_id(inner)?.array_inner())
            .is_some();
        let incomplete_array = incomplete_array
            || (expected_array
                && global
                    ._type
                    .mem_ref_inner()
                    .and_then(|inner| env.symbols.try_resolve_type_id(inner)?.ptr_inner())
                    .is_some());
        let global = if incomplete_array && expected_array {
            match &global.kind {
                THIRExpressionKind::GlobalVariable { symbol } => THIRExpression {
                    token_range: global.token_range.clone(),
                    kind: THIRExpressionKind::GlobalVariable {
                        symbol: symbol.clone(),
                    },
                    _type: mem_type.clone(),
                },
                _ => global,
            }
        } else {
            global
        };

        if !env.type_eq(&global._type, &mem_type) {
            return env.log_error(
                expr.token_range(),
                format!("External variable '{}' has an incompatible type", name),
            );
        }

        env.symbols.insert_local_value(symbol_name, global);
        return Ok(TypecheckResult::from(THIRExpression {
            token_range: TokenRange::internal(),
            kind: THIRExpressionKind::Unit,
            _type: THIRType::unit(),
        }));
    }

    ensure_valid_allocation_type(env, expr.token_range().clone(), "a variable", &ty)?;

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

    Ok(TypecheckResult::from(binding))
}
