use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{
        constexpr::constexpr_evaluate, typechecker::typecheck_expr,
        value::ensure_valid_allocation_type,
    },
};
use cx_ast::{
    ast::{CXExprKind, CXExpression, CXGlobalVariable},
    data::{CXLinkageMode, ModuleResource},
};
use cx_mir::mir::{
    data::{MIRIntegerType, MIRType, MIRTypeContext, MIRTypeKind},
    expression::{MIRExpression, MIRExpressionKind},
    program::{MIRBaseMappings, MIRGlobalVarKind, MIRGlobalVariable},
};
use cx_util::{CXResult, identifier::CXIdent};

pub fn complete_base_globals(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
) -> CXResult<()> {
    for (name, module_res) in base_data.global_variables.iter() {
        if matches!(module_res.resource, CXGlobalVariable::EnumDefinition { .. }) {
            complete_global(env, base_data, name, module_res)?;
        }
    }

    for (name, module_res) in base_data.global_variables.iter() {
        if matches!(module_res.resource, CXGlobalVariable::Standard { .. }) {
            complete_global(env, base_data, name, module_res)?;
        }
    }

    Ok(())
}

fn complete_global(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    ident: &str,
    module_res: &ModuleResource<CXGlobalVariable>,
) -> CXResult<()> {
    let owns_base_data = base_data.unit == env.source.compilation_unit.as_str();
    let owns_resource = owns_base_data && module_res.external_module.is_none();

    match &module_res.resource {
        CXGlobalVariable::EnumDefinition { variants } => {
            let mut previous = None;

            for variant in variants {
                let value = if let Some(expr) = variant.value.as_ref() {
                    let value = typecheck_expr(env, base_data, expr, None)?.into_expression();
                    let Some(value) = constexpr_evaluate(env, value)?.get_integer() else {
                        return log_typecheck_error!(
                            env,
                            Some(expr.token_range()),
                            "Invalid enum variant value, expected integer result"
                        );
                    };
                    value
                } else {
                    previous.map(|value| value + 1).unwrap_or(0)
                };

                previous = Some(value);
                env.insert_pure_expr(variant.name.clone(), enum_literal(value));
            }

            Ok(())
        }

        CXGlobalVariable::Standard {
            _type,
            initializer,
            is_mutable,
        } => {
            let _type = env.complete_type(base_data, &CXExpression::default(), _type)?;
            ensure_valid_allocation_type(env, None, "a global variable", &_type)?;
            let _initializer = if owns_resource {
                match initializer.as_ref() {
                    Some(init_expr) => {
                        let CXExprKind::IntLiteral { val, .. } = &init_expr.kind else {
                            return log_typecheck_error!(
                                env,
                                Some(init_expr.token_range()),
                                "CX currently only supports integer initializers for global variables"
                            );
                        };

                        Some(*val)
                    }

                    None => None,
                }
            } else {
                None
            };
            let linkage = if owns_resource {
                module_res.linkage
            } else {
                CXLinkageMode::Extern
            };

            env.items.realized_globals.insert(
                ident.to_string(),
                MIRGlobalVariable {
                    kind: MIRGlobalVarKind::Variable {
                        name: CXIdent::new(ident.to_string()),
                        initializer: _initializer,
                        _type,
                    },
                    is_mutable: *is_mutable,
                    linkage,
                },
            );

            let expr = tcglobal_expr(
                env.items.realized_globals.get(ident).unwrap(),
                &mut env.symbols.context,
            )?;
            env.insert_value_symbol(CXIdent::new(ident), expr);
            Ok(())
        }
    }
}

fn enum_literal(value: i64) -> MIRExpression {
    MIRExpression {
        token_range: None,
        kind: MIRExpressionKind::IntLiteral(value, MIRIntegerType::from_bytes(8).unwrap(), true),
        _type: MIRType::from(MIRTypeKind::Integer {
            _type: MIRIntegerType::from_bytes(8).unwrap(),
            signed: true,
        }),
    }
}

fn tcglobal_expr(
    global: &MIRGlobalVariable,
    definitions: &mut MIRTypeContext,
) -> CXResult<MIRExpression> {
    match &global.kind {
        MIRGlobalVarKind::Variable { name, _type, .. } => Ok(MIRExpression {
            token_range: None,
            kind: MIRExpressionKind::Variable(name.clone()),
            _type: definitions.mem_ref_to(_type.clone()),
        }),

        MIRGlobalVarKind::StringLiteral { .. } => {
            unreachable!("String literals cannot be referenced via an identifier")
        }
    }
}
