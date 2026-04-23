use crate::{environment::TypeEnvironment, log_typecheck_error};
use cx_ast::ast::{CXExpr, CXExprKind, CXGlobalVariable};
use cx_mir::mir::{
    data::{MIRIntegerType, MIRType, MIRTypeContext, MIRTypeKind},
    expression::{MIRExpression, MIRExpressionKind},
    program::{MIRBaseMappings, MIRGlobalVarKind, MIRGlobalVariable},
};
use cx_util::{CXError, CXResult, identifier::CXIdent};

pub(crate) fn global_expr(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    ident: &str,
) -> CXResult<MIRExpression> {
    if let Some(global) = env.items.realized_globals.get(ident) {
        return tcglobal_expr(global, &mut env.symbols.context);
    }

    let Some(module_res) = base_data.global_variables.get(ident) else {
        return CXError::create_result(format!("Global variable '{}' not found", ident));
    };

    let module_res = match env.items.in_external_templated_function {
        true => module_res.clone().transfer(""),
        false => module_res.clone(),
    };

    match &module_res.resource {
        CXGlobalVariable::EnumConstant(val) => Ok(MIRExpression {
            token_range: None,
            kind: MIRExpressionKind::IntLiteral(
                *val as i64,
                MIRIntegerType::from_bytes(8).unwrap(),
                true,
            ),
            _type: MIRType::from(MIRTypeKind::Integer {
                _type: MIRIntegerType::from_bytes(8).unwrap(),
                signed: true,
            }),
        }),

        CXGlobalVariable::Standard {
            _type,
            initializer,
            is_mutable,
        } => {
            let _type = env.complete_type(base_data, &CXExpr::default(), _type)?;
            let _initializer = match initializer.as_ref() {
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
                    linkage: module_res.linkage,
                },
            );

            tcglobal_expr(
                env.items.realized_globals.get(ident).unwrap(),
                &mut env.symbols.context,
            )
        }
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
