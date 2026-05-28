use crate::{
    environment::TypeEnvironment, type_checking::result::TypecheckResult,
    type_checking::typechecker::typecheck_expr,
};
use cx_ast::{ast::CXExpression, data::CXType};
use cx_mir::mir::{
    data::{MIRIntegerType, MIRType, MIRTypeKind},
    expression::{MIRExpression, MIRExpressionKind},
    program::MIRBaseMappings,
};
use cx_util::CXResult;

pub(crate) fn typecheck_sizeof_type(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    ty: &CXType,
) -> CXResult<TypecheckResult> {
    let tc_type = env.complete_type(base_data, expr, ty)?;
    Ok(sizeof_result(tc_type.padded_size(&env.symbols.context)))
}

pub(crate) fn typecheck_sizeof_expr(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    let tc_expr = typecheck_expr(env, base_data, expr, None)?;
    Ok(sizeof_result(
        tc_expr.get_type()?.padded_size(&env.symbols.context),
    ))
}

fn sizeof_result(size: usize) -> TypecheckResult {
    TypecheckResult::from(MIRExpression {
        token_range: None,
        kind: MIRExpressionKind::IntLiteral(size as i64, MIRIntegerType::I64, false),
        _type: MIRType::from(MIRTypeKind::Integer {
            _type: MIRIntegerType::I64,
            signed: false,
        }),
    })
}
