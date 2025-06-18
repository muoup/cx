use cx_data_ast::parse::ast::{CXExpr, CXExprKind, CXUnOp};
use cx_data_ast::parse::value_type::{struct_field_type, CXTypeKind, CXType};
use cx_util::{log_error, expr_error_log};
use crate::checker::{coerce_value, type_check_traverse};
use crate::TypeEnvironment;

pub fn access_struct(
    env: &mut TypeEnvironment,
    lhs: &mut CXExpr,
    rhs: &CXExpr
) -> Option<CXType> {
    let mut lhs_type = coerce_value(env, lhs)?.clone();
    
    if lhs_type.is_pointer(env.type_map) {
        let lhs_temp = std::mem::take(lhs);
        let start_index = lhs_temp.start_index;
        let end_index = lhs_temp.end_index;
        
        *lhs =
            CXExprKind::UnOp {
                operator: CXUnOp::Dereference,
                operand: Box::new(lhs_temp)
            }.into_expr(start_index, end_index);

        lhs_type = type_check_traverse(env, lhs)?.clone();
    }

    if !lhs_type.is_structure_ref(env.type_map) {
        expr_error_log!(env.tokens, lhs.start_index, rhs.end_index, "TYPE ERROR: Access operator can only be applied to structured types, found {}", lhs_type);
    }

    let CXExprKind::Identifier(accessor) = &rhs.kind else {
        log_error!("TYPE ERROR: Invalid struct accessor: {rhs}");
    };

    let Some(field_type) = struct_field_type(env.type_map, &lhs_type, accessor.as_str()) else {
        log_error!("TYPE ERROR: Invalid struct accessor {accessor} for type {lhs_type}");
    };

    Some(
        CXType::new(
            0,
            CXTypeKind::MemoryAlias(Box::new(field_type.clone()))
        )
    )
}