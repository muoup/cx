use cx_data_ast::parse::ast::{CXExpr, CXExprKind, CXUnOp};
use cx_data_ast::parse::value_type::{struct_field_type, CXTypeKind, CXType};
use cx_util::log_error;
use crate::checker::type_check_traverse;
use crate::TypeEnvironment;

pub fn access_struct(
    env: &mut TypeEnvironment,
    lhs: &mut CXExpr,
    rhs: &CXExpr
) -> Option<CXType> {
    let mut lhs_type = type_check_traverse(env, lhs)?.clone();

    if let CXTypeKind::PointerTo(inner) = lhs_type.kind {
        let lhs_temp = std::mem::take(lhs);
        *lhs =
            CXExprKind::UnOp {
                operator: CXUnOp::Dereference,
                operand: Box::new(lhs_temp)
            }.into();

        lhs_type = *inner;
    }
    
    if !lhs_type.is_structure(env.type_map) {
        log_error!("TYPE ERROR: Access operator can only be applied to structured types, found {lhs_type}");
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