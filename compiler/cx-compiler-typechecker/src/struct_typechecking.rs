use cx_data_ast::parse::ast::{CXExpr, CXUnOp};
use cx_data_ast::parse::value_type::{get_intrinsic_type, is_structure, struct_field_type, CXTypeUnion, CXValType};
use cx_util::log_error;
use crate::checker::type_check_traverse;
use crate::TypeEnvironment;

pub fn access_struct(
    env: &mut TypeEnvironment,
    lhs: &mut CXExpr,
    rhs: &CXExpr
) -> Option<CXValType> {
    let mut lhs_type = type_check_traverse(env, lhs)?;

    if let CXTypeUnion::PointerTo(inner) = lhs_type.internal_type {
        let lhs_temp = std::mem::replace(lhs, CXExpr::Taken);
        *lhs =
            CXExpr::UnOp {
                operator: CXUnOp::Dereference,
                operand: Box::new(lhs_temp)
            };

        lhs_type = *inner;
    }

    if !is_structure(env.type_map, &lhs_type) {
        log_error!("TYPE ERROR: Access operator can only be applied to structured types, found {lhs_type}");
    }

    let CXExpr::Identifier(accessor) = rhs else {
        log_error!("TYPE ERROR: Invalid struct accessor: {rhs}");
    };

    let Some(field_type) = struct_field_type(env.type_map, &lhs_type, accessor.as_str()) else {
        log_error!("TYPE ERROR: Invalid struct accessor {accessor} for type {lhs_type}");
    };

    Some(
        CXValType::new(
            0,
            CXTypeUnion::MemoryReference(Box::new(field_type.clone()))
        )
    )
}