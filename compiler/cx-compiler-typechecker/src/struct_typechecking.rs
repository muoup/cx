use cx_data_ast::parse::ast::{CXExpr, CXExprKind, CXUnOp};
use cx_data_ast::parse::value_type::{struct_field_type, CXTypeKind, CXType};
use cx_util::{log_error};
use crate::checker::{coerce_value, type_check_traverse};
use crate::TypeEnvironment;

pub fn typecheck_access(
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
    
    let CXExprKind::Identifier(accessor) = &rhs.kind else {
        log_error!("TYPE ERROR: Invalid struct accessor: {rhs}");
    };
    
    match lhs_type.mem_ref_inner(env.type_map)? {
        CXTypeKind::Union { fields, .. } |
        CXTypeKind::Structured { fields, .. } => {
            fields.iter()
                .find(|(name, _)| name == accessor.as_str())
                .map(|(_, field_type)| {
                    CXType::new(
                        0,
                        CXTypeKind::MemoryAlias(Box::new(field_type.clone()))
                    )
                })
                .or_else(|| log_error!("TYPE ERROR: Invalid union accessor {accessor} for type {lhs_type}"))
        },
        
        _ => log_error!("TYPE ERROR: Cannot access field on {accessor} type {lhs_type}")
    }
}