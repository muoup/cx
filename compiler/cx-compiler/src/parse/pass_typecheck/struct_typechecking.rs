use crate::log_error;
use crate::parse::pass_ast::{CXBinOp, CXExpr, CXUnOp, TypeMap};
use crate::parse::pass_bytecode::typing::{get_intrinsic_type, get_type_size, struct_field_offset};
use crate::parse::pass_typecheck::checker::type_check_traverse;
use crate::parse::pass_typecheck::type_utils::{prototype_to_type, struct_field_access};
use crate::parse::pass_typecheck::TypeEnvironment;
use crate::parse::value_type::{is_structure, CXValType};

pub(crate) fn access_struct(
    env: &mut TypeEnvironment,
    lhs: &mut CXExpr,
    rhs: &CXExpr
) -> Option<CXValType> {
    let mut lhs_type = type_check_traverse(env, lhs)?;

    if let CXValType::PointerTo(inner) = lhs_type {
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

    let Some(field_type) = struct_field_type(env, &lhs_type, accessor.as_str()) else {
        log_error!("TYPE ERROR: Invalid struct accessor {accessor} for type {lhs_type}");
    };

    Some(field_type)
}

pub struct StructAccessRecord {
    pub field_type: CXValType,
    pub field_offset: usize,
    pub field_index: usize,
    pub field_name: String
}

pub fn struct_field_type(
    env: &mut TypeEnvironment,
    type_: &CXValType,
    field: &str
) -> Option<CXValType> {
    let CXValType::Structured { fields } = get_intrinsic_type(env.type_map, type_)? else {
        log_error!("Cannot access field {field} of non-structured type {type_}");
    };

    fields.iter()
        .find(|(name, _)| name == field)
        .map(|(_, ty)| ty.clone())
}