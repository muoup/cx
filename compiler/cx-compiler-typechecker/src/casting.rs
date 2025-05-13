use cx_data_ast::parse::ast::{CXExpr, TypeMap};
use cx_data_ast::parse::value_type::{CXTypeUnion, CXValType};
use crate::TypeEnvironment;

pub fn implicit_cast(expr: &mut CXExpr, from_type: &mut CXValType, to_type: &mut CXValType) -> Option<()> {
    match (from_type, to_type) {
        (x, y) if x == y => return Some(()),

        _ => return None
    }

    Some(())
}

pub fn legal_explicit_cast(env: &TypeEnvironment, expr: &mut CXExpr, from_type: &mut CXValType, to_type: &mut CXValType) -> Option<bool> {
    if let Some(_) = implicit_cast(expr, from_type, to_type) {
        return Some(true);
    }

    match (from_type.intrinsic_type(env.type_map).cloned()?, to_type.intrinsic_type(env.type_map).cloned()?) {
        (CXTypeUnion::PointerTo(_), CXTypeUnion::PointerTo(_)) => return Some(true),

        _ => return Some(true),
    }

    Some(true)
}