use cx_data_ast::parse::identifier::CXIdent;
use crate::environment::TCEnvironment;
use cx_data_typechecker::ast::{TCExpr, TCExprKind};
use cx_data_typechecker::cx_types::{CXType, CXTypeKind};
use cx_util::CXResult;

pub(crate) fn visit_destructable_instance(env: &mut TCEnvironment, ty: &CXType) -> bool {
    if env.deconstructors.contains(ty) {
        return true;
    }

    match &ty.kind {
        CXTypeKind::StrongPointer { inner_type, .. } => {
            let _ = visit_destructable_instance(env, inner_type);

            // Strong pointers, due to some problems with how identically defined types
            // defined separately have different UUIDs, we just handle all strong pointers
            // in-place in the function they are used in. May be revisited later.

            true
        },

        CXTypeKind::Structured { name: Some(name), fields } => {
            let any_field_deconstructable = fields.into_iter()
                .any(|(_, field_type)| visit_destructable_instance(env, &field_type));

            if !any_field_deconstructable && !env.destructor_exists(name.as_str()) {
                return false;
            }

            env.deconstructors.insert(ty.clone());
            true
        }

        _ => false
    }
}