use crate::environment::TCEnvironment;
use cx_typechecker_data::{ast::TCBaseMappings, cx_types::{CXType, CXTypeKind}};

pub(crate) fn acknowledge_declared_type(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    ty: &CXType,
) -> bool {
    if env.deconstructors.contains(ty) {
        return true;
    }

    match &ty.kind {
        CXTypeKind::StrongPointer { inner_type, .. } => {
            let _ = acknowledge_declared_type(env, base_data, inner_type);

            // Strong pointers, due to some problems with how identically defined types
            // defined separately have different UUIDs, we just handle all strong pointers
            // in-place in the function they are used in. May be revisited later.

            true
        }

        CXTypeKind::Structured {
            name: Some(_name),
            fields,
            ..
        } => {
            let any_field_deconstructable = fields
                .iter()
                .any(|(_, field_type)| acknowledge_declared_type(env, base_data, field_type));

            if !any_field_deconstructable && !env.destructor_exists(base_data, ty) {
                return false;
            }

            env.deconstructors.insert(ty.clone());
            true
        }

        _ => false,
    }
}
