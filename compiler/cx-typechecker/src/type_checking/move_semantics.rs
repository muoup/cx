use crate::environment::TypeEnvironment;
use cx_typechecker_data::mir::{
    program::MIRBaseMappings,
    types::{CXType, CXTypeKind},
};

pub(crate) fn acknowledge_declared_type(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    ty: &CXType,
) -> bool {
    if env.deconstructors.contains(ty) {
        return true;
    }

    match &ty.kind {
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

pub fn acknowledge_object_destructed(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    ty: &CXType,
) {
    let _ = acknowledge_declared_type(env, base_data, ty);
}