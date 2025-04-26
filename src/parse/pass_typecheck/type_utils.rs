use crate::log_error;
use crate::parse::pass_bytecode::typing::{get_type_size, struct_field_offset};
use crate::parse::value_type::CXValType;
use crate::parse::pass_typecheck::TypeEnvironment;

pub fn type_matches(
    env: &TypeEnvironment,
    lhs: &CXValType,
    rhs: &CXValType
) -> Option<bool> {
    Some(get_intrinsic_val(env, lhs)? == get_intrinsic_val(env, rhs)?)
}

pub fn get_intrinsic_val<'a>(
    env: &'a TypeEnvironment,
    val: &'a CXValType
) -> Option<&'a CXValType> {
    match val {
        CXValType::Identifier(ident) => {
            let type_ = env.type_map.get(ident)?;

            get_intrinsic_val(env, type_)
        },

        _ => Some(val)
    }
}

pub struct StructAccessRecord {
    pub field_type: CXValType,
    pub field_offset: usize,
    pub field_index: usize,
    pub field_name: String
}

pub fn struct_access(
    env: &TypeEnvironment,
    type_: &CXValType,
    field: &str
) -> Option<StructAccessRecord> {
    let CXValType::Structured { fields } = get_intrinsic_val(env, type_).cloned()? else {
        log_error!("Cannot access field {field} of non-structured type {type_}");
    };

    let mut offset = 0;

    for (i, (name, ty)) in fields.iter().enumerate() {
        if name == field {
            return Some(
                StructAccessRecord {
                    field_type: ty.clone(),
                    field_offset: offset,
                    field_index: i,
                    field_name: name.clone()
                }
            );
        }

        offset += get_type_size(env.type_map, ty)?;
    }

    None
}