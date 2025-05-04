use crate::log_error;
use crate::parse::pass_bytecode::typing::{get_intrinsic_type, get_type_size, struct_field_offset};
use crate::parse::pass_ast::{CXFunctionPrototype, TypeMap};
use crate::parse::pass_typecheck::struct_typechecking::StructAccessRecord;
use crate::parse::value_type::CXValType;
use crate::parse::pass_typecheck::TypeEnvironment;

pub fn type_matches(
    env: &TypeEnvironment,
    lhs: &CXValType,
    rhs: &CXValType
) -> Option<bool> {
    Some(get_intrinsic_val(env, lhs)? == get_intrinsic_val(env, rhs)?)
}

#[macro_export]
macro_rules! type_matches {
    ($env:expr, $lhs:expr, $rhs:pat) => {
        {
            use crate::parse::pass_typecheck::type_utils::get_intrinsic_val;

            matches!(get_intrinsic_val($env, $lhs)?, rhs)
        }
    };
}

pub fn get_intrinsic_val<'a>(
    env: &'a TypeEnvironment,
    val: &'a CXValType
) -> Option<&'a CXValType> {
    match val {
        CXValType::Identifier(ident) => {
            let type_ = env.type_map.get(ident.as_str())?;

            get_intrinsic_val(env, type_)
        },

        _ => Some(val)
    }
}

pub fn struct_field_access(
    type_map: &TypeMap,
    type_: &CXValType,
    field: &str
) -> Option<StructAccessRecord> {
    let CXValType::Structured { fields } = get_intrinsic_type(type_map, type_)? else {
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

        offset += get_type_size(type_map, ty)?;
    }

    None
}

pub fn prototype_to_type(prototype: &CXFunctionPrototype) -> Option<CXValType> {
    let return_type = prototype.return_type.clone();
    let args = prototype.parameters.iter()
        .cloned()
        .map(|param| param.type_)
        .collect::<Vec<_>>();

    Some(CXValType::Function {
        return_type: Box::new(return_type),
        args
    })
}