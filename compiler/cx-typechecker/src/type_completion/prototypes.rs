use crate::environment::TypeEnvironment;
use crate::environment::name_mangling::base_mangle_fn_name;
use crate::type_checking::binary_ops::typecheck_contract;
use cx_parsing_data::data::{CXNaiveParameter, CXNaivePrototype, CXNaiveTemplateInput};
use cx_typechecker_data::mir::program::MIRBaseMappings;
use cx_typechecker_data::mir::types::{CXTemplateInput, MIRFunctionPrototype, MIRParameter};
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

pub(crate) fn apply_implicit_fn_attr(mut proto: CXNaivePrototype) -> CXNaivePrototype {
    if let Some(implicit_member) = proto.kind.implicit_member() {
        proto.params.insert(
            0,
            CXNaiveParameter {
                name: Some(CXIdent::new("this")),
                _type: implicit_member.as_type().pointer_to(true, 0),
            },
        );
    }

    proto
}

pub fn complete_template_args(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    template_args: &CXNaiveTemplateInput,
) -> CXResult<CXTemplateInput> {
    let args = template_args
        .params
        .iter()
        .map(|arg| env.complete_type(base_data, arg))
        .collect::<CXResult<Vec<_>>>()?;

    Ok(CXTemplateInput { args })
}

pub fn _complete_fn_prototype(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    prototype: &CXNaivePrototype,
) -> CXResult<MIRFunctionPrototype> {
    let prototype = apply_implicit_fn_attr(prototype.clone());
    let return_type = env.complete_type(base_data, &prototype.return_type)?;

    let parameters = prototype
        .params
        .iter()
        .map(|CXNaiveParameter { name, _type }| {
            let param_type = env.complete_type(base_data, _type)?;

            Ok(MIRParameter {
                name: name.clone(),
                _type: param_type,
            })
        })
        .collect::<CXResult<Vec<_>>>()?;

    let name = CXIdent::from(base_mangle_fn_name(env, base_data, &prototype.kind)?);
    let contract = typecheck_contract(env, base_data, &name, &prototype)?;
    
    let prototype = MIRFunctionPrototype {
        name: name,
        return_type,
        params: parameters,
        contract,
        var_args: prototype.var_args,
    };

    Ok(prototype)
}
