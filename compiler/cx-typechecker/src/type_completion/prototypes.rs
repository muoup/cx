use crate::environment::TypeEnvironment;
use crate::environment::name_mangling::base_mangle_fn_name;
use crate::type_checking::binary_ops::typecheck_contract;
use cx_ast::data::{CXReceiverMode, CXTemplateInput, CXParameter, CXPrototype, CXTypeKind};
use cx_mir::mir::program::MIRBaseMappings;
use cx_mir::mir::types::{MIRFunctionPrototype, MIRParameter, MIRReceiverMode, MIRTemplateInput};
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

pub(crate) fn apply_implicit_fn_attr(mut proto: CXPrototype) -> CXPrototype {
    if let Some(implicit_member) = proto.kind.implicit_member() {
        let receiver_base_type = implicit_member
            .as_type()
            .add_specifier(proto.receiver_specifiers);

        let receiver_type = match proto.receiver_mode {
            CXReceiverMode::ByRef => CXTypeKind::MemoryReference {
                inner_type: Box::new(receiver_base_type),
            }
            .to_type(),
            CXReceiverMode::ByMove => receiver_base_type,
            CXReceiverMode::None => return proto,
        };

        proto.params.insert(
            0,
            CXParameter {
                name: Some(CXIdent::new("this")),
                _type: receiver_type,
            },
        );
    }

    proto
}

pub fn complete_template_args(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    template_args: &CXTemplateInput,
) -> CXResult<MIRTemplateInput> {
    let args = template_args
        .params
        .iter()
        .map(|arg| env.complete_type(base_data, arg))
        .collect::<CXResult<Vec<_>>>()?;

    Ok(MIRTemplateInput { args })
}

pub fn _complete_fn_prototype(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    prototype: &CXPrototype,
) -> CXResult<MIRFunctionPrototype> {
    let prototype = apply_implicit_fn_attr(prototype.clone());
    let return_type = env.complete_type(base_data, &prototype.return_type)?;

    let parameters = prototype
        .params
        .iter()
        .map(|CXParameter { name, _type }| {
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
        receiver_mode: match prototype.receiver_mode {
            CXReceiverMode::None => MIRReceiverMode::None,
            CXReceiverMode::ByRef => MIRReceiverMode::ByRef,
            CXReceiverMode::ByMove => MIRReceiverMode::ByMove,
        },
        return_type,
        params: parameters,
        contract,
        var_args: prototype.var_args,
    };

    Ok(prototype)
}
