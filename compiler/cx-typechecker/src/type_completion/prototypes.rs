use crate::environment::TypeEnvironment;
use crate::environment::name_mangling::base_mangle_fn_name;
use crate::type_checking::binary_ops::typecheck_contract;
use cx_ast::data::{CXFunctionPrototype, CXParameter, CXReceiverMode, CXTemplateInput, CXTypeKind};
use cx_mir::mir::program::MIRBaseMappings;
use cx_mir::mir::types::{MIRFunctionPrototype, MIRParameter, MIRTemplateInput, MIRType, MIRTypeKind};
use cx_util::{CXError, CXResult};
use cx_util::identifier::CXIdent;

pub(crate) fn apply_implicit_fn_attr(mut proto: CXFunctionPrototype) -> CXFunctionPrototype {
    if let Some(implicit_member) = proto.kind.implicit_member() {
        let receiver = proto.kind.receiver().copied().unwrap_or_default();
        let receiver_base_type = implicit_member.as_type().add_specifier(receiver.specifiers);

        let receiver_type = match receiver.mode {
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

pub(crate) fn valid_prototype_type(_ty: &MIRType) -> bool {
    !matches!(_ty.kind, MIRTypeKind::Str)
}

pub fn _complete_fn_prototype(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    prototype: &CXFunctionPrototype,
) -> CXResult<MIRFunctionPrototype> {
    let source_prototype = prototype.clone();
    let normalized_prototype = apply_implicit_fn_attr(source_prototype.clone());
    let return_type = env.complete_type(base_data, &normalized_prototype.return_type)?;

    let parameters = normalized_prototype
        .params
        .iter()
        .map(|CXParameter { name, _type }| {
            let param_type = env.complete_type(base_data, _type)?;
            
            if !valid_prototype_type(&param_type) {
                return CXError::create_result(
                    format!("Invalid parameter type '{}' in function prototype", param_type),
                );
            }

            Ok(MIRParameter {
                name: name.clone(),
                _type: param_type,
            })
        })
        .collect::<CXResult<Vec<_>>>()?;

    let name = CXIdent::from(base_mangle_fn_name(env, base_data, &source_prototype.kind)?);
    let contract = typecheck_contract(env, base_data, &name, &normalized_prototype)?;

    let prototype = MIRFunctionPrototype {
        name,
        source_prototype,
        return_type,
        params: parameters,
        contract,
        var_args: normalized_prototype.var_args,
    };

    Ok(prototype)
}
