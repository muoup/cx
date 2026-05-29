use crate::environment::TypeEnvironment;
use crate::environment::functions::mangling::base_mangle_fn_name;
use crate::environment::symbols::completion::{int_complete_type, namespace_from_module};
use crate::log_typecheck_error;
use cx_ast::ast::CXExpression;
use cx_ast::data::{CXFunctionPrototype, CXParameter, CXReceiverMode, CXType, CXTypeKind};
use cx_mir::mir::data::{MIRFunctionPrototype, MIRParameter, MIRType, MIRTypeKind};
use cx_mir::mir::program::EnvironmentNamespace;
use cx_util::identifier::CXIdent;
use cx_util::{CXError, CXResult};

pub fn complete_prototype_no_insert(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    external_module: Option<&String>,
    prototype: &CXFunctionPrototype,
) -> CXResult<MIRFunctionPrototype> {
    let namespace = namespace_from_module(env, namespace, external_module);

    int_complete_fn_prototype(env, &namespace, prototype)
}

pub fn complete_type(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    external_module: Option<&String>,
    expr: &CXExpression,
    ty: &CXType,
) -> CXResult<MIRType> {
    let namespace = namespace_from_module(env, namespace, external_module);

    int_complete_type(env, &namespace, expr, ty)
}

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

pub(crate) fn valid_prototype_type(_ty: &MIRType) -> bool {
    !matches!(_ty.kind, MIRTypeKind::Function { .. } | MIRTypeKind::Str)
}

fn require_complete_prototype_type(
    env: &TypeEnvironment,
    ty: &MIRType,
    context: &str,
) -> CXResult<()> {
    if let Some(name) = ty.get_name()
        && let Some(id) = env.get_named_type_id(name.as_str())
        && !env.symbols.context.contains(id)
    {
        return CXError::create_result(format!(
            "Invalid {} type '{}' in function prototype: incomplete aggregate used by value",
            context,
            ty.display_with(&env.symbols.context)
        ));
    }

    Ok(())
}

pub fn int_complete_fn_prototype(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    prototype: &CXFunctionPrototype,
) -> CXResult<MIRFunctionPrototype> {
    let source_prototype = prototype.clone();
    let normalized_prototype = apply_implicit_fn_attr(source_prototype.clone());
    let return_type = env.complete_type(
        namespace,
        &CXExpression::default(),
        &normalized_prototype.return_type,
    )?;
    require_complete_prototype_type(env, &return_type, "return")?;
    if !valid_prototype_type(&return_type) {
        return log_typecheck_error!(
            env,
            Some(prototype.range.clone()),
            "Invalid return type '{}' in function prototype",
            return_type.display_with(&env.symbols.context)
        );
    }

    let mut params = normalized_prototype
        .params
        .iter()
        .map(|CXParameter { name, _type }| {
            let param_type = env.complete_type(namespace, &CXExpression::default(), _type)?;
            require_complete_prototype_type(env, &param_type, "parameter")?;

            if !valid_prototype_type(&param_type) {
                return log_typecheck_error!(
                    env,
                    Some(prototype.range.clone()),
                    "Invalid parameter type '{}' in function prototype",
                    param_type.display_with(&env.symbols.context)
                );
            }

            Ok(MIRParameter {
                name: name.clone(),
                _type: param_type,
            })
        })
        .collect::<CXResult<Vec<_>>>()?;

    let name = CXIdent::from(base_mangle_fn_name(env, namespace, &source_prototype.kind)?);

    if params.len() == 1 {
        let first_param = &params[0];

        if matches!(first_param._type.kind, MIRTypeKind::Unit) && first_param.name.is_none() {
            // Special case, if we have some function `int foo(void)` the 'void' indicates 0 parameters
            // for legacy reasons
            params.clear();
        }
    }

    let prototype = MIRFunctionPrototype {
        name,
        source_prototype,
        return_type,
        params,
        var_args: normalized_prototype.var_args,
        contract: prototype.contract.clone(),
        linkage: prototype.linkage,
    };

    Ok(prototype)
}
