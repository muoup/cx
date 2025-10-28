use crate::environment::TCEnvironment;
use crate::log_typecheck_error;
use crate::type_checking::casting::try_implicit_cast;
use crate::type_checking::typechecker::typecheck_expr;
use crate::type_completion::complete_type;
use crate::type_completion::types::_complete_type;
use cx_parsing_data::ast::CXExpr;
use cx_parsing_data::data::{CXNaiveFunctionContract, CXNaiveParameter, CXNaivePrototype, CXNaiveTemplateInput, NaiveFnKind};
use cx_typechecker_data::ast::{TCBaseMappings, TCExpr};
use cx_typechecker_data::cx_types::{CXTemplateInput, CXTypeKind, TCFunctionContract, TCFunctionPrototype, TCParameter};
use cx_typechecker_data::function_map::{CXFunctionIdentifier, CXFunctionKind};
use cx_util::identifier::CXIdent;
use cx_util::{CXResult, log_error};

pub(crate) fn apply_implicit_fn_attr(
    mut proto: CXNaivePrototype,
) -> CXNaivePrototype {
    if let Some(implicit_member) = proto.name.implicit_member() {
        proto.params.insert(
            0,
            CXNaiveParameter {
                name: Some(CXIdent::from("this")),
                _type: implicit_member.as_type().pointer_to(false, 0),
            },
        );
    }

    proto
}

pub fn complete_template_args(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    template_args: &CXNaiveTemplateInput,
) -> Option<CXTemplateInput> {
    let args = template_args
        .params
        .iter()
        .map(|arg| complete_type(env, base_data, None, arg))
        .collect::<Option<Vec<_>>>()?;

    Some(CXTemplateInput { args })
}

pub(crate) fn complete_fn_ident(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    ident: &NaiveFnKind,
) -> Option<CXFunctionIdentifier> {
    match ident {
        NaiveFnKind::MemberFunction {
            function_name,
            _type,
        } => {
            let base = _type.as_type();
            let Some(_ty) = env.complete_type(base_data, &base) else {
                log_error!("Unknown type for member function: {function_name} of type {_type}");
            };

            let Some(cx_type) = _ty.get_identifier() else {
                log_error!(
                    "Member function base type should be identifiable: {function_name} of type {_type}"
                );
            };

            Some(
                CXFunctionKind::Member {
                    base_type: cx_type.clone(),
                    name: function_name.clone(),
                }
                .into(),
            )
        }

        NaiveFnKind::Destructor(name) => {
            let base = name.as_type();
            let Some(_ty) = _complete_type(env, base_data, &base) else {
                log_error!("Unknown type for destructor: {name}");
            };

            let Some(cx_type) = _ty.get_identifier() else {
                log_error!("Destructor base type should be identifiable: {name}");
            };

            Some(
                CXFunctionKind::Destructor {
                    base_type: cx_type.clone(),
                }
                .into(),
            )
        }

        NaiveFnKind::Standard(name) => {
            Some(CXFunctionKind::Standard { name: name.clone() }.into())
        }
    }
}

fn _complete_fn_contract_clause(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    clause: &CXExpr,
) -> CXResult<TCExpr> {
    let mut checked_clause = typecheck_expr(env, base_data, clause)?;
    let cast_result = try_implicit_cast(&mut checked_clause, &CXTypeKind::Bool.into());
    
    if let None = cast_result {
        log_typecheck_error!(
            env,
            clause,
            "Function contract clause must evaluate to an integer (boolean), found: {}",
            checked_clause._type
        );
    }
    
    Some(checked_clause)
}

fn _complete_fn_contract(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    parameters: &Vec<TCParameter>,
    contract: &CXNaiveFunctionContract,
) -> CXResult<TCFunctionContract> {
    env.push_scope();

    for param in parameters {
        if let Some(name) = &param.name {
            env.insert_symbol(name.to_string(), param._type.clone());
        }
    }

    let precondition = contract.precondition.as_ref()
        .map(|pre| _complete_fn_contract_clause(env, base_data, &pre).ok_or(()))
        .transpose()
        .ok()?;
    let postcondition = contract.postcondition.as_ref()
        .map(|post| _complete_fn_contract_clause(env, base_data, &post).ok_or(()))
        .transpose()
        .ok()?;
    
    let contract = TCFunctionContract {
        precondition,
        postcondition,
    };
    
    env.pop_scope();

    Some(contract)
}

pub fn _complete_fn_prototype(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    prototype: &CXNaivePrototype,
) -> Option<TCFunctionPrototype> {
    let prototype = apply_implicit_fn_attr(prototype.clone());
    let ident = complete_fn_ident(env, base_data, &prototype.name)?;
    
    let return_type = env.complete_type(base_data, &prototype.return_type)?;

    let parameters = prototype
        .params
        .iter()
        .map(|CXNaiveParameter { name, _type }| {
            let param_type = env.complete_type(base_data, _type)?;

            Some(TCParameter {
                name: name.clone(),
                _type: param_type,
            })
        })
        .collect::<Option<Vec<_>>>()?;
    
    let contract = prototype.contract.as_ref()
        .map(|contract| _complete_fn_contract(env, base_data, &parameters, contract).ok_or(()))
        .transpose()
        .ok()?;
    
    let prototype = TCFunctionPrototype {
        name: ident.clone(),
        return_type: return_type,
        params: parameters,
        var_args: prototype.var_args,
        contract,
    };
    
    if !env.realized_fns.contains_key(&ident) {
        env.realized_fns.insert(ident, prototype.clone());
    }
    
    Some(prototype)
}
