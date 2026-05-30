use cx_ast::ast::modifiers::CXLinkageMode;
use cx_ast::{
    ast::{
        expression::CXExpression,
        function::{CXFunctionContract, CXFunctionKind},
        template::CXTemplateInput,
        types::{CXTypeKind, PredeclarationType},
    },
    symbols::{UntypedSymbol, UntypedSymbolKind},
};
use cx_mir::mir::{
    data::{MIRFunctionPrototype, MIRParameter, MIRType},
    name_mangling::base_mangle_standard,
    program::EnvironmentNamespace,
};
use cx_tokens::TokenRange;
use cx_util::{CXResult, identifier::CXIdent, namespace::QualifiedName};

use crate::{
    environment::symbols::templates::{deduce_function_template, instantiate_function_template},
    environment::{MIRFunctionGenRequest, TypeEnvironment},
    log_typecheck_error,
};

pub fn member_function_qualified_name(
    member_type: &MIRType,
    name: &CXIdent,
) -> Option<QualifiedName> {
    member_type
        .get_base_identifier()
        .map(|base_name| member_function_key(&QualifiedName::new_raw(base_name.clone()), name))
}

pub fn query_function(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    name: &QualifiedName,
    template_input: Option<&CXTemplateInput>,
    arg_types: &[MIRType],
) -> CXResult<Option<MIRFunctionPrototype>> {
    if template_input.is_none() && name.namespace.is_root() {
        let mangled_name = base_mangle_standard(name.name.as_str());

        if let Some(func_proto) = env.get_realized_func(&mangled_name) {
            return Ok(Some(func_proto));
        }
    }

    let lookup_name = contextual_name(namespace, name);
    if let Some(UntypedSymbolKind::Function(standard)) = env
        .symbols
        .global_registry
        .resolve(&lookup_name)
        .map(|s| &s.kind)
    {
        return env
            .complete_prototype(
                namespace,
                standard.external_module.as_ref(),
                &standard.resource,
            )
            .map(Some);
    }

    if let Some(UntypedSymbol::FunctionTemplate(template, _)) =
        env.symbols.global_registry.resolve(&lookup_name)
    {
        let prototype = if let Some(template_input) = template_input {
            instantiate_function_template(env, namespace, &template, template_input)
        } else {
            match deduce_function_template(env, namespace, &template, arg_types) {
                Ok(prototype) => Ok(prototype),
                Err(err) => {
                    log_typecheck_error!(env, expr.token_range(), "{}", err.error_content())
                }
            }
        }?;

        return Ok(Some(prototype));
    }

    if let Some(prototype) = query_type_constructor(env, namespace, name, template_input)? {
        return Ok(Some(prototype));
    }

    Ok(None)
}

fn query_type_constructor(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    template_input: Option<&CXTemplateInput>,
) -> CXResult<Option<MIRFunctionPrototype>> {
    let Some((union_namespace, union_name)) = name.namespace.parent_and_name() else {
        return Ok(None);
    };
    let union_name = QualifiedName::new(union_namespace, union_name);
    let union_flat_name = union_name.as_flat_name();

    let type_lookup_name = contextual_name(namespace, &union_name);
    let type_exists = env
        .symbols
        .global_registry
        .resolve(&type_lookup_name)
        .map(|symbol| {
            matches!(
                (template_input.is_some(), symbol),
                (true, UntypedSymbol::TypeTemplate(_)) | (false, UntypedSymbol::Type(_))
            )
        })
        .unwrap_or(false)
        || env.get_realized_type(&union_flat_name).is_some();
    if !type_exists {
        return Ok(None);
    }

    let as_type = if let Some(template_input) = template_input {
        CXTypeKind::TemplatedIdentifier {
            name: union_name.clone(),
            input: template_input.clone(),
        }
    } else {
        CXTypeKind::Identifier {
            name: union_name.clone(),
            predeclaration: PredeclarationType::None,
        }
    }
    .to_type();

    let union_type = env.complete_type(namespace, &CXExpression::default(), &as_type)?;
    let Some(variants) = union_type.aggregate_fields(&env.symbols.context) else {
        return Ok(None);
    };
    let Some((variant_index, variant_type)) = variants
        .iter()
        .enumerate()
        .find(|(_, (variant_name, _))| variant_name == name.name.as_str())
        .map(|(index, (_, variant_type))| (index, variant_type.clone()))
    else {
        return Ok(None);
    };

    let mangled_name = base_mangle_standard(&name.as_flat_name());
    let prototype = MIRFunctionPrototype {
        name: CXIdent::new(mangled_name.clone()),
        source_prototype: cx_ast::data::CXFunctionPrototype {
            kind: CXFunctionKind::Standard(CXIdent::new("__tagged_union_variant_ctor")),
            params: Vec::new(),
            return_type: CXTypeKind::Identifier {
                name: QualifiedName::new_raw(CXIdent::new("__tagged_union")),
                predeclaration: PredeclarationType::None,
            }
            .to_type(),
            var_args: false,
            contract: CXFunctionContract::default(),
            linkage: CXLinkageMode::Static,
            range: TokenRange::default(),
        },
        return_type: union_type.clone(),
        params: if variant_type.is_unit() {
            Vec::new()
        } else {
            vec![MIRParameter {
                name: Some(CXIdent::new("value")),
                _type: variant_type.clone(),
            }]
        },
        var_args: false,
        contract: CXFunctionContract::default(),
        linkage: CXLinkageMode::Static,
    };

    if env.get_realized_func(&mangled_name).is_none()
        && !env.items.requests.iter().any(|request| {
            matches!(
                request,
                MIRFunctionGenRequest::TypeConstructor { name, .. } if name == &mangled_name
            )
        })
    {
        env.request_function_generation(MIRFunctionGenRequest::TypeConstructor {
            name: mangled_name,
            union_type,
            variant_type,
            variant_index,
        });
    }

    Ok(Some(prototype))
}

fn contextual_name(namespace: &EnvironmentNamespace, name: &QualifiedName) -> QualifiedName {
    if name.namespace.is_root() {
        QualifiedName::new(namespace.clone(), name.name.clone())
    } else {
        name.clone()
    }
}
