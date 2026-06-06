use cx_ast::{
    ast::{
        function::CXFunctionContract,
        modifiers::CXLinkageMode,
        template::CXTemplateInput,
        types::{CXTypeKind, PredeclarationType},
    },
    symbols::CXSymbolKind,
};
use cx_mir::{
    EnvironmentNamespace,
    mir::{
        data::{MIRFunctionPrototype, MIRFunctionSignature, MIRParameter},
        expression::{MIRExpression, MIRExpressionKind},
        name_mangling::base_mangle_static_member,
        r#type::MIRTypeKind,
    },
    symbol::MIRSymbol,
};
use cx_util::{CXResult, identifier::CXIdent, namespace::QualifiedName};

use crate::{
    environment::{MIRFunctionGenRequest, TypeEnvironment},
    symbol::completion::complete_type,
};

pub fn query_type_constructor(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    template_input: Option<&CXTemplateInput>,
) -> CXResult<Option<MIRSymbol>> {
    let Some((union_namespace, union_name)) = name.namespace.parent_and_name() else {
        return Ok(None);
    };
    let union_name = QualifiedName::new(union_namespace, union_name);

    let Some(type_lookup_name) =
        existing_type_symbol_name(env, namespace, &union_name, template_input)
    else {
        return Ok(None);
    };

    let union_cx_type = CXTypeKind::Identifier {
        name: type_lookup_name,
        predeclaration: PredeclarationType::None,
        template_input: template_input.cloned(),
    }
    .to_type();

    let union_type = complete_type(env, namespace, &union_cx_type)?;
    let Some(variants) = union_type.aggregate_fields(&env.symbols) else {
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

    let mangled_name = base_mangle_static_member(&env.symbols, name.name.as_str(), &union_type);
    let prototype = MIRFunctionPrototype {
        name: CXIdent::new(mangled_name.clone()),
        linkage: CXLinkageMode::Static,
        signature: MIRFunctionSignature {
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
        },
    };

    if !env.items.has_type_constructor_request(&mangled_name) {
        env.items
            .push_request(MIRFunctionGenRequest::TypeConstructor {
                name: mangled_name,
                union_type,
                variant_type,
                variant_index,
            });
    }

    Ok(Some(MIRSymbol::Expression(MIRExpression {
        token_range: None,
        _type: MIRTypeKind::Function {
            signature: Box::new(prototype.signature),
        }
        .into(),
        kind: MIRExpressionKind::FunctionReference {
            name: prototype.name,
        },
    })))
}

fn existing_type_symbol_name(
    env: &TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    template_input: Option<&CXTemplateInput>,
) -> Option<QualifiedName> {
    let candidates = if name.namespace.is_root() {
        vec![
            QualifiedName::new(namespace.clone(), name.name.clone()),
            name.clone(),
        ]
    } else {
        vec![env.symbols.resolve_qualified_alias(name).into_owned()]
    };

    candidates.into_iter().find(|candidate| {
        env.symbols
            .get_global_registry()
            .resolve(candidate)
            .map(|symbol| {
                matches!(
                    (template_input.is_some(), symbol.kind),
                    (true, CXSymbolKind::TypeTemplate { .. }) | (false, CXSymbolKind::Type(_))
                )
            })
            .unwrap_or(false)
    })
}
