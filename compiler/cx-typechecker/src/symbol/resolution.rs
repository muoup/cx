use cx_ast::{ast::template::CXTemplatePrototype, symbols::{CXSymbol, CXSymbolKind}};
use cx_util::{CXError, CXResult, identifier::CXIdent};

use cx_mir::{
    EnvironmentNamespace,
    mir::{
        data::{MIRTemplateInput, TemplateInfo},
        expression::{MIRExpression, MIRExpressionKind, SymbolValueOrigin},
        name_mangling::base_mangle_templated_name,
        r#type::MIRTypeKind,
    },
    symbol::MIRSymbol,
    type_context::MIRTypeContext,
};

use crate::symbol::{
    completion::{complete_prototype, complete_type},
    r#enum::resolve_enum_block,
    registry::MIRSymbolRegistry,
};

pub fn resolve_symbol<'a, 'b>(
    env: &'a mut MIRSymbolRegistry<'b>,
    namespace: &EnvironmentNamespace,
    name: &CXIdent,
    symbol: &CXSymbol,
) -> CXResult<MIRSymbol> {
    match &symbol.kind {
        CXSymbolKind::Type(ty) => {
            let mut completed = complete_type(env, namespace, ty)?;
            if completed.debug_name.is_none() {
                completed.debug_name = Some(name.clone());
            }
            let id = env.generate_type_id(completed);
            Ok(MIRSymbol::Type(id))
        }

        CXSymbolKind::AddressableGlobal(name, ty) => {
            let ty = complete_type(env, &namespace, ty)?;
            let mem_ty = env.mem_ref_to(ty);
            Ok(MIRSymbol::Expression(MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::Variable {
                    name: name.clone(),
                    location: SymbolValueOrigin::Global,
                },
                _type: mem_ty,
            }))
        }

        CXSymbolKind::FunctionReference(prototype) => {
            let prototype = complete_prototype(env, namespace, prototype)?;

            Ok(MIRSymbol::Expression(MIRExpression {
                token_range: None,
                _type: MIRTypeKind::Function {
                    signature: Box::new(prototype.signature),
                }
                .into(),
                kind: MIRExpressionKind::FunctionReference {
                    name: prototype.name,
                },
            }))
        }

        CXSymbolKind::EnumIdent {
            enum_block_idx,
            variant_index,
        } => resolve_enum_block(env, namespace, *enum_block_idx).map(|b| {
            b.variant_expr(*variant_index)
                .expect("Expected enum variant to be in the global registry")
                .clone()
        }),

        CXSymbolKind::TypeTemplate { template: input, definition } => {
            let source = CXSymbol::new(symbol.visibility, CXSymbolKind::Type(definition.clone()));

            Ok(MIRSymbol::Template {
                input: input.clone(),
                name: name.clone(),
                source: Box::new(source),
                namespace: namespace.clone(),
            })
        }

        CXSymbolKind::FunctionTemplate {
            template: input, definition, ..
        } => {
            let source = CXSymbol::new(
                symbol.visibility,
                CXSymbolKind::FunctionReference(definition.clone()),
            );

            Ok(MIRSymbol::Template {
                input: input.clone(),
                name: name.clone(),
                source: Box::new(source),
                namespace: namespace.clone(),
            })
        }
    }
}

pub fn apply_template<'a, 'b>(
    env: &'a mut MIRSymbolRegistry<'b>,
    symbol: &MIRSymbol,
    template_input: MIRTemplateInput,
) -> CXResult<Option<MIRSymbol>> {
    let MIRSymbol::Template {
        input,
        name,
        source,
        namespace,
    } = symbol
    else {
        return Ok(None);
    };

    if input.types.len() != template_input.args.len() {
        return CXError::create_result(format!(
            "Template '{}' expects {} arguments, found {}",
            name,
            input.types.len(),
            template_input.args.len()
        ));
    }

    env.push_local_scope();
    apply_template_input(env, input, &template_input)?;

    let mut result = resolve_symbol(env, namespace, name, source)?;
    env.pop_local_scope();

    attach_template_metadata(env, &mut result, name, namespace, template_input);
    Ok(Some(result))
}

pub fn apply_template_input(
    env: &mut MIRSymbolRegistry,
    prototype: &CXTemplatePrototype,
    input: &MIRTemplateInput,
) -> CXResult<()> {
    for (param, arg) in prototype.types.iter().zip(input.args.iter()) {
        env.insert_local_type(param.as_string(), arg.clone())?;
    }

    Ok(())
}

fn attach_template_metadata(
    env: &mut MIRSymbolRegistry,
    symbol: &mut MIRSymbol,
    name: &CXIdent,
    _namespace: &EnvironmentNamespace,
    input: MIRTemplateInput,
) {
    let MIRSymbol::Type(id) = symbol else {
        return;
    };

    let mut ty = env.resolve_type_id(*id).clone();
    ty.strong_identifier.as_mut().map(|base| {
        base.name =
            base_mangle_templated_name(env, base.name.as_str(), input.args.as_slice()).into()
    });
    ty.template_info = Some(Box::new(TemplateInfo {
        base_name: name.clone(),
        template_input: input,
    }));

    Some(name.clone());
    env.overwrite_type_id(*id, ty);
}
