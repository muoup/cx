use cx_ast::symbols::{CXSymbol, CXSymbolKind};
use cx_util::{CXError, CXResult, identifier::CXIdent};

use cx_mir::{
    EnvironmentNamespace,
    mir::{
        data::{MIRTemplateInput, TemplateInfo},
        expression::{MIRExpression, MIRExpressionKind, SymbolValueOrigin},
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

pub fn resolve_symbol(
    env: &mut MIRSymbolRegistry,
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
        } => resolve_enum_block(env, namespace, *enum_block_idx, *variant_index),

        CXSymbolKind::TypeTemplate { input, definition } => {
            let source = CXSymbol::new(symbol.visibility, CXSymbolKind::Type(definition.clone()));

            Ok(MIRSymbol::Template {
                input: input.clone(),
                name: name.clone(),
                source: Box::new(source),
                namespace: namespace.clone(),
            })
        }

        CXSymbolKind::FunctionTemplate {
            input, definition, ..
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

pub fn apply_template(
    env: &mut MIRSymbolRegistry,
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

    env.push_scope();
    for (param, arg) in input.types.iter().zip(template_input.args.iter()) {
        env.insert_local_type(param.as_string(), arg.clone())?;
    }

    let result = resolve_symbol(env, namespace, name, source);
    env.pop_scope();

    let mut symbol = result?;
    attach_template_metadata(env, &mut symbol, name, namespace, template_input);
    Ok(Some(symbol))
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
    ty.template_info = Some(Box::new(TemplateInfo {
        base_name: name.clone(),
        template_input: input,
    }));
    ty.strong_identifier = Some(name.clone());
    ty.debug_name.get_or_insert_with(|| name.name.clone());
    env.overwrite_type_id(*id, ty);
}
