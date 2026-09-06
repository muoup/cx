use cx_hir::{ast::template::HIRTemplatePrototype, symbols::HIRSymbolKind};
use cx_log::{
    CXRawResult,
    error::{CXErrorMaybeRaw, CXMaybeRawResult},
};
use cx_namespace::{mangling::mangle_namespace_symbol, module::QualifiedName};
use cx_thir::{
    symbol::MIRSymbol,
    thir::{
        data::{THIRFunction, THIRTemplateInput, THIRType, TemplateInfo},
        name_mangling::mangle_template_name,
    },
};

use super::{completion::complete_type_inner, resolution::resolve_symbol_inner};
use crate::environment::{THIRFunctionGenRequest, TypeEnvironment};

pub fn apply_template(
    env: &mut TypeEnvironment,
    symbol: &MIRSymbol,
    template_input: THIRTemplateInput,
) -> CXMaybeRawResult<Option<MIRSymbol>> {
    let MIRSymbol::Template {
        template_prototype: input,
        name,
        source,
        namespace,
        tag,
    } = symbol
    else {
        return Ok(None);
    };

    if input.types.len() != template_input.args.len() {
        return env
            .log_error_base(format!(
                "Template '{}' expects {} arguments, found {}",
                name,
                input.types.len(),
                template_input.args.len()
            ))
            .map_err(CXErrorMaybeRaw::from);
    }

    let mut symbol = env.in_definition(|env| -> CXMaybeRawResult<MIRSymbol> {
        apply_template_input(env, input, &template_input)?;
        if let HIRSymbolKind::Type(data) = &source.kind {
            let lookup_name = QualifiedName::new(namespace.clone(), name.clone());
            let instance_name = mangle_template_name(
                &env.symbols,
                mangle_namespace_symbol(&lookup_name),
                &template_input,
            );

            let key = (instance_name.clone(), tag.is_some());
            if let Some(id) = env.symbols.type_instances.get(&key) {
                return Ok(MIRSymbol::Type(*id));
            }

            let mut placeholder = THIRType::from(cx_thir::thir::data::THIRTypeKind::Undefined);
            placeholder.lookup_identifier = Some(lookup_name.clone());
            placeholder.strong_identifier = Some(instance_name.clone());

            let id = env.symbols.generate_type_id(placeholder);
            env.symbols.type_instances.insert(key.clone(), id);

            match complete_type_inner(env, namespace, data.base()) {
                Ok(mut ty) => {
                    if tag.is_some() || ty.template_info.is_none() {
                        ty.template_info = Some(Box::new(TemplateInfo {
                            base_name: Some(lookup_name),
                            template_input: template_input.clone(),
                        }));
                    }
                    if tag.is_some() {
                        ty.strong_identifier = Some(instance_name);
                    }
                    env.symbols.overwrite_type_id(id, ty);
                    return Ok(MIRSymbol::Type(id));
                }
                Err(error) => {
                    env.symbols.type_instances.remove(&key);
                    env.symbols.undo_type_id(id);
                    return Err(error.into());
                }
            }
        }
        resolve_symbol_inner(env, namespace, namespace, name, source, *tag, true)
            .map_err(Into::into)
    })?;

    if matches!(symbol, MIRSymbol::Type(_)) {
        return Ok(Some(symbol));
    }

    match &mut symbol {
        MIRSymbol::FunctionReference(prototype) if prototype.lookup_identifier().is_some() => {
            prototype.map_symbol_name(|name| {
                mangle_template_name(&env.symbols, name.to_owned(), &template_input)
            });
        }
        MIRSymbol::ComptimeFunctionReference { prototype, .. } => {
            prototype.map_symbol_name(|name| {
                mangle_template_name(&env.symbols, name.to_owned(), &template_input)
            });
        }
        _ => (),
    }

    if let MIRSymbol::ComptimeFunctionReference { prototype, .. } = &symbol {
        env.items.push_request(THIRFunctionGenRequest::Comptime {
            name: prototype.lookup_identifier().clone(),
            prototype: prototype.clone(),
            input: template_input.clone(),
        });
    }

    if let MIRSymbol::FunctionReference(prototype) = &symbol
        && let Some(name) = prototype.lookup_identifier().cloned()
    {
        env.items.push_generated_function(THIRFunction {
            prototype: prototype.clone(),
            body: None,
        });
        env.items.push_request(THIRFunctionGenRequest::Template {
            name,
            prototype: prototype.clone(),
            input: template_input,
        });
    }

    Ok(Some(symbol))
}

pub fn apply_template_input(
    env: &mut TypeEnvironment,
    prototype: &HIRTemplatePrototype,
    input: &THIRTemplateInput,
) -> CXRawResult<()> {
    for (param, arg) in prototype.types.iter().zip(input.args.iter()) {
        env.symbols.insert_local_type_id(param.as_string(), *arg)?;
    }

    Ok(())
}
