use cx_ast::{
    ast::global_var::CXGlobalVariable,
    symbols::{UntypedSymbol, UntypedSymbolKind},
};
use cx_util::{CXError, CXResult, namespace::QualifiedName};

use crate::{
    mir::{
        data::MIRTemplateInput,
        expression::{MIRExpression, MIRExpressionKind, SymbolValueOrigin},
        r#type::MIRTypeKind,
    },
    registry::MIRSymbolRegistry,
    symbol::{
        MIRSymbol,
        completion::{complete_prototype, complete_type},
    },
    type_context::MIRTypeContext,
};

pub fn resolve_symbol(
    env: &mut MIRSymbolRegistry,
    name: &QualifiedName,
    symbol: &UntypedSymbol,
) -> CXResult<MIRSymbol> {
    match &symbol.kind {
        UntypedSymbolKind::Type(ty) => {
            let mut completed = complete_type(env, &name.namespace, ty)?;
            if completed.strong_identifier.is_none() {
                completed.strong_identifier = Some(name.clone());
            }
            if completed.debug_name.is_none() {
                completed.debug_name = Some(name.name.clone());
            }
            let id = env.generate_type_id(completed);
            Ok(MIRSymbol::Type(id))
        }

        UntypedSymbolKind::Function(prototype) => {
            let prototype = complete_prototype(env, &name.namespace, None, prototype)?;
            Ok(MIRSymbol::PureValue(
                crate::mir::expression::MIRPureExpression::FunctionReference(Box::new(prototype)),
            ))
        }

        UntypedSymbolKind::Global(global) => resolve_global_symbol(env, name, global),

        UntypedSymbolKind::TypeTemplate { input, definition } => {
            let source = UntypedSymbol::new(
                symbol.visibility,
                UntypedSymbolKind::Type(definition.clone()),
            );
            Ok(MIRSymbol::Template {
                input: input.clone(),
                name: name.clone(),
                source: Box::new(source),
                namespace: name.namespace.clone(),
            })
        }

        UntypedSymbolKind::FunctionTemplate {
            input, definition, ..
        } => {
            let source = UntypedSymbol::new(
                symbol.visibility,
                UntypedSymbolKind::Function(definition.clone()),
            );
            Ok(MIRSymbol::Template {
                input: input.clone(),
                name: name.clone(),
                source: Box::new(source),
                namespace: name.namespace.clone(),
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

    let result = resolve_symbol(env, name, source);
    env.pop_scope();

    let mut symbol = result?;
    attach_template_metadata(env, &mut symbol, name, namespace, template_input);
    Ok(Some(symbol))
}

fn resolve_global_symbol(
    env: &mut MIRSymbolRegistry,
    name: &QualifiedName,
    global: &CXGlobalVariable,
) -> CXResult<MIRSymbol> {
    match global {
        CXGlobalVariable::Standard { _type, .. } => {
            let ty = complete_type(env, &name.namespace, _type)?;
            let mem_ty = env.mem_ref_to(ty);
            Ok(MIRSymbol::Value(MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::Variable {
                    name: name.name.clone(),
                    location: SymbolValueOrigin::Global,
                },
                _type: mem_ty,
            }))
        }
        CXGlobalVariable::EnumDefinition { .. } => Ok(MIRSymbol::Value(MIRExpression {
            token_range: None,
            kind: MIRExpressionKind::Unit,
            _type: MIRTypeKind::Unit.into(),
        })),
    }
}

fn attach_template_metadata(
    env: &mut MIRSymbolRegistry,
    symbol: &mut MIRSymbol,
    name: &QualifiedName,
    _namespace: &crate::program::EnvironmentNamespace,
    input: MIRTemplateInput,
) {
    let MIRSymbol::Type(id) = symbol else {
        return;
    };

    let mut ty = env.resolve_type_id(*id).clone();
    ty.template_info = Some(Box::new(crate::mir::data::TemplateInfo {
        base_name: name.name.clone(),
        template_input: input,
    }));
    ty.strong_identifier = Some(name.clone());
    ty.debug_name.get_or_insert_with(|| name.name.clone());
    env.overwrite_type_id(*id, ty);
}
