use std::rc::Rc;

use cx_hmir::{
    HMIRDeclaration, HMIRDef, HMIRDefID, HMIRDefKind, HMIRUnitID,
    ty::{HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_util::linkage::LinkageMode;

use crate::{
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
    },
    eval::{eval_comptime, eval_type, flow, types},
    log::hmir_error,
    symbol::{
        CompletedFunction, CompletedGlobal, CompletedSymbol, Shape,
        composite::{alike, composite, signatures_alike},
        structure::incompatible,
    },
};

pub(crate) fn complete(
    env: &mut HMIREnvironment,
    id: HMIRDefID,
    def: &HMIRDef,
    shape: &Shape,
) -> CXResult<CompletedSymbol> {
    let declarations = def.declarations();
    match *shape {
        Shape::Function { staged: true, .. } => Ok(CompletedSymbol::StagedFunction),
        Shape::Function {
            declaration,
            linkage,
            ..
        } => function(env, id, def, declaration, linkage),
        Shape::Global {
            declaration,
            linkage,
            ..
        } => global(env, id, def, declaration, linkage),
        Shape::Alias => alias(env, id, def),
        Shape::Constant(declaration) => constant(env, id, &declarations[declaration]),
        Shape::Type(declaration) => Ok(CompletedSymbol::Type(type_definition(
            env,
            id,
            &declarations[declaration],
        )?)),
    }
}

pub(crate) fn type_definition(
    env: &mut HMIREnvironment,
    id: HMIRDefID,
    declaration: &HMIRDeclaration,
) -> CXResult<HMIRTypeID> {
    match declaration.kind() {
        Some(HMIRDefKind::TypeDef(definition)) => {
            let mut context = owner(env, id.brand());
            let ty = eval_type(&mut context, definition.body(), definition.initializer())?;
            flow::top(ty.description())
        }
        Some(HMIRDefKind::Type(ty)) => {
            let ty = env.import_type(id.brand(), *ty);
            env.resolver_mut().mark_incomplete(ty);
            env.resolver_mut().publish_type(ty);
            Ok(ty)
        }
        _ => unreachable!("a type symbol declares a type"),
    }
}

fn function(
    env: &mut HMIREnvironment,
    id: HMIRDefID,
    def: &HMIRDef,
    selected: usize,
    linkage: LinkageMode,
) -> CXResult<CompletedSymbol> {
    let declarations = def.declarations();
    let mut signatures = Vec::with_capacity(declarations.len());
    for declaration in declarations {
        let Some(HMIRDefKind::Function(function)) = declaration.kind() else {
            unreachable!("every declaration of a function declares a function")
        };
        let mut context = owner(env, id.brand());
        let signature = types::eval_signature(&mut context, function.body(), function.signature());
        signatures.push(flow::top(signature)?);
    }
    for (declaration, signature) in declarations.iter().zip(&signatures) {
        if !signatures_alike(env, signature, &signatures[0]) {
            return Err(incompatible(def, declaration));
        }
    }

    let declaration = &declarations[selected];
    let Some(HMIRDefKind::Function(source)) = declaration.kind() else {
        unreachable!("every declaration of a function declares a function")
    };
    let signature = signatures.swap_remove(selected);
    let ty = env.resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::Function(signature.clone()),
        HMIRTypeSemantics::default(),
        declaration.span().clone(),
        None,
    ));

    Ok(CompletedSymbol::Function(Rc::new(CompletedFunction {
        declaration: selected,
        symbol_name: source.prototype().symbol_name().clone(),
        linkage,
        signature,
        ty,
    })))
}

fn global(
    env: &mut HMIREnvironment,
    id: HMIRDefID,
    def: &HMIRDef,
    selected: usize,
    linkage: LinkageMode,
) -> CXResult<CompletedSymbol> {
    let declarations = def.declarations();
    let mut canonical = None;
    for declaration in declarations {
        let Some(HMIRDefKind::Global(global)) = declaration.kind() else {
            unreachable!("every declaration of a global declares a global")
        };
        let mut context = owner(env, id.brand());
        let ty = flow::top(eval_type(&mut context, global.body(), global.ty())?.description())?;
        canonical = match canonical {
            None => Some(ty),
            Some(current) => match composite(env, current, ty) {
                Some(ty) => Some(ty),
                None => {
                    return Err(incompatible(def, declaration));
                }
            },
        };
    }
    let ty = canonical.expect("a global has a declaration");

    let Some(HMIRDefKind::Global(source)) = declarations[selected].kind() else {
        unreachable!("every declaration of a global declares a global")
    };
    let external = source.initializer().is_none() && linkage == LinkageMode::Extern;
    if !external && !types::is_complete(env, ty) {
        let span = source.body().expr(source.ty()).span();
        let subject = format!("global of type '{}'", types::name(env, ty));
        return Err(hmir_error(span, &typecheck::INCOMPLETE_TYPE, subject));
    }

    Ok(CompletedSymbol::Global(Rc::new(CompletedGlobal {
        declaration: selected,
        symbol_name: source.symbol_name().clone(),
        linkage,
        ty,
    })))
}

fn alias(env: &mut HMIREnvironment, id: HMIRDefID, def: &HMIRDef) -> CXResult<CompletedSymbol> {
    let (first, rest) = def
        .declarations()
        .split_first()
        .expect("an alias has a declaration");
    let ty = type_definition(env, id, first)?;
    for declaration in rest {
        let other = type_definition(env, id, declaration)?;
        if !alike(env, ty, other, true) {
            return Err(incompatible(def, declaration));
        }
    }

    Ok(CompletedSymbol::Type(ty))
}

fn constant(
    env: &mut HMIREnvironment,
    id: HMIRDefID,
    declaration: &HMIRDeclaration,
) -> CXResult<CompletedSymbol> {
    let Some(HMIRDefKind::ComptimeGlobal(global)) = declaration.kind() else {
        unreachable!("a constant symbol declares a comptime global")
    };
    let mut context = owner(env, id.brand());
    let ty = flow::top(eval_type(&mut context, global.body(), global.ty())?.description())?;
    let value = eval_comptime(&mut context, global.body(), global.initializer(), Some(ty))?;

    Ok(CompletedSymbol::Constant(value))
}

fn owner<'e, 'global, 'hmir>(
    env: &'e mut HMIREnvironment<'global, 'hmir>,
    source: HMIRUnitID,
) -> LocalLoweringContext<'e, 'global, 'hmir> {
    LocalLoweringContext::new(env, None, FnContext::Comptime(ComptimeContext::new()))
        .sourced(source)
}
