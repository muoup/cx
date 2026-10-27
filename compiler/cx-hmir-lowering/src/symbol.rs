pub(crate) mod composite;
mod interface;
mod structure;

use cx_hmir::{
    HMIRDeclarationRole, HMIRDefID, HMIRDefKind, HMIRFnSignature, HMIRFunctionStage,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    unit::def::HMIRDeclarationPoint,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
        memo::{MemoContextID, MemoKey},
    },
    eval::flow,
    log::hmir_error,
    value::HMIRValue,
};

pub(crate) use interface::{definition, initialize};
pub(crate) use structure::{needs_specialization, select, visible};

pub(crate) fn resolve(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    scope: MemoContextID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let id = context
        .env_mut()
        .resolver_mut()
        .reserve(MemoKey::Definition(def, scope));
    flow::top(context.evaluate(id, span))
}

pub(crate) fn binding(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    point: Option<HMIRDeclarationPoint>,
) -> CXResult<HMIRValue> {
    let mut context =
        LocalLoweringContext::new(env, None, FnContext::Comptime(ComptimeContext::new()))
            .sourced(def.brand())
            .at(point);
    let scope = context.memo_context(point, None, false);
    let unit = context
        .env_mut()
        .symbols_mut()
        .owner(def.brand())
        .expect("definition owner exists");
    let span = unit.resolve_def(def).unwrap().declarations()[0].span();
    resolve(&mut context, def, scope, span)
}

pub(crate) fn validate(env: &mut HMIREnvironment, id: HMIRDefID) -> CXResult<()> {
    let unit = env
        .symbols_mut()
        .owner(id.brand())
        .expect("definition owner exists");
    let def = unit.resolve_def(id).unwrap();
    let selected = select(def, None)?;
    let declaration = &def.declarations()[selected];
    let reconcile_global =
        def.declarations().len() > 1 && matches!(declaration.kind(), Some(HMIRDefKind::Global(_)));
    if let Some(HMIRDefKind::Function(function)) = declaration.kind()
        && function.def().is_some()
        && function.signature().is_variadic()
        && function.signature().contract().is_safe()
    {
        return Err(hmir_error(
            declaration.span(),
            &typecheck::INVALID_CONTEXT,
            ("Varargs".into(), "a safe function".into()),
        ));
    }
    let linkage = structure::reconcile_linkage(def, None)?;
    let root = linkage != LinkageMode::Static
        && match declaration.kind() {
            Some(HMIRDefKind::Function(function)) => {
                function.signature().stage() == HMIRFunctionStage::Runtime
                    && !needs_specialization(function)
                    && function.def().is_some()
            }
            Some(HMIRDefKind::Global(global)) => {
                global.initializer().is_some() || linkage != LinkageMode::Extern
            }
            _ => false,
        };
    if root {
        env.generation_mut().request(id);
    }
    let untagged_comptime = declaration.tag().is_none()
        && matches!(declaration.kind(), Some(HMIRDefKind::ComptimeGlobal(_)));
    if untagged_comptime && declaration.role() == HMIRDeclarationRole::Definition {
        binding(env, id, None)?;
    }
    if reconcile_global {
        binding(env, id, None)?;
    }
    Ok(())
}

pub(crate) fn value_type(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    point: Option<HMIRDeclarationPoint>,
) -> CXResult<HMIRTypeID> {
    let value = binding(env, def, point)?;
    Ok(env.referent(value.ty()))
}

pub(crate) fn signature(
    env: &mut HMIREnvironment,
    id: HMIRDefID,
    point: Option<HMIRDeclarationPoint>,
) -> CXResult<HMIRFnSignature<HMIRTypeID>> {
    let value = binding(env, id, point)?;
    let HMIRTypeKind::Function(signature) = env.resolve_kind(value.ty()) else {
        unreachable!("a function binding has a function type")
    };
    Ok(signature.clone())
}

pub(crate) fn linkage(
    env: &mut HMIREnvironment,
    id: HMIRDefID,
    point: Option<HMIRDeclarationPoint>,
) -> CXResult<LinkageMode> {
    let unit = env
        .symbols_mut()
        .owner(id.brand())
        .expect("definition owner exists");
    structure::reconcile_linkage(unit.resolve_def(id).unwrap(), point)
}

pub(crate) fn link_name(
    env: &HMIREnvironment,
    id: HMIRDefID,
    point: Option<HMIRDeclarationPoint>,
) -> CXResult<Option<CXIdent>> {
    let definition = env.symbols().resolve_definition(id);
    let selected = select(definition, point)?;
    Ok(match definition.declarations()[selected].kind() {
        Some(HMIRDefKind::Function(function)) => Some(function.prototype().symbol_name().clone()),
        Some(HMIRDefKind::Global(global)) => Some(global.symbol_name().clone()),
        _ => None,
    })
}
