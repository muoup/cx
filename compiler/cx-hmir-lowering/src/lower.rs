use crate::{
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
    },
    eval::{self, eval_type, flow, types::lower_type},
};
use cx_hmir::{
    HMIRDefKind, HMIRFunction, HMIRFunctionStage, HMIRGlobal, HMIRUnit,
    ty::context::HMIRTypeContext,
};
use cx_log::CXResult;
use cx_mir::{MIRGlobalID, MIRGlobalState, MIRGlobalVariable};
use cx_util::linkage::LinkageMode;

pub fn lower_unit<'hmir>(
    unit: &'hmir HMIRUnit,
    env: &mut HMIREnvironment<'_, 'hmir>,
) -> CXResult<()> {
    fn is_root(function: &HMIRFunction) -> bool {
        function.signature().stage() == HMIRFunctionStage::Runtime
            && !function
                .signature()
                .params()
                .iter()
                .any(|param| param.comptime())
    }

    // A declaration of storage defined elsewhere is lowered only once something refers to it
    fn is_declaration(global: &HMIRGlobal) -> bool {
        global.linkage() == LinkageMode::Extern && global.initializer().is_none()
    }

    for (_, def) in unit.defs() {
        match def.kind() {
            HMIRDefKind::Function(func) => {
                if func.def().is_none() || !is_root(func) {
                    continue;
                }

                lower_function(func, env)?;
            }

            HMIRDefKind::Global(global) => {
                if is_declaration(global) {
                    continue;
                }

                lower_global(global, env)?;
            }

            _ => {}
        }
    }

    Ok(())
}

pub fn lower_function<'hmir>(
    function: &'hmir HMIRFunction,
    env: &mut HMIREnvironment<'_, 'hmir>,
) -> CXResult<()> {
    let function = eval::lower_runtime_function(env, function)?;
    let id = env
        .module_mut()
        .declare_function(function.prototype().clone());
    env.module_mut().define_function(id, function);

    Ok(())
}

pub fn lower_global(global: &HMIRGlobal, env: &mut HMIREnvironment) -> CXResult<MIRGlobalID> {
    let mut context =
        LocalLoweringContext::new(env, None, FnContext::Comptime(ComptimeContext::new()));
    let span = global.body().expr(global.ty()).span();

    let ty = flow::top(eval_type(&mut context, global.body(), global.ty()))?;
    let mir_ty = lower_type(context.env_mut(), ty, span)?;
    let is_mutable = !context.env().resolve_type_id(ty).semantics().constant;

    let state = match (global.initializer(), global.linkage()) {
        (Some(_), _) => todo!("global initializers: comptime evaluation to the global's type"),
        (None, LinkageMode::Extern) => MIRGlobalState::External,
        (None, _) => MIRGlobalState::ZeroInitialized,
    };

    Ok(context
        .env_mut()
        .module_mut()
        .declare_global(MIRGlobalVariable::new(
            global.symbol_name().clone(),
            global.linkage(),
            mir_ty,
            state,
            is_mutable,
        )))
}
