use crate::{
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
    },
    eval::{self, eval_type, flow, types::lower_type},
};
use cx_hmir::{HMIRDefKind, HMIRFunction, HMIRFunctionStage, HMIRGlobal, HMIRUnit};
use cx_log::CXResult;
use cx_mir::MIRGlobalVariable;

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

    for (_, def) in unit.defs() {
        match def.kind() {
            HMIRDefKind::Function(func) => {
                if func.def().is_none() || !is_root(func) {
                    continue;
                }

                lower_function(func, env)?;
            }

            HMIRDefKind::Global(global) => {
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

pub fn lower_global(global: &HMIRGlobal, env: &mut HMIREnvironment) -> CXResult<()> {
    let mut context =
        LocalLoweringContext::new(env, None, FnContext::Comptime(ComptimeContext::new()));
    
    let ty = flow::top(eval_type(&mut context, global.body(), global.ty()))?;
    let mir_ty = flow::top(lower_type(context.env_mut(), ty, &TokenRange::internal()))?;

    // let global = MIRGlobalVariable::new(
    //     global.symbol_name(),
    //     mir_ty,
    // );

    todo!()
}
