pub(crate) mod global;

pub use global::lower as lower_global;

use crate::{env::HMIREnvironment, eval};
use cx_hmir::{HMIRDef, HMIRDefID, HMIRDefKind, HMIRFunction, HMIRFunctionStage, HMIRUnit};
use cx_log::CXResult;
use cx_util::linkage::LinkageMode;

pub fn lower_unit<'hmir>(
    unit: &'hmir HMIRUnit,
    env: &mut HMIREnvironment<'_, 'hmir>,
) -> CXResult<()> {
    let mut roots = unit
        .defs()
        .filter(|(_, def)| is_root(def))
        .map(|(id, _)| *id)
        .collect::<Vec<_>>();
    roots.sort();

    for root in roots {
        env.generation_mut().request(root);
    }

    while let Some(id) = env.generation_mut().next() {
        generate(id, env)?;
    }

    Ok(())
}

// What other units may link against is generated regardless of use; everything else waits for a reference
fn is_root(def: &HMIRDef) -> bool {
    match def.kind() {
        HMIRDefKind::Function(function) => {
            function.def().is_some() && function.prototype().linkage() != LinkageMode::Static
        }
        HMIRDefKind::Global(global) => match global.linkage() {
            LinkageMode::Standard => true,
            LinkageMode::Extern => global.initializer().is_some(),
            LinkageMode::Static => false,
        },
        HMIRDefKind::ComptimeGlobal(_) | HMIRDefKind::Type(_) => false,
    }
}

pub(crate) fn is_staged(function: &HMIRFunction) -> bool {
    function.signature().stage() == HMIRFunctionStage::Comptime
        || function
            .signature()
            .params()
            .iter()
            .any(|param| param.comptime())
}

fn generate<'hmir>(id: HMIRDefID, env: &mut HMIREnvironment<'_, 'hmir>) -> CXResult<()> {
    let def = env
        .symbols()
        .local_unit()
        .resolve_def(id)
        .expect("Local symbol not found");

    match def.kind() {
        HMIRDefKind::Function(function) => {
            if function.def().is_none() || is_staged(function) {
                return Ok(());
            }

            lower_function(function, env)
        }
        HMIRDefKind::Global(global) => lower_global(global, env).map(|_| ()),
        HMIRDefKind::ComptimeGlobal(_) | HMIRDefKind::Type(_) => {
            unreachable!("generation requested for a definition that is never emitted")
        }
    }
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
