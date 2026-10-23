pub(crate) mod global;

use crate::{
    env::HMIREnvironment,
    eval,
    symbol::{self, CompletedFunction, CompletedSymbol},
};
use cx_hmir::{HMIRDefID, HMIRDefKind, HMIRFunction, HMIRUnit};
use cx_log::CXResult;

pub fn lower_unit<'hmir>(
    unit: &'hmir HMIRUnit,
    env: &mut HMIREnvironment<'_, 'hmir>,
) -> CXResult<()> {
    let mut defs = unit.defs().map(|(id, _)| *id).collect::<Vec<_>>();
    defs.sort();

    for id in defs {
        if symbol::validate(env, id)?.is_root() {
            env.generation_mut().request(id);
        }
    }

    while let Some(id) = env.generation_mut().next() {
        generate(id, env)?;
    }

    Ok(())
}

fn generate(id: HMIRDefID, env: &mut HMIREnvironment) -> CXResult<()> {
    let unit = env
        .symbols_mut()
        .owner(id.brand())
        .expect("symbol owner is available");
    let source = unit.resolve_def(id).expect("symbol exists");
    match symbol::complete(env, id)? {
        CompletedSymbol::Function(function) => {
            let HMIRDefKind::Function(source) =
                source.declarations()[function.declaration].kind().unwrap()
            else {
                unreachable!("a function symbol selects a function declaration")
            };
            if source.def().is_none() {
                return Ok(());
            }

            lower_function(source, id, &function, env)
        }
        CompletedSymbol::Global(global) => global::lower(env, id, &global),
        CompletedSymbol::StagedFunction => Ok(()),
        CompletedSymbol::Constant(_) | CompletedSymbol::Type(_) => {
            unreachable!("generation requested for a definition that is never emitted")
        }
    }
}

fn lower_function<'hmir>(
    source: &HMIRFunction,
    id: HMIRDefID,
    function: &CompletedFunction,
    env: &mut HMIREnvironment<'_, 'hmir>,
) -> CXResult<()> {
    let lowered = eval::lower_runtime_function(env, id, source, function)?;
    let id = env
        .module_mut()
        .declare_function(lowered.prototype().clone());
    env.module_mut().define_function(id, lowered);

    Ok(())
}
