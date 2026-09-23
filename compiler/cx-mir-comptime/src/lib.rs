mod arithmetic;
mod engine;
mod log;

use cx_log::CXResult;
use cx_mir::{
    MIRComptimeBody, MIRComptimeFunction, MIRConstant, MIRFunctionID, MIRGlobalID,
    MIRGlobalVariable, ty::interface::MTRegistry,
};

pub trait ComptimeContext {
    type Registry: MTRegistry;

    fn function(&self, id: MIRFunctionID) -> Option<&MIRComptimeFunction<'_>>;
    fn global(&self, id: MIRGlobalID) -> Option<&MIRGlobalVariable>;
    fn types(&self) -> &Self::Registry;
}

pub use engine::EngineLimits;

pub fn evaluate_body<C: ComptimeContext>(
    context: &C,
    body: &MIRComptimeBody<'_>,
    args: &[MIRConstant],
) -> CXResult<MIRConstant> {
    engine::Engine::new(context).run(body, args)
}
