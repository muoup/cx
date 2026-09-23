mod arithmetic;
mod engine;
mod execution;
mod log;

use cx_log::CXResult;
use cx_mir::{
    MIRComptimeBody, MIRComptimeFunction, MIRComptimeValue, MIRFunctionID, MIRGlobalID,
    MIRGlobalVariable, MIRStagedExpression, MIRStagedID, ty::interface::MTRegistry,
};

pub trait ComptimeContext<'thir> {
    type Registry: MTRegistry;

    fn function(&self, id: MIRFunctionID) -> Option<&MIRComptimeFunction<'thir>>;
    fn global(&self, id: MIRGlobalID) -> Option<&MIRGlobalVariable>;
    fn types(&self) -> &Self::Registry;
    fn add_staged_expression(&self, expression: MIRStagedExpression<'thir>) -> MIRStagedID;
}

pub use engine::EngineLimits;

pub fn evaluate_body<'c, 'thir, C: ComptimeContext<'thir>>(
    context: &'c C,
    body: &MIRComptimeBody<'thir>,
    args: &[MIRComptimeValue],
) -> CXResult<MIRComptimeValue> {
    engine::Engine::new(context).run(body, args)
}
