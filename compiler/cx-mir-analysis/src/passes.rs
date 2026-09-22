use crate::{MIRAnalysisOptions, framework::pipeline::Pipeline, passes::ownership::Ownership};

mod ownership;
mod value;

pub fn register_passes(pipeline: &mut Pipeline, _: &MIRAnalysisOptions) {
    pipeline.push(Ownership::new());
    // pipeline.push(ValueTracking::new());
}
