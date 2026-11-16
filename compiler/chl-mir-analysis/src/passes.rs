use crate::{MIRAnalysisOptions, framework::pipeline::Pipeline, passes::{liveness::Liveness, values::Values}};

mod liveness;
mod values;

pub fn register_passes(pipeline: &mut Pipeline, options: &MIRAnalysisOptions) {
    if options.liveness {
        pipeline.push(Liveness::new());
    }
    if options.values {
        pipeline.push(Values::new());
    }
}
