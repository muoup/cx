use crate::{MIRAnalysisOptions, framework::pipeline::Pipeline, passes::{ownership::Ownership, values::Values}};

mod ownership;
mod values;

pub fn register_passes(pipeline: &mut Pipeline, options: &MIRAnalysisOptions) {
    if options.ownership {
        pipeline.push(Ownership::new());
    }
    if options.values {
        pipeline.push(Values::new());
    }
}
