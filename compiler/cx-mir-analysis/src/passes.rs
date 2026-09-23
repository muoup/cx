use crate::{MIRAnalysisOptions, framework::pipeline::Pipeline, passes::ownership::Ownership};

mod ownership;

pub fn register_passes(pipeline: &mut Pipeline, options: &MIRAnalysisOptions) {
    if options.ownership {
        pipeline.push(Ownership::new());
    }
}
