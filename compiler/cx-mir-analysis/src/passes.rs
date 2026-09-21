use crate::{
    framework::pipeline::Pipeline, options::MIRAnalysisOptions, passes::ownership::Ownership,
};

mod ownership;
mod value;

pub fn register_passes(pipeline: &mut Pipeline, config: &MIRAnalysisOptions) {
    pipeline.push(Ownership::new());
    pipeline.push(ValueTracking::new());
}
