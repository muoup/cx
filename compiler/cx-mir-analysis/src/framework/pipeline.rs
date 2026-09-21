use cx_log::CXResult;
use cx_mir::{MIRBasicBlockID, MIRInstruction};

use crate::MIRAnalysisOptions;
use crate::framework::environment::AnalysisEnvironment;
use crate::passes::register_passes;

pub struct Pipeline {
    analyses: Vec<Box<dyn AnalysisPass>>,
}

impl Pipeline {
    pub fn new(options: MIRAnalysisOptions) -> Self {
        let mut pipeline = Self {
            analyses: Vec::new(),
        };

        register_passes(&mut pipeline, &options);

        pipeline
    }

    pub fn push<A: AnalysisPass + 'static>(&mut self, analysis: A) {
        self.analyses.push(Box::new(analysis));
    }

    pub fn analyze_instruction(
        &mut self,
        _env: &AnalysisEnvironment,
        _instruction: &MIRInstruction,
    ) -> CXResult<()> {
        todo!()
    }

    pub fn merge(&mut self, env: &AnalysisEnvironment, other: MIRBasicBlockID) {
        for analysis in &mut self.analyses {
            analysis.merge(env, other);
        }
    }

    pub fn reload_block(&mut self, env: &AnalysisEnvironment, block: MIRBasicBlockID) {
        for analysis in &mut self.analyses {
            analysis.reload_block(env, block);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.analyses.is_empty()
    }
}

pub trait AnalysisPass {
    fn function_entry(
        &mut self,
        env: &AnalysisEnvironment,
    ) -> CXResult<()>;
    
    fn analyze_instruction(
        &mut self,
        env: &AnalysisEnvironment,
        instruction: &MIRInstruction,
    ) -> CXResult<()>;

    fn merge(&mut self, env: &AnalysisEnvironment, other: MIRBasicBlockID) -> CXResult<()>;

    fn reload_block(&mut self, env: &AnalysisEnvironment, block: MIRBasicBlockID) -> CXResult<()>;
}
