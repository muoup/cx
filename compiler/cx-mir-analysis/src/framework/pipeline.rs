use cx_log::CXResult;
use cx_mir::{MIRBasicBlockID, MIRInstruction};
use cx_tokens::TokenRange;

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
        env: &AnalysisEnvironment,
        instruction: &MIRInstruction,
    ) -> CXResult<()> {
        for analysis in &mut self.analyses {
            analysis.analyze_instruction(env, instruction)?;
        }

        Ok(())
    }

    pub fn function_entry(&mut self, env: &AnalysisEnvironment) -> CXResult<()> {
        for analysis in &mut self.analyses {
            analysis.function_entry(env)?;
        }
        Ok(())
    }

    pub fn block_entry(
        &mut self,
        env: &AnalysisEnvironment,
        block: MIRBasicBlockID,
    ) -> CXResult<()> {
        for analysis in &mut self.analyses {
            analysis.block_entry(env, block)?;
        }
        Ok(())
    }

    pub fn merge(
        &mut self,
        env: &AnalysisEnvironment,
        other: MIRBasicBlockID,
        range: &TokenRange,
    ) -> CXResult<bool> {
        let mut changed = false;

        for analysis in &mut self.analyses {
            changed |= analysis.merge(env, other, range)?;
        }

        Ok(changed)
    }

    pub fn reload_block(
        &mut self,
        env: &AnalysisEnvironment,
        block: MIRBasicBlockID,
    ) -> CXResult<()> {
        for analysis in &mut self.analyses {
            analysis.reload_block(env, block)?;
        }

        Ok(())
    }

    pub fn is_empty(&self) -> bool {
        self.analyses.is_empty()
    }

    pub fn finish(&mut self, env: &AnalysisEnvironment) -> CXResult<()> {
        for analysis in &mut self.analyses {
            analysis.finish(env)?;
        }
        Ok(())
    }
}

pub trait AnalysisPass {
    fn function_entry(&mut self, env: &AnalysisEnvironment) -> CXResult<()>;

    fn finish(&mut self, _: &AnalysisEnvironment) -> CXResult<()> {
        Ok(())
    }

    fn block_entry(&mut self, _env: &AnalysisEnvironment, _block: MIRBasicBlockID) -> CXResult<()> {
        Ok(())
    }

    fn analyze_instruction(
        &mut self,
        env: &AnalysisEnvironment,
        instruction: &MIRInstruction,
    ) -> CXResult<()>;

    fn merge(
        &mut self,
        env: &AnalysisEnvironment,
        other: MIRBasicBlockID,
        range: &TokenRange,
    ) -> CXResult<bool>;

    fn reload_block(&mut self, env: &AnalysisEnvironment, block: MIRBasicBlockID) -> CXResult<()>;
}
