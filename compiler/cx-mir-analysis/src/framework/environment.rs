use cx_log::CXResult;
use cx_mir::{MIRBody, MIRFunction, MIRUnit, expr::visit::successors};

use crate::{MIRAnalysisOptions, Pipeline};

pub struct AnalysisEnvironment<'mir> {
    #[allow(dead_code)]
    unit: &'mir MIRUnit<'mir>,
    function: &'mir MIRFunction,
    body: &'mir MIRBody,

    options: MIRAnalysisOptions,
}

impl<'mir> AnalysisEnvironment<'mir> {
    pub fn new(
        unit: &'mir MIRUnit,
        function: &'mir MIRFunction,
        body: &'mir MIRBody,
        options: MIRAnalysisOptions,
    ) -> Self {
        Self {
            unit,
            function,
            body,
            options,
        }
    }

    pub fn analyze(&mut self) -> CXResult<()> {
        let mut pipeline = Pipeline::new(self.options);
        pipeline.retain_applicable(self);

        if pipeline.is_empty() {
            return Ok(());
        }

        run(self, &mut pipeline)?;

        Ok(())
    }

    #[allow(dead_code)]
    pub fn unit(&self) -> &MIRUnit<'_> {
        self.unit
    }

    pub fn function(&self) -> &MIRFunction {
        self.function
    }

    pub fn body(&self) -> &MIRBody {
        self.body
    }
}

fn run(env: &AnalysisEnvironment<'_>, pipeline: &mut Pipeline) -> CXResult<()> {
    let body = env.body();
    if body.block(body.entry()).is_none() {
        return Ok(());
    }

    let mut worklist = Vec::new();
    let mut queued = vec![false; body.blocks().len()];

    pipeline.function_entry(env)?;

    let mut current = Some(body.entry());
    while let Some(block_id) = current {
        pipeline.block_entry(env, block_id)?;

        for instruction in body.block(block_id).unwrap().instructions() {
            pipeline.analyze_instruction(env, instruction)?;

            for successor in successors(instruction) {
                let target = successor.block;
                if pipeline.merge(env, target, &instruction.token_range)? && !queued[target.index()]
                {
                    queued[target.index()] = true;
                    worklist.push(target);
                }
            }
        }

        current = worklist.pop();
        if let Some(next) = current {
            queued[next.index()] = false;
            pipeline.reload_block(env, next)?;
        }
    }

    pipeline.finish(env)
}
