use cx_log::CXResult;
use cx_mir::{MIRBasicBlockID, MIRFunction, MIRUnit, expr::visit::successors};

use crate::{MIRAnalysisOptions, Pipeline};

pub struct AnalysisEnvironment<'mir> {
    #[allow(dead_code)]
    unit: &'mir MIRUnit<'mir>,
    function: &'mir MIRFunction,

    options: MIRAnalysisOptions,
}

impl<'mir> AnalysisEnvironment<'mir> {
    pub fn new(
        unit: &'mir MIRUnit,
        function: &'mir MIRFunction,
        options: MIRAnalysisOptions,
    ) -> Self {
        Self {
            unit,
            function,
            options,
        }
    }

    pub fn analyze(&mut self) -> CXResult<()> {
        let mut pipeline = Pipeline::new(self.options);

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
}

fn run(env: &AnalysisEnvironment<'_>, pipeline: &mut Pipeline) -> CXResult<()> {
    let Some(body) = env.function().body() else {
        unreachable!("Function body is missing for analysis");
    };

    let entry = body.entry().index();
    if entry >= body.blocks().len() {
        return Ok(());
    }

    let mut current_block = entry;
    let mut current_instruction = 0;

    // Blocks whose incoming state changed and still need (re)analysis; a block is queued at most once
    let mut reloads = Vec::new();
    let mut queued = vec![false; body.blocks().len()];

    pipeline.function_entry(env)?;
    pipeline.block_entry(env, body.entry())?;

    loop {
        let instruction = body
            .block(MIRBasicBlockID(current_block))
            .and_then(|b| b.instruction(current_instruction))
            .unwrap();

        pipeline.analyze_instruction(env, instruction)?;

        for successor in successors(instruction) {
            if pipeline.merge(env, successor.block, &instruction.token_range)?
                && !queued[successor.block.index()]
            {
                queued[successor.block.index()] = true;
                reloads.push(successor.block);
            }
        }

        if instruction.is_terminator()
            && let Some(next_target) = reloads.pop()
        {
            queued[next_target.index()] = false;
            current_block = next_target.index();
            current_instruction = 0;

            pipeline.reload_block(env, MIRBasicBlockID(current_block))?;
            pipeline.block_entry(env, MIRBasicBlockID(current_block))?;
        } else {
            current_instruction += 1;

            if current_instruction
                == body
                    .block(MIRBasicBlockID(current_block))
                    .unwrap()
                    .instructions()
                    .len()
            {
                break;
            }
        }
    }

    pipeline.finish(env)
}
