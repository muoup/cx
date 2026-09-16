use cx_log::CXResult;
use cx_mir::{MIRBasicBlockID, MIRFunction, MIRFunctionBody, MIRUnit};

use crate::{Pipeline, options::MIRAnalysisOptions};

pub struct AnalysisEnvironment<'mir> {
    unit: &'mir MIRUnit,
    function: &'mir MIRFunction,

    options: MIRAnalysisOptions,
}

impl AnalysisEnvironment<'_> {
    pub fn new(unit: &MIRUnit, function: &MIRFunction, options: MIRAnalysisOptions) -> Self {
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

    pub fn unit(&self) -> &MIRUnit {
        self.unit
    }

    pub fn function(&self) -> &MIRFunction {
        self.function
    }
}

fn run(
    env: &AnalysisEnvironment<'_>,
    pipeline: &mut Pipeline,
) -> CXResult<()> {
    let body = match env.function().body() {
        Some(MIRFunctionBody::Runtime(body)) => body,

        _ => return Ok(()),
    };

    let entry = body.entry().index();
    if entry >= body.blocks().len() {
        return Ok(());
    }

    let mut current_block = entry;
    let mut current_instruction = 0;

    let mut reloads = Vec::new();

    loop {
        let instruction = body
            .block(MIRBasicBlockID(current_block))
            .map(|b| b.instruction(current_instruction))
            .unwrap();

        pipeline.analyze_instruction(env, instruction)?;

        for successor in instruction.successors() {
            pipeline.merge(env, successor);
            reloads.push(successor);
        }

        if instruction.is_terminator()
            && let Some(next_target) = reloads.pop()
        {
            current_block = next_target.index();
            current_instruction = 0;

            pipeline.reload_block(env, MIRBasicBlockID(current_block));
        } else {
            current_instruction += 1;

            if current_instruction
                == body
                    .block(MIRBasicBlockID(current_block))
                    .unwrap()
                    .instrs()
                    .len()
            {
                break;
            }
        }
    }

    Ok(())
}
