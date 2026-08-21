use cx_mir::{
    MIRBasicBlockID, MIRBlockTarget, MIRConstant, MIRDiagnosticLocation, MIRFunctionDefinition,
    MIRFunctionID,
};

use crate::{
    control_flow, engine::MIRComptimeEngine, error, frame::ExecutionFrame, instructions,
    value::RuntimeValue,
};

pub(crate) enum Step {
    Continue,
    Jump(MIRBasicBlockID),
    Return(MIRConstant),
}

pub(crate) fn run(
    engine: &MIRComptimeEngine<'_>,
    function: MIRFunctionID,
    definition: &MIRFunctionDefinition,
    entry: MIRBasicBlockID,
    arguments: &[MIRConstant],
) -> Result<MIRConstant, error::MIRComptimeError> {
    let mut frame = ExecutionFrame::new(arguments);
    let return_type = engine
        .unit()
        .function(function)
        .expect("the evaluated MIR function is still present")
        .prototype()
        .signature
        .return_type;
    let mut block_id = entry;

    'blocks: loop {
        let block = definition.block(block_id).ok_or_else(|| {
            engine.internal(
                format!("comptime function {function} has an invalid block {block_id}"),
                format!("block {block_id}"),
            )
        })?;
        let mut next_block = None;

        for (instruction_index, instruction) in block.instrs.iter().enumerate() {
            let location = MIRDiagnosticLocation::Instruction {
                function,
                block: block_id,
                instruction: instruction_index,
            };
            match instructions::execute(
                engine,
                &mut frame,
                definition,
                &instruction.kind,
                return_type,
                &location,
            )? {
                Step::Continue => {}
                Step::Jump(block) => {
                    next_block = Some(block);
                    break;
                }
                Step::Return(value) => return Ok(value),
            }
        }

        if let Some(next_block) = next_block {
            block_id = next_block;
            continue 'blocks;
        }
        return Err(engine.internal(
            format!("comptime function {function} has no terminator in block {block_id}"),
            format!("block {block_id}"),
        ));
    }
}

pub(crate) fn bind_target(
    engine: &MIRComptimeEngine<'_>,
    frame: &mut ExecutionFrame,
    definition: &MIRFunctionDefinition,
    target: &MIRBlockTarget,
    location: &MIRDiagnosticLocation,
) -> Result<MIRBasicBlockID, error::MIRComptimeError> {
    let block = definition
        .block(target.block)
        .ok_or_else(|| engine.error("branch target is not a valid MIR block", location.clone()))?;
    if block.params.len() != target.args.len() {
        return Err(engine.error(
            format!(
                "branch target expects {} arguments, found {}",
                block.params.len(),
                target.args.len()
            ),
            location.clone(),
        ));
    }
    let values = control_flow::target_arguments(target)
        .map(|value| engine.eval_value(frame, value, None, location))
        .collect::<Result<Vec<RuntimeValue>, _>>()?;
    for (register, value) in block.params.iter().zip(values) {
        frame.set_register(*register, value);
    }
    Ok(target.block)
}
