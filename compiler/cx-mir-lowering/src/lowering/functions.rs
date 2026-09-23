use cx_lmir::{
    LMIRBlockParameter, LMIRFunction
};
use cx_log::CXResult;
use cx_mir::{MIRBody, MIRFunction};

use crate::context::{LMIRFunctionContext, LMIRGlobalContext};

use super::instructions::lower_instruction;
use super::typing::{convert_prototype, convert_type};

pub(super) fn lower_function<'context>(
    context: &'context mut LMIRGlobalContext<'context>,
    function: &'context MIRFunction,
    body: &MIRBody,
) -> CXResult<LMIRFunction> {
    let prototype = convert_prototype(function.prototype(), context.types());
    let mut context = LMIRFunctionContext::new(context, function, prototype);

    for block in body.blocks() {
        let params = block
            .params()
            .iter()
            .map(|param| {
                let register = context.lower_register(param);
                let _type = body
                    .register(*param)
                    .map(|reg| convert_type(reg.ty, context.global().types()))
                    .expect("LMIR block parameter has no MIR source");

                LMIRBlockParameter { register, _type }
            })
            .collect();

        context.push_block(params, block.debug_name().cloned(), Some(block.id()));
    }

    for block in body.blocks() {
        context.set_current(context.block_index(block.id()));

        for instruction in block.instructions() {
            lower_instruction(&mut context, instruction);
        }
    }

    Ok(context.finish())
}
