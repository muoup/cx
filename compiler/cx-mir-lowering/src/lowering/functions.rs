use std::collections::{HashMap, HashSet};

use cx_lmir::{
    LMIRBasicBlock, LMIRBlockParameter, LMIRFunction, LMIRFunctionMap, LMIRGlobalValue,
    LMIRInstructionKind, LMIRParameterABI, LMIRValue,
};
use cx_log::CXResult;
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::registry::MIRTypeRegistry;
use cx_mir::visit::MIRWalk;
use cx_mir::{MIRBody, MIRFnPrototype};

use crate::context::{LMIRFunctionContext, LMIRGlobalContext};

use super::instructions::lower_instruction;
use super::typing::{convert_prototype, convert_type};

pub(super) fn lower_function<'context>(
    context: &'context mut LMIRGlobalContext<'context>,
    prototype: &MIRFnPrototype,
    body: &MIRBody,
) -> CXResult<LMIRFunction> {
    let prototype = convert_prototype(prototype, context.types());
    let mut context = LMIRFunctionContext::new(context, prototype);

    for block in body.blocks() {
        let params = block
            .params
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

        context.push_block(params, block.debug_name.clone(), Some(block.id));
    }

    lower_parameters(&mut context);

    for block in body.blocks() {
        context.set_current(context.block_index(block.id));

        for instruction in &block.instrs {
            lower_instruction(&mut context, instruction);
        }
    }

    Ok(context.finish())
}

fn lower_parameters(context: &mut LMIRFunctionContext<'_>) {
    
}
