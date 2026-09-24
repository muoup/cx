use crate::lowering::memory;
use cx_lmir::{
    LMIRBasicBlock, LMIRBlockParameter, LMIRFunction, LMIRInstructionKind, LMIRParameterABI,
    LMIRValue,
};
use cx_log::CXResult;
use cx_mir::ty::layout::calculate_type_layout;
use cx_mir::{MIRBody, MIRFunction};
use cx_util::identifier::CXIdent;

use crate::context::{FunctionContext, GlobalContext};

use super::instructions::lower_instruction;
use super::typing::convert_prototype;

pub(super) fn lower_function<'mir>(
    global: &mut GlobalContext<'mir>,
    function: &'mir MIRFunction,
    body: &'mir MIRBody,
) -> CXResult<LMIRFunction> {
    let prototype = convert_prototype(function.prototype(), global.unit.types());
    let mut context = FunctionContext::new(global, function, body, prototype);
    for block in body.blocks() {
        let params = block
            .params()
            .iter()
            .map(|id| {
                let LMIRValue::Register { register, _type } = context.reg(*id) else {
                    unreachable!()
                };
                LMIRBlockParameter { register, _type }
            })
            .collect();
        let index = context.blocks.len();
        context.block_indices.insert(block.id(), index);
        context.blocks.push(LMIRBasicBlock {
            id: CXIdent::new(format!("block.{}", block.id().index())),
            debug_name: block.debug_name().cloned(),
            params,
            body: Vec::new(),
        });
    }
    context.current_block = context.block_indices[&body.entry()];
    for (index, place) in body.places().iter().enumerate() {
        let address = memory::allocate(&mut context, place.ty);
        context
            .places
            .insert(cx_mir::MIRPlaceID::new(index), address);
    }
    lower_parameters(&mut context);
    for block in body.blocks() {
        context.current_block = context.block_indices[&block.id()];
        for instruction in block.instructions() {
            lower_instruction(&mut context, instruction);
        }
    }
    Ok(context.finish())
}

fn lower_parameters(context: &mut FunctionContext<'_, '_>) {
    let mut abi_index = u32::from(context.prototype.signature.has_indirect_return_param());
    for (place, param) in context
        .body
        .parameters()
        .iter()
        .zip(context.prototype.signature.params.clone())
    {
        let address = context.places[place].clone();
        match param.abi {
            LMIRParameterABI::Direct { slots } => {
                for slot in slots {
                    let destination = memory::offset(context, address.clone(), slot.offset as i64);
                    memory::void(
                        context,
                        LMIRInstructionKind::Store {
                            memory: destination,
                            value: LMIRValue::ParameterRef(abi_index),
                            _type: slot._type,
                        },
                    );
                    abi_index += 1;
                }
            }
            LMIRParameterABI::Indirect { alignment } | LMIRParameterABI::ByValue { alignment } => {
                let ty = context
                    .body
                    .place(*place)
                    .expect("invalid MIR parameter")
                    .ty;
                let size = calculate_type_layout(context.types(), ty).size();
                memory::void(
                    context,
                    LMIRInstructionKind::Memcpy {
                        dest: address,
                        src: LMIRValue::ParameterRef(abi_index),
                        size: context.integer(size as i128, cx_lmir::types::LMIRIntegerType::I64),
                        alignment,
                    },
                );
                abi_index += 1;
            }
        }
    }
}
