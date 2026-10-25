use crate::lowering::memory;
use cx_lmir::types::LMIRIntegerType;
use cx_lmir::{
    LMIRBasicBlock, LMIRFunction, LMIRInstructionKind, LMIRParameterABI, LMIRRegister, LMIRValue,
};
use cx_log::CXResult;
use cx_mir::ty::layout::calculate_type_layout;
use cx_mir::{MIRBody, MIRFunction};

use crate::context::{FunctionContext, GlobalContext, block_id};

use super::instructions::lower_instruction;
use super::typing::convert_prototype;

pub(super) fn lower_function<'mir>(
    global: &mut GlobalContext<'mir>,
    function: &'mir MIRFunction,
    body: &'mir MIRBody,
) -> CXResult<LMIRFunction> {
    let prototype = convert_prototype(function.prototype(), global.unit.types());
    let mut context = FunctionContext::new(global, function, body, prototype);
    
    for (id, block) in body.blocks() {
        let index = context.blocks.len();
        context.block_indices.insert(id, index);
        context.blocks.push(LMIRBasicBlock {
            id: block_id(id),
            debug_name: block.debug_name().cloned(),
            params: Vec::new(),
            body: Vec::new(),
        });
    }
    
    context.current_block = context.block_indices[&body.entry()];
    for (id, place) in body.places() {
        let address = if place.adopted() {
            LMIRValue::Register {
                register: LMIRRegister::new(format!("mir.place.{}", id.index())),
                ty: context.pointer(),
            }
        } else {
            memory::allocate(&mut context, place.ty())
        };
        context.places.insert(id, address);
    }
    lower_parameters(&mut context);
    
    for (id, block) in body.blocks() {
        context.current_block = context.block_indices[&id];
        for instruction in block.instructions() {
            lower_instruction(&mut context, instruction);
        }
    }
    Ok(context.finish())
}

fn lower_parameters(context: &mut FunctionContext<'_, '_>) {
    let mut abi_index = u32::from(context.prototype.signature.has_indirect_return_param());
    for ((place, decl), param) in context
        .body
        .parameters()
        .zip(context.prototype.signature.params.clone())
    {
        let address = context.places[&place].clone();
        match param.abi {
            LMIRParameterABI::Direct { slots } => {
                for slot in slots {
                    let destination = memory::offset(context, address.clone(), slot.offset as i64);
                    memory::void(
                        context,
                        LMIRInstructionKind::Store {
                            memory: destination,
                            value: LMIRValue::ParameterRef(abi_index),
                            ty: slot.ty,
                        },
                    );
                    abi_index += 1;
                }
            }
            LMIRParameterABI::Indirect { alignment } | LMIRParameterABI::ByValue { alignment } => {
                let ty = decl.ty();
                let size = calculate_type_layout(context.types(), ty).size();
                memory::void(
                    context,
                    LMIRInstructionKind::Memcpy {
                        dest: address,
                        src: LMIRValue::ParameterRef(abi_index),
                        size: context.integer(size as i128, LMIRIntegerType::I64),
                        alignment,
                    },
                );
                abi_index += 1;
            }
        }
    }
}
