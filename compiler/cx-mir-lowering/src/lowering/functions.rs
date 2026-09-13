use std::collections::{HashMap, HashSet};

use cx_lmir::{
    LMIRBasicBlock, LMIRBlockParameter, LMIRFunction, LMIRFunctionMap, LMIRInstructionKind,
    LMIRParameterABI, LMIRValue,
};
use cx_log::CXResult;
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::registry::MIRTypeRegistry;
use cx_mir::{MIRBody, MIRFunction, MIRGlobalID, MIRPlace};

use crate::context::FunctionLoweringContext;

use super::instructions::lower_instruction;
use super::output::{
    allocate_temp, emit_void, lowered_type, mir_layout, offset_address, register_id,
};
use super::typing::{convert_prototype, convert_type};

pub(super) fn lower_function(
    unit: &cx_mir::MIRUnit,
    function: &MIRFunction,
    types: &MIRTypeRegistry,
    prototypes: &LMIRFunctionMap,
    global_indices: &HashMap<MIRGlobalID, u32>,
    globals: &mut Vec<cx_lmir::LMIRGlobalValue>,
) -> CXResult<LMIRFunction> {
    let definition = function
        .definition()
        .expect("MIR declaration cannot be lowered as a function definition");
    let (order, visited) = block_order(definition);
    let (blocks, block_indices) = lower_blocks(definition, types, &order);
    let prototype = convert_prototype(function.prototype(), types);
    let mut context = FunctionLoweringContext::new(
        unit,
        function,
        types,
        prototypes,
        global_indices,
        globals,
        prototype,
        blocks,
        block_indices,
    );

    lower_parameters(&mut context);
    for block_id_value in order {
        context.set_current(context.block_index(block_id_value));
        let instructions = definition
            .block(block_id_value)
            .expect("LMIR block has no MIR source")
            .instrs
            .clone();
        for instruction in instructions {
            lower_instruction(&mut context, &instruction.kind);
        }
    }
    for block in definition.blocks() {
        if !visited.contains(&block.id) {
            context.set_current(context.block_index(block.id));
            emit_void(&mut context, LMIRInstructionKind::Unreachable);
        }
    }

    Ok(context.finish())
}

fn block_order(
    definition: &MIRBody,
) -> (
    Vec<cx_mir::MIRBasicBlockID>,
    HashSet<cx_mir::MIRBasicBlockID>,
) {
    let mut order = Vec::new();
    let mut visited = HashSet::new();
    let mut pending = vec![(definition.entry(), false)];
    while let Some((id, expanded)) = pending.pop() {
        if expanded {
            order.push(id);
        } else if visited.insert(id) {
            pending.push((id, true));
            if let Some(block) = definition.block(id) {
                pending.extend(
                    block
                        .instrs
                        .iter()
                        .flat_map(|instruction| instruction.successors())
                        .map(|successor| (successor, false)),
                );
            }
        }
    }
    order.reverse();
    (order, visited)
}

fn lower_blocks(
    function: &MIRBody,
    types: &MIRTypeRegistry,
    reachable: &[cx_mir::MIRBasicBlockID],
) -> (Vec<LMIRBasicBlock>, HashMap<cx_mir::MIRBasicBlockID, usize>) {
    let mut order = reachable.to_vec();
    let reachable = reachable.iter().copied().collect::<HashSet<_>>();
    order.extend(
        function
            .blocks()
            .iter()
            .map(|block| block.id)
            .filter(|id| !reachable.contains(id)),
    );

    let mut blocks = Vec::with_capacity(order.len());
    let mut block_indices = HashMap::new();
    for block_id in order {
        let block = function.block(block_id).expect("invalid MIR block");
        block_indices.insert(block_id, blocks.len());
        blocks.push(LMIRBasicBlock {
            id: super::output::block_id(block_id),
            debug_name: block.debug_name.as_ref().map(ToString::to_string),
            params: block
                .params
                .iter()
                .map(|register| LMIRBlockParameter {
                    register: register_id(*register),
                    _type: convert_type(
                        function
                            .register(*register)
                            .expect("invalid block parameter")
                            .ty,
                        types,
                    ),
                })
                .collect(),
            body: Vec::new(),
        });
    }
    (blocks, block_indices)
}

fn lower_parameters(context: &mut FunctionLoweringContext<'_>) {
    let mut abi_index = usize::from(context.prototype().signature.has_indirect_return_param());
    for (index, parameter) in context
        .function()
        .prototype()
        .signature
        .params
        .iter()
        .enumerate()
    {
        let place = MIRPlace::Parameter(cx_mir::MIRParameterID::new(index));
        let lowered_type = lowered_type(context, parameter.ty);
        let layout = mir_layout(context, parameter.ty);
        let abi = context.prototype().signature.params[index].abi.clone();
        match abi {
            LMIRParameterABI::Direct { slots } if lowered_type.is_memory_resident() => {
                let address = allocate_temp(context, &lowered_type, layout.alignment as u8);
                for (slot_index, slot) in slots.iter().enumerate() {
                    let target = offset_address(context, address.clone(), slot.offset, &slot._type);
                    emit_void(
                        context,
                        LMIRInstructionKind::Store {
                            memory: target,
                            value: LMIRValue::ParameterRef((abi_index + slot_index) as u32),
                            _type: slot._type.clone(),
                        },
                    );
                }
                abi_index += slots.len();
                context.bind_place(
                    place,
                    crate::context::PlaceBinding::Address {
                        value: address,
                        ty: parameter.ty,
                    },
                );
            }
            LMIRParameterABI::Indirect { .. } | LMIRParameterABI::ByValue { .. } => {
                context.bind_place(
                    place,
                    crate::context::PlaceBinding::Address {
                        value: LMIRValue::ParameterRef(abi_index as u32),
                        ty: parameter.ty,
                    },
                );
                abi_index += 1;
            }
            LMIRParameterABI::Direct { slots }
                if context.types().is_reference_type(parameter.ty).unwrap() =>
            {
                debug_assert_eq!(slots.len(), 1);
                context.bind_place(
                    place,
                    crate::context::PlaceBinding::Address {
                        value: LMIRValue::ParameterRef(abi_index as u32),
                        ty: context
                            .types()
                            .reference_inner(parameter.ty)
                            .unwrap()
                            .expect("reference parameter is missing its pointee type"),
                    },
                );
                abi_index += slots.len();
            }
            LMIRParameterABI::Direct { slots } => {
                debug_assert_eq!(slots.len(), 1);
                let address = allocate_temp(context, &lowered_type, layout.alignment as u8);
                emit_void(
                    context,
                    LMIRInstructionKind::Store {
                        memory: address.clone(),
                        value: LMIRValue::ParameterRef(abi_index as u32),
                        _type: lowered_type,
                    },
                );
                abi_index += slots.len();
                context.bind_place(
                    place,
                    crate::context::PlaceBinding::Address {
                        value: address,
                        ty: parameter.ty,
                    },
                );
            }
        }
    }
}
