use crate::builder::BytecodeBuilder;
use cx_bytecode_data::{
    bc_type::{BCFunctionPrototype, BCType, IntegerType},
    instruction::{BCAddress, BCInstruction, BCValue, BCValueCoercion},
};
use cx_mir_data::{
    BlockID, MIRFunction, MIRFunctionPrototype, MIRInstruction, MIRInstructionKind, MIRValue,
    types::{MIRType, MIRTypeKind},
};
use cx_util::{CXError, CXResult, identifier::CXIdent};

fn counter() -> u32 {
    use std::sync::atomic::{AtomicU32, Ordering};
    static COUNTER: AtomicU32 = AtomicU32::new(0);
    COUNTER.fetch_add(1, Ordering::SeqCst)
}

pub(crate) fn lower_mir_function<'a>(
    builder: &mut BytecodeBuilder<'a>,
    mir_function: &'a MIRFunction,
) -> CXResult<()> {
    let prototype = lower_mir_prototype(&mir_function.prototype)?;
    let param_names = (0..prototype.parameter_types.len())
        .map(|i| CXIdent::from(format!("param_{}", i)))
        .collect();

    builder.new_function(mir_function, prototype, param_names);

    for (block_idx, mir_block) in mir_function.blocks.iter().enumerate() {
        let block_id = BlockID::Block(block_idx as u32);

        let idx = builder.add_block(CXIdent::from(format!("block_{}", block_idx)));
        builder.set_block_pointer(idx);

        for (value_id, mir_instruction) in mir_block.body.iter().enumerate() {
            lower_mir_instruction(builder, block_id, value_id as u32, mir_instruction)?;
        }
    }

    builder.finish_function();

    Ok(())
}

pub(crate) fn lower_mir_instruction(
    builder: &mut BytecodeBuilder,
    block_id: BlockID,
    value_id: u32,
    mir_instruction: &MIRInstruction,
) -> CXResult<()> {
    let block_result = || lower_mir_block_result(block_id, value_id);

    match &mir_instruction.kind {
        MIRInstructionKind::Allocate { _type, alignment } => {
            let bc_type = lower_mir_type(_type)?;

            builder.add_instruction(BCInstruction::Allocate {
                value: block_result()?,
                type_: bc_type,
                alignment: *alignment,
            });
        }

        MIRInstructionKind::Store {
            memory,
            value,
            type_,
        } => {
            let bc_pointer = lower_mir_value(builder, memory)?;
            let bc_pointer = bc_pointer.get_address();

            let bc_value = lower_mir_value(builder, value)?;
            let bc_type = lower_mir_type(type_)?;

            builder.add_instruction(BCInstruction::Store {
                destination: bc_pointer.clone(),
                value: bc_value,
                store_type: bc_type,
            });
        }

        MIRInstructionKind::Branch {
            condition,
            true_block,
            false_block,
        } => {
            let bc_condition = lower_mir_value(builder, condition)?;
            let true_block = lower_mir_block(builder, *true_block);
            let false_block = lower_mir_block(builder, *false_block);

            builder.add_instruction(BCInstruction::Branch {
                condition: bc_condition,
                true_target: true_block,
                false_target: false_block,
            });
        }

        MIRInstructionKind::Jump { target } => {
            let target_block = lower_mir_block(builder, *target);

            builder.add_instruction(BCInstruction::Jump {
                target: target_block,
            });
        }

        MIRInstructionKind::Return { value } => {
            let bc_value = value
                .as_ref()
                .map(|value| lower_mir_value(builder, value))
                .transpose()?;

            builder.add_instruction(BCInstruction::Return { value: bc_value });
        }

        MIRInstructionKind::DirectCall { args, method_sig } => {
            let bc_args = args
                .iter()
                .map(|arg| lower_mir_value(builder, arg))
                .collect::<CXResult<Vec<BCValue>>>()?;

            let bc_method_sig = lower_mir_prototype(method_sig)?;

            builder.add_instruction(BCInstruction::CallDirect {
                destination: if method_sig.return_type.is_void() {
                    None
                } else {
                    Some(block_result()?)
                },
                function: bc_method_sig,
                arguments: bc_args,
            });
        }

        MIRInstructionKind::StructAccess {
            struct_,
            struct_type,
            field_index,
            ..
        } => {
            let bc_struct = lower_mir_value(builder, struct_)?;
            let bc_struct_type = lower_mir_type(struct_type)?;

            builder.add_instruction(BCInstruction::GetElementPtr {
                destination: block_result()?,
                base: bc_struct,
                index: BCValue::Integer {
                    value: *field_index as i64,
                    type_: IntegerType::I64,
                },
                offset: BCValue::Integer {
                    value: 0,
                    type_: IntegerType::I64,
                },
                structure_type: bc_struct_type,
            });
        }

        MIRInstructionKind::SExtend { value } => {
            let to_type = lower_mir_type(&mir_instruction.value_type)?
                .get_integer_type()
                .unwrap();
            let bc_value = lower_mir_value(builder, value)?;

            builder.add_instruction(BCInstruction::ValueCoercion {
                destination: block_result()?,
                value: bc_value,
                coercion: BCValueCoercion::SExtend { to: to_type },
            });
        }

        MIRInstructionKind::ZExtend { value } => {
            let to_type = lower_mir_type(&mir_instruction.value_type)?
                .get_integer_type()
                .unwrap();
            let bc_value = lower_mir_value(builder, value)?;

            builder.add_instruction(BCInstruction::ValueCoercion {
                destination: block_result()?,
                value: bc_value,
                coercion: BCValueCoercion::ZExtend { to: to_type },
            });
        }

        MIRInstructionKind::Trunc { value } => {
            let to_type = lower_mir_type(&mir_instruction.value_type)?
                .get_integer_type()
                .unwrap();
            let bc_value = lower_mir_value(builder, value)?;

            builder.add_instruction(BCInstruction::ValueCoercion {
                destination: block_result()?,
                value: bc_value,
                coercion: BCValueCoercion::Truncate { to: to_type },
            });
        }

        MIRInstructionKind::ZeroMemory { memory, _type } => {
            let bc_pointer = lower_mir_value(builder, memory)?;
            let bc_pointer = bc_pointer.get_address();
            let bc_type = lower_mir_type(_type)?;

            builder.add_instruction(BCInstruction::Memset {
                destination: bc_pointer.clone(),
                value: 0,
                _type: bc_type,
            });
        }
        
        MIRInstructionKind::IntegerBinOp { op, left, right } => {
            let bc_left = lower_mir_value(builder, left)?;
            let bc_right = lower_mir_value(builder, right)?;
            
            builder.add_instruction(BCInstruction::IntBinOp {
                destination: block_result()?,
                left: bc_left,
                right: bc_right,
                op: *op,
            });
        },
        
        MIRInstructionKind::FloatBinOp { op, left, right } => {
            let bc_left = lower_mir_value(builder, left)?;
            let bc_right = lower_mir_value(builder, right)?;
            
            builder.add_instruction(BCInstruction::FloatBinOp {
                destination: block_result()?,
                left: bc_left,
                right: bc_right,
                op: *op,
            });
        }
        
        MIRInstructionKind::IntegerUnOp { op, value } => {
            let bc_value = lower_mir_value(builder, value)?;
            
            builder.add_instruction(BCInstruction::IntUnOp {
                destination: block_result()?,
                value: bc_value,
                op: *op,
            });
        }
        
        MIRInstructionKind::FloatUnOp { op, value } => {
            let bc_value = lower_mir_value(builder, value)?;
            
            builder.add_instruction(BCInstruction::FloatUnOp {
                destination: block_result()?,
                value: bc_value,
                op: *op,
            });
        }
        
        MIRInstructionKind::PointerBinOp { op, ptr_type, left, right } => {
            let bc_left = lower_mir_value(builder, left)?;
            let bc_right = lower_mir_value(builder, right)?;
            let bc_ptr_type = lower_mir_type(ptr_type)?;
            
            builder.add_instruction(BCInstruction::PointerBinOp {
                destination: block_result()?,
                left: bc_left,
                right: bc_right,
                op: *op,
                ptr_type: bc_ptr_type,
            });
        }

        _ => todo!("MIR Instruction: {mir_instruction}"),
    }

    Ok(())
}

fn lower_mir_prototype(
    prototype: &MIRFunctionPrototype,
) -> CXResult<cx_bytecode_data::bc_type::BCFunctionPrototype> {
    let bc_params = prototype
        .params
        .iter()
        .map(|param| lower_mir_type(&param._type))
        .collect::<CXResult<Vec<BCType>>>()?;
    let bc_return_type = lower_mir_type(&prototype.return_type)?;

    Ok(BCFunctionPrototype {
        name: prototype.name.clone(),
        parameter_types: bc_params,
        return_type: bc_return_type,
        var_args: prototype.var_args,
        linkage: prototype.linkage.clone(),
    })
}

pub(crate) fn lower_mir_type(mir_type: &MIRType) -> CXResult<BCType> {
    match &mir_type.kind {
        MIRTypeKind::Unit => Ok(BCType::Unit),
        MIRTypeKind::Unsigned { bytes } | MIRTypeKind::Signed { bytes } => {
            Ok(BCType::Integer(match bytes {
                1 => IntegerType::I8,
                2 => IntegerType::I16,
                4 => IntegerType::I32,
                8 => IntegerType::I64,

                _ => return CXError::create_result("Unsupported integer byte size"),
            }))
        }
        MIRTypeKind::Float { bytes } => Ok(BCType::Float(match bytes {
            4 => cx_bytecode_data::bc_type::FloatType::F32,
            8 => cx_bytecode_data::bc_type::FloatType::F64,

            _ => return CXError::create_result("Unsupported float byte size"),
        })),
        MIRTypeKind::Array { element, size } => {
            let bc_element_type = lower_mir_type(element)?;

            Ok(BCType::Array {
                element: Box::new(bc_element_type),
                size: *size,
            })
        }
        MIRTypeKind::Pointer { .. } => Ok(BCType::Pointer {
            dereferenceable: 0,
            nonnull: false,
        }),

        MIRTypeKind::Bool => Ok(BCType::Bool),

        MIRTypeKind::Opaque { bytes } => Ok(BCType::Opaque { size: *bytes }),

        MIRTypeKind::Struct { name, fields } => {
            let mut bc_fields = Vec::new();
            for (field_name, field_type) in fields.iter() {
                let bc_field_type = lower_mir_type(field_type)?;
                bc_fields.push((field_name.clone(), bc_field_type));
            }
            Ok(BCType::Structured {
                name: name.clone(),
                fields: bc_fields,
            })
        }

        MIRTypeKind::Union { .. } => Ok(BCType::Opaque {
            size: mir_type.fixed_size(),
        }),

        MIRTypeKind::VariableSized { .. } => unimplemented!(),
    }
}

fn lower_mir_value(builder: &mut BytecodeBuilder, value: &MIRValue) -> CXResult<BCValue> {
    match value {
        MIRValue::ParameterRef(idx) => Ok(BCValue::Address(BCAddress::Local(
            builder.parameter_id(*idx as usize),
        ))),
        MIRValue::Global(id) => Ok(BCValue::Address(BCAddress::Global(CXIdent::from(format!(
            "global_{}",
            id
        ))))),
        MIRValue::FloatImmediate { type_, val } => {
            let bc_type = lower_mir_type(type_)?
                .get_float_type()
                .ok_or_else(|| CXError::create_boxed("Expected float type for FloatImmediate"))?;

            Ok(BCValue::Float {
                value: *val,
                type_: bc_type,
            })
        }
        MIRValue::IntImmediate { type_, val } => {
            let bc_type = lower_mir_type(type_)?
                .get_integer_type()
                .ok_or_else(|| CXError::create_boxed("Expected integer type for IntImmediate"))?;

            Ok(BCValue::Integer {
                value: *val,
                type_: bc_type,
            })
        }
        MIRValue::BlockResult { block_id, value_id } => {
            lower_mir_block_result(*block_id, *value_id).map(BCValue::Address)
        }

        MIRValue::FunctionRef(_) => todo!(),

        MIRValue::LoadOf(mirtype, mirvalue) => {
            let destination = BCAddress::Local(CXIdent::from(format!("{}_load", counter())));

            let source = lower_mir_value(builder, mirvalue)?;
            let load_type = lower_mir_type(mirtype)?;

            builder.add_instruction(BCInstruction::Load {
                destination: destination.clone(),
                source,
                load_type,
            });

            Ok(BCValue::Address(destination))
        }

        MIRValue::NULL => Ok(BCValue::NULL),
    }
}

fn lower_mir_block_result(block_id: BlockID, value_id: u32) -> CXResult<BCAddress> {
    Ok(BCAddress::Local(CXIdent::from(format!(
        "{block_id}_{value_id}"
    ))))
}

fn lower_mir_block(builder: &mut BytecodeBuilder, block_id: BlockID) -> CXIdent {
    CXIdent::from(format!(
        "block_{}",
        match block_id {
            BlockID::Block(id) => id as usize,
            BlockID::DeferredBlock(id) => id as usize + builder.defer_offset(),
        }
    ))
}
