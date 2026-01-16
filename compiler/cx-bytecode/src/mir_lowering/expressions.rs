//! Expression tree lowering from MIRExpression to bytecode
//!
//! This module handles lowering of MIRExpression (AST-style IR) to bytecode,
//! replacing the old instruction-based lowering which worked with SSA-style MIR.

use std::rc::Rc;

use cx_bytecode_data::{
    types::{BCIntegerType, BCType},
    BCInstructionKind, BCValue,
};
use cx_typechecker_data::mir::{
    expression::{
        MIRExpression, MIRExpressionKind, MIRUnOp, MIRCoercion, MIRBinOp,
    },
    program::MIRFunction,
    types::MIRType,
};
use cx_util::identifier::CXIdent;
use cx_util::CXResult;

use crate::builder::BCBuilder;

/// Main entry point for expression lowering
///
/// Lowers a MIRExpression tree to a bytecode value, generating SSA registers
/// for intermediate results as needed.
pub fn lower_expression(
    builder: &mut BCBuilder,
    expr: &MIRExpression,
) -> CXResult<BCValue> {
    let expr_type = builder.convert_cx_type(&expr._type);

    match &expr.kind {
        // ===== Literals =====
        MIRExpressionKind::BoolLiteral(value) => {
            Ok(BCValue::IntImmediate {
                val: if *value { 1 } else { 0 },
                _type: BCIntegerType::I1,
            })
        }

        MIRExpressionKind::IntLiteral(val, _type, _signed) => {
            let bc_type = builder.convert_integer_type(_type);
            Ok(BCValue::IntImmediate {
                val: *val,
                _type: bc_type,
            })
        }

        MIRExpressionKind::FloatLiteral(val, _type) => {
            let bc_type = builder.convert_float_type(_type);
            Ok(BCValue::FloatImmediate {
                val: *val,
                _type: bc_type,
            })
        }

        MIRExpressionKind::Unit => Ok(BCValue::NULL),
        MIRExpressionKind::Null => Ok(BCValue::NULL),

        // ===== Variables =====
        MIRExpressionKind::Parameter(name) => {
            let index = builder
                .current_prototype()
                .params
                .iter()
                .position(|param| {
                    param
                        .name
                        .as_ref()
                        .map(|p_name| p_name.as_str() == name.as_str())
                        .unwrap_or(false)
                })
                .expect("Parameter not found in function prototype");

            Ok(BCValue::ParameterRef(index as u32))
        }

        MIRExpressionKind::LocalVariable(name) | MIRExpressionKind::GlobalVariable(name) => {
            // For local variables, look up in symbol table
            // For global variables, look up in global symbol table
            if let Some(local_value) = builder.get_symbol(name) {
                return Ok(local_value);
            }

            if let Some(global_value) = builder.get_global_symbol(name.as_str()) {
                return Ok(global_value);
            }

            panic!("Variable '{}' not found in symbol table", name);
        }

        MIRExpressionKind::FunctionReference { .. } => {
            // Function references are handled in CallFunction lowering
            todo!("FunctionReference lowering - to be implemented")
        }

        // ===== Arithmetic & Logic =====
        MIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
            lower_binary_op(builder, lhs, rhs, op, &expr._type)
        }

        MIRExpressionKind::UnaryOperation { operand, op } => {
            lower_unary_op(builder, operand, op, &expr._type)
        }

        // ===== Memory Operations =====
        MIRExpressionKind::MemoryRead { source } => {
            let bc_source = lower_expression(builder, source)?;
            let bc_type = builder.convert_cx_type(&source.get_type());

            builder.add_new_instruction(
                BCInstructionKind::Load {
                    memory: bc_source,
                    _type: bc_type,
                },
                builder.convert_cx_type(&expr._type),
                true,
            )
        }

        MIRExpressionKind::StackAllocation { _type } => {
            let bc_type = builder.convert_cx_type(_type);

            builder.add_new_instruction(
                BCInstructionKind::Allocate {
                    alignment: bc_type.alignment(),
                    _type: bc_type,
                },
                BCType::default_pointer(),
                true,
            )
        }

        MIRExpressionKind::MemoryWrite { target, value } => {
            let bc_target = lower_expression(builder, target)?;
            let bc_value = lower_expression(builder, value)?;
            let bc_type = builder.get_value_type(&bc_value);

            builder.add_new_instruction(
                BCInstructionKind::Store {
                    memory: bc_target,
                    value: bc_value,
                    _type: bc_type,
                },
                BCType::unit(),
                false,
            )
        }

        MIRExpressionKind::Move { source } => {
            // Move is essentially an alias - just return the source expression
            lower_expression(builder, source)
        }

        MIRExpressionKind::CopyRegion { source, _type } => {
            // For now, treat as memory read
            // TODO: Implement proper memcpy for CopyRegion
            lower_expression(builder, source)
        }

        // ===== Control Flow =====
        MIRExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => lower_if(builder, condition, then_branch, else_branch.as_deref(), &expr._type),

        MIRExpressionKind::While {
            condition,
            body,
            pre_eval,
        } => lower_while(builder, condition, body, *pre_eval),

        MIRExpressionKind::Return { value } => lower_return(builder, value.as_deref()),

        MIRExpressionKind::Block { statements } => lower_block(builder, statements),

        // ===== Function Calls =====
        MIRExpressionKind::CallFunction { function, arguments } => {
            lower_call_function(builder, function, arguments)
        }

        // ===== Type Conversion =====
        MIRExpressionKind::TypeConversion { operand, conversion } => {
            lower_type_conversion(builder, operand, *conversion, &expr._type)
        }

        // ===== Lifetime Management =====
        // Stubbed for now - will be implemented later
        MIRExpressionKind::LifetimeStart { variable, _type } => {
            let _ = variable;
            let _ = _type;
            todo!("LifetimeStart lowering - to be implemented")
        }

        MIRExpressionKind::LifetimeEnd { variable, _type } => {
            let _ = variable;
            let _ = _type;
            todo!("LifetimeEnd lowering - to be implemented")
        }

        MIRExpressionKind::LeakLifetime { expression } => {
            let _ = expression;
            todo!("LeakLifetime lowering - to be implemented")
        }

        // ===== Not yet implemented =====
        MIRExpressionKind::StructFieldAccess { .. } => {
            todo!("StructFieldAccess lowering - to be implemented")
        }

        MIRExpressionKind::UnionAliasAccess { .. } => {
            todo!("UnionAliasAccess lowering - to be implemented")
        }

        MIRExpressionKind::ArrayAccess { .. } => {
            todo!("ArrayAccess lowering - to be implemented")
        }

        MIRExpressionKind::TaggedUnionTag { .. } => {
            todo!("TaggedUnionTag lowering - to be implemented")
        }

        MIRExpressionKind::TaggedUnionGet { .. } => {
            todo!("TaggedUnionGet lowering - to be implemented")
        }

        MIRExpressionKind::ConstructTaggedUnion { .. } => {
            todo!("ConstructTaggedUnion lowering - to be implemented")
        }

        MIRExpressionKind::ArrayInitializer { .. } => {
            todo!("ArrayInitializer lowering - to be implemented")
        }

        MIRExpressionKind::StructInitializer { .. } => {
            todo!("StructInitializer lowering - to be implemented")
        }

        MIRExpressionKind::Break => {
            todo!("Break lowering - to be implemented")
        }

        MIRExpressionKind::Continue => {
            todo!("Continue lowering - to be implemented")
        }

        MIRExpressionKind::For { .. } => {
            todo!("For lowering - to be implemented")
        }

        MIRExpressionKind::CSwitch { .. } => {
            todo!("CSwitch lowering - to be implemented")
        }

        MIRExpressionKind::Match { .. } => {
            todo!("Match lowering - to be implemented")
        }

        MIRExpressionKind::Defer { .. } => {
            todo!("Defer lowering - to be implemented")
        }
    }
}

/// Lower a binary operation
fn lower_binary_op(
    builder: &mut BCBuilder,
    lhs: &MIRExpression,
    rhs: &MIRExpression,
    op: &MIRBinOp,
    result_type: &MIRType,
) -> CXResult<BCValue> {
    let bc_lhs = lower_expression(builder, lhs)?;
    let bc_rhs = lower_expression(builder, rhs)?;
    let bc_result_type = builder.convert_cx_type(result_type);

    let instruction_kind = match op {
        MIRBinOp::Integer { op, .. } => {
            let bc_op = convert_int_binop(op);
            BCInstructionKind::IntegerBinOp {
                op: bc_op,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        MIRBinOp::Float { op, .. } => {
            let bc_op = convert_float_binop(op);
            BCInstructionKind::FloatBinOp {
                op: bc_op,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        MIRBinOp::PtrDiff { .. } | MIRBinOp::Pointer { .. } => {
            todo!("Pointer binary operations - to be implemented")
        }
    };

    builder.add_new_instruction(instruction_kind, bc_result_type, true)
}

/// Lower a unary operation
fn lower_unary_op(
    builder: &mut BCBuilder,
    operand: &MIRExpression,
    op: &MIRUnOp,
    result_type: &MIRType,
) -> CXResult<BCValue> {
    let bc_operand = lower_expression(builder, operand)?;
    let bc_result_type = builder.convert_cx_type(result_type);

    let instruction_kind = match op {
        MIRUnOp::LNOT => BCInstructionKind::IntegerUnOp {
            value: bc_operand,
            op: cx_bytecode_data::BCIntUnOp::LNOT,
        },
        MIRUnOp::BNOT => BCInstructionKind::IntegerUnOp {
            value: bc_operand,
            op: cx_bytecode_data::BCIntUnOp::BNOT,
        },
        MIRUnOp::NEG => {
            // Integer negation: subtract from zero
            let zero = BCValue::IntImmediate {
                val: 0,
                _type: match &bc_result_type.kind {
                    cx_bytecode_data::types::BCTypeKind::Integer(itype) => *itype,
                    _ => panic!("Integer negation requires integer type"),
                },
            };
            BCInstructionKind::IntegerBinOp {
                op: cx_bytecode_data::BCIntBinOp::SUB,
                left: zero,
                right: bc_operand,
            }
        }
        MIRUnOp::FNEG => {
            // Float negation - for now, use placeholder
            // TODO: Implement proper float negation
            todo!("Float negation (FNEG) - to be implemented")
        }
        MIRUnOp::INEG => {
            // Signed integer negation
            let zero = BCValue::IntImmediate {
                val: 0,
                _type: match &bc_result_type.kind {
                    cx_bytecode_data::types::BCTypeKind::Integer(itype) => *itype,
                    _ => panic!("Integer negation requires integer type"),
                },
            };
            BCInstructionKind::IntegerBinOp {
                op: cx_bytecode_data::BCIntBinOp::SUB,
                left: zero,
                right: bc_operand,
            }
        }
    };

    builder.add_new_instruction(instruction_kind, bc_result_type, true)
}

/// Lower an if expression
fn lower_if(
    builder: &mut BCBuilder,
    condition: &MIRExpression,
    then_branch: &MIRExpression,
    else_branch: Option<&MIRExpression>,
    result_type: &MIRType,
) -> CXResult<BCValue> {
    let bc_condition = lower_expression(builder, condition)?;

    // Create basic blocks
    let then_block_id = CXIdent::from(format!("then_{}", builder.block_count()));
    let else_block_id = CXIdent::from(format!("else_{}", builder.block_count()));
    let merge_block_id = CXIdent::from(format!("merge_{}", builder.block_count()));

    let bc_result_type = builder.convert_cx_type(result_type);

    // Create blocks
    builder.create_block(then_block_id.clone());
    builder.create_block(else_block_id.clone());
    builder.create_block(merge_block_id.clone());

    // Emit branch
    builder.add_new_instruction(
        BCInstructionKind::Branch {
            condition: bc_condition,
            true_block: then_block_id.clone(),
            false_block: else_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    // Lower then branch
    builder.set_current_block(then_block_id.clone());
    let bc_then_value = lower_expression(builder, then_branch)?;
    let result_reg_then = if !result_type.is_unit() {
        // Store result in a temporary
        let temp = builder.add_new_instruction(
            BCInstructionKind::Alias { value: bc_then_value },
            bc_result_type.clone(),
            true,
        )?;
        Some(temp)
    } else {
        None
    };
    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: merge_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    // Lower else branch
    builder.set_current_block(else_block_id.clone());
    let bc_else_value = if let Some(else_branch) = else_branch {
        lower_expression(builder, else_branch)?
    } else {
        BCValue::NULL
    };
    let result_reg_else = if !result_type.is_unit() {
        let temp = builder.add_new_instruction(
            BCInstructionKind::Alias { value: bc_else_value },
            bc_result_type.clone(),
            true,
        )?;
        Some(temp)
    } else {
        None
    };
    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: merge_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    // Merge block
    builder.set_current_block(merge_block_id);

    // If we have a result, we need to merge the two branches
    // For now, just return NULL if unit, or the then value (simplified)
    // TODO: Implement proper phi merging
    Ok(result_reg_then.unwrap_or(BCValue::NULL))
}

/// Lower a while loop
fn lower_while(
    builder: &mut BCBuilder,
    condition: &MIRExpression,
    body: &MIRExpression,
    pre_eval: bool,
) -> CXResult<BCValue> {
    let condition_block_id = CXIdent::from(format!("while_cond_{}", builder.block_count()));
    let body_block_id = CXIdent::from(format!("while_body_{}", builder.block_count()));
    let exit_block_id = CXIdent::from(format!("while_exit_{}", builder.block_count()));

    // Create blocks
    builder.create_block(condition_block_id.clone());
    builder.create_block(body_block_id.clone());
    builder.create_block(exit_block_id.clone());

    // Jump to condition or body based on pre_eval
    if pre_eval {
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: condition_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    } else {
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: body_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    }

    // Condition block
    builder.set_current_block(condition_block_id.clone());
    let bc_condition = lower_expression(builder, condition)?;
    builder.add_new_instruction(
        BCInstructionKind::Branch {
            condition: bc_condition,
            true_block: body_block_id.clone(),
            false_block: exit_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    // Body block
    builder.set_current_block(body_block_id.clone());
    lower_expression(builder, body)?;
    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: condition_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    // Exit block
    builder.set_current_block(exit_block_id);

    Ok(BCValue::NULL)
}

/// Lower a return statement
fn lower_return(
    builder: &mut BCBuilder,
    value: Option<&MIRExpression>,
) -> CXResult<BCValue> {
    let bc_value = match value {
        Some(v) => Some(lower_expression(builder, v)?),
        None => None,
    };

    builder.add_new_instruction(
        BCInstructionKind::Return { value: bc_value },
        BCType::unit(),
        false,
    )
}

/// Lower a block expression
fn lower_block(
    builder: &mut BCBuilder,
    statements: &[MIRExpression],
) -> CXResult<BCValue> {
    let mut last_value = BCValue::NULL;

    for stmt in statements {
        last_value = lower_expression(builder, stmt)?;
    }

    Ok(last_value)
}

/// Lower a function call
fn lower_call_function(
    builder: &mut BCBuilder,
    function: &MIRExpression,
    arguments: &[MIRExpression],
) -> CXResult<BCValue> {
    // Lower arguments
    let mut bc_args = Vec::new();
    for arg in arguments {
        bc_args.push(lower_expression(builder, arg)?);
    }

    // For now, assume direct function call
    // TODO: Handle indirect calls through function expressions
    match &function.kind {
        MIRExpressionKind::FunctionReference { .. } => {
            // This should have been resolved during typechecking
            // For now, just return NULL
            todo!("FunctionReference lowering in CallFunction")
        }
        _ => {
            todo!("Indirect function calls - to be implemented")
        }
    }
}

/// Lower a type conversion
fn lower_type_conversion(
    builder: &mut BCBuilder,
    operand: &MIRExpression,
    conversion: MIRCoercion,
    result_type: &MIRType,
) -> CXResult<BCValue> {
    let bc_operand = lower_expression(builder, operand)?;
    let bc_result_type = builder.convert_cx_type(result_type);

    let coercion_type = convert_coercion(&conversion, &bc_operand, builder)?;

    builder.add_new_instruction(
        BCInstructionKind::Coercion {
            coercion_type,
            value: bc_operand,
        },
        bc_result_type,
        true,
    )
}

// ===== Helper functions =====

use cx_typechecker_data::mir::expression::{MIRIntegerBinOp, MIRFloatBinOp};

fn convert_int_binop(op: &MIRIntegerBinOp) -> cx_bytecode_data::BCIntBinOp {
    match op {
        MIRIntegerBinOp::ADD => cx_bytecode_data::BCIntBinOp::ADD,
        MIRIntegerBinOp::SUB => cx_bytecode_data::BCIntBinOp::SUB,
        MIRIntegerBinOp::MUL => cx_bytecode_data::BCIntBinOp::MUL,
        MIRIntegerBinOp::IMUL => cx_bytecode_data::BCIntBinOp::IMUL,
        MIRIntegerBinOp::DIV => cx_bytecode_data::BCIntBinOp::UDIV,
        MIRIntegerBinOp::IDIV => cx_bytecode_data::BCIntBinOp::IDIV,
        MIRIntegerBinOp::MOD => cx_bytecode_data::BCIntBinOp::UREM,
        MIRIntegerBinOp::IMOD => cx_bytecode_data::BCIntBinOp::IREM,
        MIRIntegerBinOp::EQ => cx_bytecode_data::BCIntBinOp::EQ,
        MIRIntegerBinOp::NE => cx_bytecode_data::BCIntBinOp::NE,
        MIRIntegerBinOp::LT => cx_bytecode_data::BCIntBinOp::ULT,
        MIRIntegerBinOp::LE => cx_bytecode_data::BCIntBinOp::ULE,
        MIRIntegerBinOp::GT => cx_bytecode_data::BCIntBinOp::UGT,
        MIRIntegerBinOp::GE => cx_bytecode_data::BCIntBinOp::UGE,
        MIRIntegerBinOp::ILT => cx_bytecode_data::BCIntBinOp::ILT,
        MIRIntegerBinOp::ILE => cx_bytecode_data::BCIntBinOp::ILE,
        MIRIntegerBinOp::IGT => cx_bytecode_data::BCIntBinOp::IGT,
        MIRIntegerBinOp::IGE => cx_bytecode_data::BCIntBinOp::IGE,
        MIRIntegerBinOp::LAND => cx_bytecode_data::BCIntBinOp::LAND,
        MIRIntegerBinOp::LOR => cx_bytecode_data::BCIntBinOp::LOR,
        MIRIntegerBinOp::BAND => cx_bytecode_data::BCIntBinOp::BAND,
        MIRIntegerBinOp::BOR => cx_bytecode_data::BCIntBinOp::BOR,
        MIRIntegerBinOp::BXOR => cx_bytecode_data::BCIntBinOp::BXOR,
    }
}

fn convert_float_binop(op: &MIRFloatBinOp) -> cx_bytecode_data::BCFloatBinOp {
    match op {
        MIRFloatBinOp::FADD => cx_bytecode_data::BCFloatBinOp::ADD,
        MIRFloatBinOp::FSUB => cx_bytecode_data::BCFloatBinOp::SUB,
        MIRFloatBinOp::FMUL => cx_bytecode_data::BCFloatBinOp::FMUL,
        MIRFloatBinOp::FDIV => cx_bytecode_data::BCFloatBinOp::FDIV,
        MIRFloatBinOp::EQ => cx_bytecode_data::BCFloatBinOp::EQ,
        MIRFloatBinOp::NEQ => cx_bytecode_data::BCFloatBinOp::NEQ,
        MIRFloatBinOp::FLT => cx_bytecode_data::BCFloatBinOp::FLT,
        MIRFloatBinOp::FLE => cx_bytecode_data::BCFloatBinOp::FLE,
        MIRFloatBinOp::FGT => cx_bytecode_data::BCFloatBinOp::FGT,
        MIRFloatBinOp::FGE => cx_bytecode_data::BCFloatBinOp::FGE,
    }
}

use cx_bytecode_data::BCCoercionType;

fn convert_coercion(
    coercion: &MIRCoercion,
    _operand: &BCValue,
    _builder: &BCBuilder,
) -> CXResult<BCCoercionType> {
    match coercion {
        MIRCoercion::ReinterpretBits => Ok(BCCoercionType::BitCast),
        MIRCoercion::IntToBool => {
            // Int to bool conversion - check if non-zero
            // For now, treat as truncation to i1
            // TODO: Implement proper int-to-bool comparison
            Ok(BCCoercionType::Trunc)
        }
        MIRCoercion::Integral { sextend, .. } => {
            if *sextend {
                Ok(BCCoercionType::SExtend)
            } else {
                Ok(BCCoercionType::ZExtend)
            }
        }
        MIRCoercion::FloatCast { .. } => {
            // For float casts, we need more info
            // TODO: Properly implement float casts
            todo!("FloatCast coercion - to be implemented")
        }
        MIRCoercion::IntToFloat { .. } => {
            todo!("IntToFloat coercion - to be implemented")
        }
        MIRCoercion::PtrToInt { .. } => Ok(BCCoercionType::PtrToInt),
        MIRCoercion::IntToPtr { .. } => {
            todo!("IntToPtr coercion - to be implemented")
        }
        MIRCoercion::FloatToInt { .. } => {
            todo!("FloatToInt coercion - to be implemented")
        }
    }
}

/// Generate bytecode for a MIR function
///
/// This is the main entry point for lowering a complete function from MIR to bytecode.
pub fn lower_function(
    builder: &mut BCBuilder,
    mir_fn: &MIRFunction,
) -> CXResult<()> {
    // Create the BCFunction
    let prototype = builder.convert_cx_prototype(&mir_fn.prototype);
    builder.new_function(prototype);

    // Create entry block
    let entry_block = CXIdent::from("entry");
    builder.create_block(entry_block.clone());
    builder.set_current_block(entry_block);

    // Lower the function body (expression tree)
    let result = lower_expression(builder, &mir_fn.body)?;

    // If return type is not Unit, ensure the function returns the value
    if !mir_fn.prototype.return_type.is_unit() {
        builder.add_new_instruction(
            BCInstructionKind::Return { value: Some(result) },
            BCType::unit(),
            false,
        )?;
    } else {
        builder.add_new_instruction(
            BCInstructionKind::Return { value: None },
            BCType::unit(),
            false,
        )?;
    }

    builder.finish_function();

    Ok(())
}
