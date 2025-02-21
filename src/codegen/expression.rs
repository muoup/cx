use std::clone;
use crate::codegen::codegen::FunctionState;
use crate::codegen::routines::{signed_bin_op, stack_alloca, string_literal};
use crate::lex::token::OperatorType;
use crate::parse::ast::{ControlExpression, Expression, IntegerCastType, LValueExpression, LiteralExpression, ValueExpression};
use cranelift::codegen::ir;
use cranelift::prelude::{EntityRef, Imm64, InstBuilder, Value};
use cranelift_module::{Linkage, Module};
use crate::codegen::value_type::get_cranelift_type;

pub(crate) fn codegen_expression(context: &mut FunctionState, expr: &Expression) -> Option<Value> {
    match expr {
        Expression::Value(value) => codegen_value_expr(context, value),
        Expression::Control(control) => codegen_control_expr(context, control),
        Expression::Literal(literal) => codegen_literal_expr(context, literal),
        Expression::LValue(lvalue) => codegen_lvalue_expr(context, lvalue),
        Expression::Unit => None,
        Expression::Unverified(_) => panic!("Unverified expression encountered {:#?}", expr),

        _ => unimplemented!("Expression not implemented: {:#?}", expr)
    }
}

pub(crate) fn codegen_value_expr(context: &mut FunctionState, expr: &ValueExpression) -> Option<Value> {
    match expr {
        ValueExpression::DirectFunctionCall { name, args } => {
            let id = context.functions
                .get(name.as_str())
                .expect(format!("Function not found: {}", name.as_str()).as_str());
            let sig = context.object_module
                .declarations()
                .get_function_decl(*id).signature.clone();

            let call = context.object_module
                .declare_function(name.as_str(), Linkage::Export, &sig)
                .unwrap();
            let local_call = context.object_module.declare_func_in_func(call, context.builder.func);

            let arguments = args.iter()
                .map(|arg| codegen_expression(context, arg).expect("Argument not found"))
                .collect::<Vec<_>>();

            let call = context.builder.ins().call(
                local_call,
                arguments.as_slice()
            );

            context.builder.inst_results(call)
                .first()
                .cloned()
        },

        ValueExpression::UnaryOperation { operator, operand } => {
            let operand = codegen_expression(context, operand.as_ref()?.as_ref()).unwrap();

            match operator {
                OperatorType::Subtract => {
                    Some(context.builder.ins().ineg(operand))
                },
                OperatorType::BitNot => {
                    Some(context.builder.ins().bnot(operand))
                },
                OperatorType::LNot => {
                    let zero = context.builder.ins().iconst(ir::types::I32, 0);
                    Some(context.builder.ins().icmp(ir::condcodes::IntCC::Equal, operand, zero))
                },
                OperatorType::Increment => {
                    let one = context.builder.ins().iconst(ir::types::I32, 1);
                    let add = context.builder.ins().iadd(operand, one);
                    context.builder.ins().store(ir::MemFlags::new(), add, operand, 0);
                    Some(add)
                },
                OperatorType::Decrement => {
                    let one = context.builder.ins().iconst(ir::types::I32, 1);
                    let sub = context.builder.ins().isub(operand, one);
                    context.builder.ins().store(ir::MemFlags::new(), sub, operand, 0);
                    Some(sub)
                },
                _ => unimplemented!("Operator not implemented: {:#?}", operator)
            }
        },

        ValueExpression::BinaryOperation { operator, left, right } => {
            let left = codegen_expression(context, left).unwrap();
            let right = codegen_expression(context, right).unwrap();

            signed_bin_op(&mut context.builder, operator.clone(), left, right)
        },

        ValueExpression::Assignment { left, right, operator } => {
            let left_val = codegen_expression(context, left).unwrap();
            let right_val = codegen_expression(context, right).unwrap();

            context.builder.ins().store(ir::MemFlags::new(), right_val, left_val, 0);
            Some(right_val)
        },

        ValueExpression::VariableReference { name, lval_type } => {
            let val = context.variable_table
                .get(name.as_str())
                .expect(format!("Variable not found: {}", name.as_str()).as_str());
            let type_ = get_cranelift_type(lval_type);

            Some(context.builder.ins().load(type_, ir::MemFlags::new(), *val, 0))
        },

        ValueExpression::StructFieldReference { struct_, field_offset, field_type } => {
            let struct_ = codegen_expression(context, struct_).unwrap();
            let field_type = get_cranelift_type(field_type);
            let field_offset = *field_offset as i32;

            Some(context.builder.ins().load(field_type, ir::MemFlags::new(), struct_, field_offset))
        },

        ValueExpression::IntegerCast { expr, type_, cast_type } => {
            let expr = codegen_expression(context, expr).unwrap();
            let type_ = get_cranelift_type(type_);

            match cast_type {
                IntegerCastType::IReduce => Some(context.builder.ins().ireduce(type_, expr)),
                IntegerCastType::ZeroExtend => Some(context.builder.ins().uextend(type_, expr)),
                IntegerCastType::SignExtend => Some(context.builder.ins().sextend(type_, expr))
            }
        }

        _ => unimplemented!("Value expression not implemented: {:#?}", expr)
    }
}

pub(crate) fn codegen_control_expr(context: &mut FunctionState, expr: &ControlExpression) -> Option<Value> {
    match expr {
        ControlExpression::If { condition, then, else_ } =>
            cg_expr_if(context, condition, then, else_),

        ControlExpression::Loop { condition, body, evaluate_condition_first } =>
            cg_expr_loop(context, condition, body, *evaluate_condition_first),

        ControlExpression::ForLoop { init, increment, condition, body } =>
            cg_expr_for_loop(context, init, increment, condition, body),

        ControlExpression::Return(expr) => {
            let val = codegen_expression(context, expr).unwrap();
            context.builder.ins().return_(&[val]);
            context.current_block_exited = true;

            None
        },

        ControlExpression::Continue => {
            let block_id = context.loop_block_id.expect("Loop block not found");
            context.builder.ins().jump(ir::Block::from_u32(block_id), &[]);
            context.current_block_exited = true;

            None
        },

        ControlExpression::Break => {
            let block_id = context.merge_block_id.expect("Merge block not found");
            context.builder.ins().jump(ir::Block::from_u32(block_id), &[]);
            context.current_block_exited = true;

            None
        },

        _ => unimplemented!("Control expression not implemented: {:#?}", expr)
    }
}

pub(crate) fn codegen_literal_expr(context: &mut FunctionState, expr: &LiteralExpression) -> Option<Value> {
    match expr {
        LiteralExpression::IntLiteral { val, .. } =>
            Some(context.builder.ins().iconst(ir::types::I32, *val)),

        LiteralExpression::StringLiteral(str) => {
            let literal = string_literal(context, str.as_ref());
            let ptr_type = context.object_module.target_config().pointer_type();

            Some(context.builder.ins().global_value(ptr_type, literal))
        },

        _ => unimplemented!("Literal expression not implemented: {:#?}", expr)
    }
}

pub(crate) fn codegen_lvalue_expr(context: &mut FunctionState, expr: &LValueExpression) -> Option<Value> {
    match expr {
        LValueExpression::Value { name, .. } => {
            let val = context.variable_table.get(name.as_str()).unwrap();

            Some(*val)
        },
        LValueExpression::Alloca { name, type_ }
            => {
            let val = stack_alloca(context, type_.clone())?;

            context.variable_table.insert(name.clone(), val);

            Some(val)
        },

        LValueExpression::DereferencedPointer {
            pointer
        } => codegen_expression(context, pointer),

        LValueExpression::StructField {
            struct_, field_offset, field_type
        } => {
            let struct_ = codegen_expression(context, struct_).unwrap();

            Some(context.builder.ins().iadd_imm(struct_, Imm64::new(*field_offset as i64)))
        }

        _ => unimplemented!("LValue expression not implemented: {:#?}", expr)
    }
}

fn cg_expr_if(context: &mut FunctionState, condition: &Expression,
              then: &Vec<Expression>, else_: &Vec<Expression>) -> Option<Value> {
    let condition = codegen_expression(context, condition).unwrap();
    let then_block = context.builder.create_block();
    let else_block = context.builder.create_block();

    context.builder.ins().brif(condition, then_block, &[], else_block, &[]);
    context.builder.switch_to_block(then_block);
    context.variable_table.push_scope();
    context.current_block_exited = false;

    for expr in then {
        codegen_expression(context, expr);
    }

    context.variable_table.pop_scope();

    if else_.is_empty() {
        if !context.current_block_exited {
            context.builder.ins().jump(else_block, &[]);
        }
        context.current_block_exited = false;

        context.builder.switch_to_block(else_block);
        return None;
    }

    let merge_block = context.builder.create_block();
    if !context.current_block_exited {
        context.builder.ins().jump(merge_block, &[]);
    }

    context.builder.switch_to_block(else_block);

    for expr in else_ {
        codegen_expression(context, expr);
    }

    if !context.current_block_exited {
        context.builder.ins().jump(merge_block, &[]);
    }

    context.builder.switch_to_block(merge_block);

    None
}

fn cg_expr_loop(context: &mut FunctionState, condition: &Expression,
                body: &Vec<Expression>, eval_cond_first: bool) -> Option<Value> {
    let loop_block = context.builder.create_block();
    let body_block = context.builder.create_block();
    let merge_block = context.builder.create_block();

    if eval_cond_first {
        context.builder.ins().jump(loop_block, &[]);
    } else {
        context.builder.ins().jump(body_block, &[]);
    }

    context.builder.switch_to_block(loop_block);
    let condition = codegen_expression(context, condition).unwrap();
    context.builder.ins().brif(condition, body_block, &[], merge_block, &[]);

    context.builder.switch_to_block(body_block);
    context.variable_table.push_scope();
    context.merge_block_id = Some(merge_block.as_u32());
    context.loop_block_id = Some(loop_block.as_u32());

    for expr in body {
        codegen_expression(context, expr);
    }

    context.merge_block_id = None;
    context.loop_block_id = None;
    context.variable_table.pop_scope();
    if !context.current_block_exited {
        context.builder.ins().jump(loop_block, &[]);
    }

    context.builder.switch_to_block(merge_block);
    context.current_block_exited = false;

    None
}

fn cg_expr_for_loop(context: &mut FunctionState, init: &Expression,
                    increment: &Expression, condition: &Expression,
                    body: &Vec<Expression>) -> Option<Value> {
    let cond_block = context.builder.create_block();
    let body_block = context.builder.create_block();
    let increment_block = context.builder.create_block();
    let merge_block = context.builder.create_block();

    codegen_expression(context, init);
    context.builder.ins().jump(cond_block, &[]);

    context.builder.switch_to_block(cond_block);
    let condition = codegen_expression(context, condition).unwrap();
    context.builder.ins().brif(condition, body_block, &[], merge_block, &[]);

    context.builder.switch_to_block(body_block);
    context.variable_table.push_scope();
    context.merge_block_id = Some(merge_block.as_u32());
    context.loop_block_id = Some(cond_block.as_u32());

    for expr in body {
        codegen_expression(context, expr);
    }

    context.merge_block_id = None;
    context.loop_block_id = None;
    context.variable_table.pop_scope();
    if !context.current_block_exited {
        context.builder.ins().jump(increment_block, &[]);
    }

    context.builder.switch_to_block(increment_block);
    codegen_expression(context, increment);
    context.builder.ins().jump(cond_block, &[]);

    context.builder.switch_to_block(merge_block);

    None
}