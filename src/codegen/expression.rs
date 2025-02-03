use crate::codegen::codegen::FunctionState;
use crate::lex::token::OperatorType;
use crate::parse::ast::Expression;
use cranelift::codegen::ir;
use cranelift::codegen::ir::GlobalValue;
use cranelift::prelude::{InstBuilder, Value};
use cranelift_module::{DataDescription, Linkage, Module};
use crate::codegen::routines::allocate_variable;
use crate::codegen::value_type::get_cranelift_type;

pub(crate) fn codegen_expression(context: &mut FunctionState, expr: &Expression) -> Option<Value> {
    match expr {
        Expression::Return(expr) => {
            context.current_block_exited = true;
            match expr.as_ref() {
                &Expression::Unit => {
                    context.builder.ins().return_(&[]);
                },
                _ => {
                    let codegen_expr = codegen_expression(context, expr).unwrap();

                    context.builder.ins().return_(&[
                        codegen_expr
                    ]);
                }
            }
            None
        },
        Expression::FunctionCall { name, args } => {
            let Expression::Identifier(fn_name) = name.as_ref() else { return None; };

            let id = context.functions.get(fn_name.as_str()).expect(format!("Function not found: {}", fn_name.as_str()).as_str());
            let sig = context.object_module.declarations().get_function_decl(*id).signature.clone();

            let call = context.object_module
                .declare_function(&fn_name, Linkage::Export, &sig)
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

        Expression::BinaryOperation { left, right, operator } => {
            let left = codegen_expression(context, left).unwrap();
            let right = codegen_expression(context, right).unwrap();

            match operator.clone() {
                OperatorType::Add => Some(context.builder.ins().iadd(left, right)),
                OperatorType::Subtract => Some(context.builder.ins().isub(left, right)),
                OperatorType::Multiply => Some(context.builder.ins().imul(left, right)),
                OperatorType::Divide => Some(context.builder.ins().udiv(left, right)),
                OperatorType::Modulo => Some(context.builder.ins().urem(left, right)),

                OperatorType::Less => Some(context.builder.ins().icmp(ir::condcodes::IntCC::SignedLessThan, left, right)),
                OperatorType::LessEqual => Some(context.builder.ins().icmp(ir::condcodes::IntCC::SignedLessThanOrEqual, left, right)),
                OperatorType::Equal => Some(context.builder.ins().icmp(ir::condcodes::IntCC::Equal, left, right)),
                OperatorType::Greater => Some(context.builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThan, left, right)),
                OperatorType::GreaterEqual => Some(context.builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThanOrEqual, left, right)),

                _ => unimplemented!("Operator not implemented: {:?}", operator)
            }
        },

        Expression::If { condition, then, else_ } => {
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
        },
        Expression::Loop { condition, body, evaluate_condition_first } => {
            let loop_block = context.builder.create_block();
            let body_block = context.builder.create_block();
            let merge_block = context.builder.create_block();

            if *evaluate_condition_first {
                context.builder.ins().jump(loop_block, &[]);
            } else {
                context.builder.ins().jump(body_block, &[]);
            }

            context.builder.switch_to_block(loop_block);
            let condition = codegen_expression(context, condition).unwrap();
            context.builder.ins().brif(condition, body_block, &[], merge_block, &[]);

            context.builder.switch_to_block(body_block);
            context.variable_table.push_scope();

            for expr in body {
                codegen_expression(context, expr);
            }

            context.variable_table.pop_scope();
            if !context.current_block_exited {
                context.builder.ins().jump(loop_block, &[]);
            }

            context.builder.switch_to_block(merge_block);
            None
        },
        Expression::ForLoop { init, increment, condition, body } => {
            let cond_block = context.builder.create_block();
            let body_block = context.builder.create_block();
            let increment_block = context.builder.create_block();
            let merge_block = context.builder.create_block();

            codegen_expression(context, init.as_ref());
            context.builder.ins().jump(cond_block, &[]);

            context.builder.switch_to_block(cond_block);
            let condition = codegen_expression(context, condition).unwrap();
            context.builder.ins().brif(condition, body_block, &[], merge_block, &[]);

            context.builder.switch_to_block(body_block);
            context.variable_table.push_scope();

            for expr in body {
                codegen_expression(context, expr);
            }

            context.variable_table.pop_scope();
            if !context.current_block_exited {
                context.builder.ins().jump(increment_block, &[]);
            }

            context.builder.switch_to_block(increment_block);
            codegen_expression(context, increment.as_ref());
            context.builder.ins().jump(cond_block, &[]);

            context.builder.switch_to_block(merge_block);
            None
        },

        Expression::Assignment{ left, right, op } => {
            let left = codegen_expression(context, left).unwrap();
            let right = codegen_expression(context, right).unwrap();

            context.builder.ins().store(ir::MemFlags::new(), right, left, 0);
            Some(right)
        },
        Expression::VariableDeclaration { name, type_ } => {
            let param_type = get_cranelift_type(context.object_module, type_);

            allocate_variable(
                &mut context.builder, &mut context.variable_table,
                name, param_type,
                None
            )
        },
        Expression::VariableStorage { name } => {
            let (val, _) = context.variable_table.get(name.as_str())
                .expect(format!("Variable not found: {}", name.as_str()).as_str());

            Some(*val)
        },
        Expression::Identifier(name) => {
            let (val, type_) = context.variable_table.get(name.as_str())
                .expect(format!("Variable not found: {}", name.as_str()).as_str());

            Some(context.builder.ins().load(*type_, ir::MemFlags::new(), *val, 0))
        },

        Expression::IntLiteral(val) => {
            Some(context.builder.ins().iconst(ir::types::I32, *val))
        },
        Expression::StringLiteral(str) => {
            let literal = string_literal(context, str.as_ref());
            let ptr_type = context.object_module.target_config().pointer_type();

            Some(context.builder.ins().global_value(ptr_type, literal))
        },
        Expression::Unit => None,
        _ => unimplemented!("Expression not implemented: {:?}", expr)
    }
}

fn string_literal(context: &mut FunctionState, str: &str) -> GlobalValue {
    let id = context.object_module.declare_anonymous_data(
        false,
        false
    ).unwrap();

    let mut data = DataDescription::new();
    let mut str_data = str.as_bytes().to_vec();
    str_data.push('\0' as u8);

    data.define(str_data.into_boxed_slice());

    context.object_module.define_data(id, &data).unwrap();
    context.object_module.declare_data_in_func(
        id,
        context.builder.func
    )
}