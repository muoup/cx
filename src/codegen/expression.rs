use crate::codegen::codegen::FunctionState;
use crate::lex::token::OperatorType;
use crate::parse::ast::Expression;
use cranelift::codegen::ir;
use cranelift::codegen::ir::GlobalValue;
use cranelift::prelude::{InstBuilder, Value};
use cranelift_module::{DataDescription, Linkage, Module};

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

            let id = context.functions.get(fn_name.as_str()).expect("Function not found");
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

        Expression::Identifier(name) => {
            context.variable_table.get(name.as_str()).cloned()
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
