use cx_mir_data::expression::{MIRBinOp, MIRInstruction, MIRValue};
use cx_mir_data::program::MIRUnit;
use cx_parsing_data::ast::CXBinOp;
use cx_typechecker_data::ast::{TCAST, TCExpr, TCExprKind, TCFunctionDef};
use cx_typechecker_data::cx_types::{CXType, CXTypeKind};

use crate::builder::MIRBuilder;

pub fn lower_tcast_to_mir(tcast: &TCAST) -> MIRUnit {
    let mut builder = MIRBuilder::new();

    for function in &tcast.function_defs {
        lower_tc_function(&mut builder, function);
    }

    MIRUnit {
        prototypes: tcast.fn_map.values().cloned().collect(),
        functions: builder.generated_functions,
    }
}

fn lower_tc_function(builder: &mut MIRBuilder, function: &TCFunctionDef) {
    builder.start_function(function.prototype.clone());
    lower_tc_expression(builder, &function.body);
    builder.finish_function();
}

fn lower_tc_expression(builder: &mut MIRBuilder, expression: &TCExpr) -> Option<MIRValue> {
    match &expression.kind {
        TCExprKind::IntLiteral { value } => Some(MIRValue::IntLiteral {
            value: *value,
            _type: expression._type.clone(),
        }),
        TCExprKind::FloatLiteral { value } => Some(MIRValue::FloatLiteral {
            value: *value,
            _type: expression._type.clone(),
        }),
        TCExprKind::BinOp { lhs, rhs, op } => {
            let lhs_val = lower_tc_expression(builder, lhs).unwrap();
            let rhs_val = lower_tc_expression(builder, rhs).unwrap();
            let result_reg = builder.new_temp_register();

            builder.add_instruction(MIRInstruction::BinOp {
                result: result_reg.clone(),
                lhs: lhs_val,
                rhs: rhs_val,
                op: lower_binop(&lhs._type, &rhs._type, op.clone())
            });

            Some(MIRValue::Register(result_reg))
        }
        TCExprKind::UnOp { operand, operator } => {
            let operand_val = lower_tc_expression(builder, operand).unwrap();
            let result_reg = builder.new_temp_register();

            builder.add_instruction(MIRInstruction::UnOp {
                result: result_reg.clone(),
                operand: operand_val,
                op: *operator,
            });

            Some(MIRValue::Register(result_reg))
        }
        TCExprKind::Return { value } => {
            let return_val = value
                .as_ref()
                .and_then(|val| lower_tc_expression(builder, val));
            builder.add_instruction(MIRInstruction::Return { value: return_val });
            None
        }
        TCExprKind::Block { statements } => {
            for (i, statement) in statements.iter().enumerate() {
                let is_last = i == statements.len() - 1;
                let value = lower_tc_expression(builder, statement);

                if is_last {
                    return value;
                }
            }
            None
        }
        TCExprKind::VariableDeclaration { type_, name } => {
            let var_reg = builder.new_temp_register();
            builder.add_instruction(MIRInstruction::CreateStackRegion {
                result: var_reg.clone(),
                _type: type_.clone(),
            });
            builder.declare_variable(name.clone(), var_reg);
            None
        }
        TCExprKind::GlobalVariableReference { name } => {
            let result_reg = builder.new_temp_register();
            builder.add_instruction(MIRInstruction::LoadGlobal {
                result: result_reg.clone(),
                name: name.clone(),
            });
            Some(MIRValue::Register(result_reg))
        }
        TCExprKind::VariableReference { name } => {
            let var_reg = builder.get_variable(name).unwrap();
            Some(MIRValue::Register(var_reg))
        }
        TCExprKind::Assignment {
            target,
            value,
            additional_op,
        } => {
            let target_val = lower_tc_expression(builder, target).unwrap();
            let value_val = lower_tc_expression(builder, value).unwrap();

            if let Some(op) = additional_op {
                let result_reg = builder.new_temp_register();
                let loaded_target = builder.new_temp_register();
                builder.add_instruction(MIRInstruction::MemoryRead {
                    result: loaded_target.clone(),
                    source: target_val.clone(),
                    _type: value.as_ref()._type.clone(),
                });
                builder.add_instruction(MIRInstruction::BinOp {
                    result: result_reg.clone(),
                    lhs: MIRValue::Register(loaded_target),
                    rhs: value_val,
                    op: *op,
                });
                builder.add_instruction(MIRInstruction::MemoryWrite {
                    target: target_val,
                    value: MIRValue::Register(result_reg),
                });
            } else {
                builder.add_instruction(MIRInstruction::MemoryWrite {
                    target: target_val,
                    value: value_val,
                });
            }
            None
        }
        TCExprKind::ImplicitLoad { operand } => {
            let operand_val = lower_tc_expression(builder, operand).unwrap();
            let result_reg = builder.new_temp_register();
            builder.add_instruction(MIRInstruction::MemoryRead {
                result: result_reg.clone(),
                source: operand_val,
                _type: expression._type.clone(),
            });
            Some(MIRValue::Register(result_reg))
        }
        TCExprKind::Coercion { operand, cast_type } => {
            let operand_val = lower_tc_expression(builder, operand).unwrap();
            let result_reg = builder.new_temp_register();
            
            builder.add_instruction(MIRInstruction::Coercion {
                result: result_reg.clone(),
                operand: operand_val,
                cast_type: *cast_type,
            });
            
            Some(MIRValue::Register(result_reg))
        }
        _ => todo!("unimplemented MIR lowering for {:?}", expression.kind),
    }
}

pub(crate) fn lower_binop(lhs_type: &CXType, rhs_type: &CXType, op: CXBinOp) -> MIRBinOp {
    match (&lhs_type.kind, &rhs_type.kind, op) {
        (CXTypeKind::Integer { signed: true, .. }, CXTypeKind::Integer { signed: true, .. }, op) => {
            match op {
                CXBinOp::Add => MIRBinOp::ADD,
                CXBinOp::Subtract => MIRBinOp::SUB,
                CXBinOp::Multiply => MIRBinOp::IMUL,
                CXBinOp::Divide => MIRBinOp::IDIV,
                CXBinOp::Modulus => MIRBinOp::IDIV,
                
                CXBinOp::Less => MIRBinOp::ILT,
                CXBinOp::Greater => MIRBinOp::IGT,
                CXBinOp::LessEqual => MIRBinOp::ILE,
                CXBinOp::GreaterEqual => MIRBinOp::IGE,
                
                CXBinOp::Equal => MIRBinOp::EQ,
                CXBinOp::NotEqual => MIRBinOp::NEQ,
                
                CXBinOp::BitAnd => MIRBinOp::AND,
                CXBinOp::BitOr => MIRBinOp::OR,
                CXBinOp::BitXor => MIRBinOp::XOR,
                CXBinOp::LShift => MIRBinOp::SHL,
                CXBinOp::RShift => MIRBinOp::SHR,
                
                _ => panic!("Unsupported binary operation for signed integers: {:?}", op),
            }
        },
        
        (CXTypeKind::Integer { signed: false, .. }, CXTypeKind::Integer { signed: false, .. }, op) => {
            match op {
                CXBinOp::Add => MIRBinOp::ADD,
                CXBinOp::Subtract => MIRBinOp::SUB,
                CXBinOp::Multiply => MIRBinOp::MUL,
                CXBinOp::Divide => MIRBinOp::DIV,
                
                CXBinOp::Less => MIRBinOp::LT,
                CXBinOp::Greater => MIRBinOp::GT,
                CXBinOp::LessEqual => MIRBinOp::LE,
                CXBinOp::GreaterEqual => MIRBinOp::GE,
                
                CXBinOp::Equal => MIRBinOp::EQ,
                CXBinOp::NotEqual => MIRBinOp::NEQ,
                
                CXBinOp::BitAnd => MIRBinOp::AND,
                CXBinOp::BitOr => MIRBinOp::OR,
                CXBinOp::BitXor => MIRBinOp::XOR,
                CXBinOp::LShift => MIRBinOp::SHL,
                CXBinOp::RShift => MIRBinOp::SHR,
                
                _ => panic!("Unsupported binary operation for unsigned integers: {:?}", op),
            }
        },
        
        (CXTypeKind::Float { .. }, CXTypeKind::Float { .. }, op) => {
            match op {
                CXBinOp::Add => MIRBinOp::FADD,
                CXBinOp::Subtract => MIRBinOp::FSUB,
                CXBinOp::Multiply => MIRBinOp::FMUL,
                CXBinOp::Divide => MIRBinOp::FDIV,
                
                CXBinOp::Less => MIRBinOp::LT,
                CXBinOp::Greater => MIRBinOp::GT,
                CXBinOp::LessEqual => MIRBinOp::LE,
                CXBinOp::GreaterEqual => MIRBinOp::GE,
                
                CXBinOp::Equal => MIRBinOp::EQ,
                CXBinOp::NotEqual => MIRBinOp::NEQ,
                
                _ => panic!("Unsupported binary operation for floats: {:?}", op),
            }
        },
        
        (CXTypeKind::Bool, CXTypeKind::Bool, op) => {
            match op {
                
            }
        },
        
        _ => panic!(
            "Unsupported binary operation for types {:?} and {:?}: {:?}",
            lhs_type, rhs_type, op
        ),
    }
}