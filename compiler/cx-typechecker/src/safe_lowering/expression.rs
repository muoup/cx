use std::rc::Rc;

use cx_fmir_data::ast::{FMIRNode, FMIRNodeBody, FMIRType, FRc};
use cx_parsing_data::ast::{CXExpr, CXExprKind};
use cx_typechecker_data::mir::{
    expression::{MIRExpression, MIRExpressionKind},
    program::MIRBaseMappings,
    types::{CXIntegerType, MIRFunctionPrototype, MIRType, MIRTypeKind},
};
use cx_util::CXResult;

use crate::environment::TypeEnvironment;

// TODO: Implement mir_expression_to_fmir_node to convert MIRExpression to FMIRNode
// This is incomplete PoC code that needs to be updated for new MIR structure
// Since safe functions now go through MIR first, this function should convert MIRExpression to FMIRNode

fn mir_expression_to_fmir_node(_expr: &MIRExpression) -> CXResult<FMIRNode> {
    // TODO: Implement conversion from MIRExpression to FMIRNode
    todo!("mir_expression_to_fmir_node not yet implemented")
}

pub fn lower_expression(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    prototype: &MIRFunctionPrototype,
    expr: &CXExpr,
) -> CXResult<FMIRNode> {
    Ok(match &expr.kind {
        CXExprKind::IntLiteral { val, bytes } => {
            let int_type = MIRType::from(MIRTypeKind::Integer {
                _type: match bytes {
                    1 => CXIntegerType::I8,
                    2 => CXIntegerType::I16,
                    4 => CXIntegerType::I32,
                    8 => CXIntegerType::I64,
                    _ => unreachable!(),
                },
                signed: true,
            });

            FMIRNode {
                _type: FMIRType::Standard(int_type),
                body: FMIRNodeBody::IntegerLiteral(*val),
            }
        }

        CXExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition = FRc::new(lower_expression(env, base_data, prototype, condition)?);
            let then_branch = FRc::new(lower_expression(env, base_data, prototype, then_branch)?);
            let else_branch = else_branch
                .as_ref()
                .map(|branch| lower_expression(env, base_data, prototype, branch))
                .transpose()?
                .map(FRc::new);

            FMIRNode::if_else(condition, then_branch, else_branch)
        }

        CXExprKind::Block { exprs } => {
            let mut current_node = None;

            for expr in exprs {
                let lowered_expr = lower_expression(env, base_data, prototype, expr)?;

                current_node = match current_node {
                    Some(prev_node) => Some(FMIRNode::if_else(
                        Rc::new(prev_node),
                        Rc::new(lowered_expr.clone()),
                        None,
                    )),
                    None => Some(lowered_expr),
                };
            }

            current_node.unwrap_or(FMIRNode::unit())
        }

        CXExprKind::Return { value } => {
            let return_value = match value {
                Some(val) => FRc::new(lower_expression(env, base_data, prototype, val)?),
                None => FRc::new(FMIRNode::unit()),
            };

            FMIRNode {
                _type: FMIRType::Standard(MIRType::unit()),
                body: FMIRNodeBody::CReturn {
                    value: return_value,
                },
            }
        }

        CXExprKind::While {
            condition,
            body,
            pre_eval,
        } => {
            let condition = FRc::new(lower_expression(env, base_data, prototype, condition)?);
            let body = FRc::new(lower_expression(env, base_data, prototype, body)?);
            let cloop = FMIRNode::cloop(condition.clone(), body);

            if *pre_eval {
                FMIRNode::if_else(condition, FRc::new(cloop), Some(Rc::new(FMIRNode::unit())))
            } else {
                cloop
            }
        }

        CXExprKind::Unit => FMIRNode::unit(),

        CXExprKind::Identifier(name) => {
            // Look up the variable in the environment
            let mir_expr = env
                .symbol_value(name.as_str())
                .ok_or_else(|| cx_util::CXError::create_boxed(format!("undefined variable: {}", name)))?
                .clone();

            // Convert MIRExpression to FMIRNode
            mir_expression_to_fmir_node(&mir_expr)?
        }

        CXExprKind::UnOp { operand, operator } => {
            let lowered_operand = lower_expression(env, base_data, prototype, operand)?;
            let operand_type = &lowered_operand._type;

            // For now, create a simple representation of unary operations
            // TODO: This could be enhanced to use proper intrinsic functions or new FMIRNodeBody variants
            match operator {
                cx_parsing_data::ast::CXUnOp::Negative => {
                    // Negation: For now, we'll create a pure value placeholder
                    // In a full implementation, this would be an Application of a negation function
                    FMIRNode {
                        _type: operand_type.clone(),
                        body: FMIRNodeBody::Pure,
                    }
                }
                cx_parsing_data::ast::CXUnOp::BNot => {
                    // Bitwise NOT
                    FMIRNode {
                        _type: operand_type.clone(),
                        body: FMIRNodeBody::Pure,
                    }
                }
                cx_parsing_data::ast::CXUnOp::LNot => {
                    // Logical NOT - returns bool (I1, unsigned)
                    FMIRNode {
                        _type: FMIRType::Standard(MIRType::from(MIRTypeKind::Integer {
                            _type: CXIntegerType::I1,
                            signed: false,
                        })),
                        body: FMIRNodeBody::Pure,
                    }
                }
                _ => todo!("UnOp operator {:?} not yet implemented", operator),
            }
        }

        CXExprKind::BinOp { lhs, rhs, op } => {
            let _lowered_lhs = lower_expression(env, base_data, prototype, lhs)?;
            let _lowered_rhs = lower_expression(env, base_data, prototype, rhs)?;

            // For now, create a simple representation of binary operations
            // TODO: This could be enhanced to use proper intrinsic functions or new FMIRNodeBody variants
            let result_type = match op {
                cx_parsing_data::ast::CXBinOp::Less
                | cx_parsing_data::ast::CXBinOp::Greater
                | cx_parsing_data::ast::CXBinOp::LessEqual
                | cx_parsing_data::ast::CXBinOp::GreaterEqual
                | cx_parsing_data::ast::CXBinOp::Equal
                | cx_parsing_data::ast::CXBinOp::NotEqual
                | cx_parsing_data::ast::CXBinOp::LAnd
                | cx_parsing_data::ast::CXBinOp::LOr => {
                    FMIRType::Standard(MIRType::from(MIRTypeKind::Integer {
                        _type: CXIntegerType::I1,
                        signed: false,
                    }))
                }
                _ => {
                    // For arithmetic operations, the result type is the same as the operand type
                    _lowered_lhs._type.clone()
                }
            };

            FMIRNode {
                _type: result_type,
                body: FMIRNodeBody::Pure,
            }
        }
        
        _ => todo!(),
    })
}
