use std::rc::Rc;

use cx_fmir_data::ast::{FMIRNode, FMIRNodeBody, FMIRType, FRc};
use cx_parsing_data::ast::{CXExpr, CXExprKind};
use cx_typechecker_data::mir::{
    program::MIRBaseMappings,
    types::{CXIntegerType, MIRFunctionPrototype, MIRType, MIRTypeKind},
};
use cx_util::CXResult;

use crate::environment::TypeEnvironment;

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
        
        _ => todo!(),
    })
}
