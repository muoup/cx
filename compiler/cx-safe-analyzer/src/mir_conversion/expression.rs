use cx_mir::mir::{
    expression::{MIRExpression, MIRExpressionKind},
    types::{MIRType, MIRTypeKind},
};
use cx_safe_ir::ast::{CVMOperation, FMIRNode, FMIRNodeBody, FMIRType, FRc};
use std::convert::Into;

pub fn convert_expression(mir_expr: &MIRExpression) -> FMIRNode {
    match &mir_expr.kind {
        MIRExpressionKind::IntLiteral(value, itype, signed) => FMIRNode {
            body: FMIRNodeBody::IntegerLiteral(*value),
            _type: FMIRType::Pure {
                mir_type: MIRType::from(MIRTypeKind::Integer {
                    _type: *itype,
                    signed: *signed,
                }),
            },
        },

        MIRExpressionKind::FloatLiteral(value, ftype) => {
            let f64_value: f64 = value.into();
            FMIRNode {
                body: FMIRNodeBody::FloatLiteral(f64_value),
                _type: FMIRType::Pure {
                    mir_type: MIRTypeKind::Float { _type: *ftype }.into(),
                },
            }
        }

        MIRExpressionKind::Block { statements } => {
            if statements.is_empty() {
                return FMIRNode {
                    body: FMIRNodeBody::Unit,
                    _type: FMIRType::Pure {
                        mir_type: MIRTypeKind::Unit.into(),
                    },
                };
            }

            statements
                .iter()
                .map(|stmt| convert_expression(stmt))
                .rfold(None, |acc, stmt| {
                    match acc {
                        None => Some(stmt),
                        Some(prev) => {
                            let combined_effect = prev._type.union(&stmt._type);

                            Some(FMIRNode {
                                _type: combined_effect.apply(stmt._type.clone()),
                                body: FMIRNodeBody::Then {
                                    first: FRc::new(prev),
                                    second: FRc::new(stmt),
                                },
                            })
                        }
                    }
                })
                .unwrap()
        }

        MIRExpressionKind::CreateStackVariable { name: _, _type } => {
            // Stack allocation - initially unsafe (conservative)
            FMIRNode {
                body: FMIRNodeBody::Alloca,
                _type: FMIRType::CMonad {
                    inner: Box::new(FMIRType::Pure {
                        mir_type: _type.clone(),
                    }),
                    effect: CVMOperation::Unsafe,
                },
            }
        }
        
        MIRExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let then_node = convert_expression(then_branch);
            let else_node = else_branch.as_ref().map(|expr| convert_expression(expr))
                .unwrap_or(FMIRNode::unit());

            let union = then_node._type.union(&else_node._type);
                
            FMIRNode {
                _type: union.apply(FMIRType::pure(MIRType::unit())),
                body: FMIRNodeBody::If {
                    condition: FRc::new(convert_expression(condition)),
                    then_branch: FRc::new(then_node),
                    else_branch: FRc::new(else_node),
                },
            }
        }
        
        MIRExpressionKind::While { pre_eval, condition, body } => {
            let condition_node = FRc::new(convert_expression(condition));
            let body_node = FRc::new(convert_expression(body));
            
            let loop_effect = condition_node._type.union(&body_node._type);
                
            let loop_node = FMIRNode {
                _type: loop_effect.clone().apply(FMIRType::pure(MIRType::unit())),
                body: FMIRNodeBody::CLoop {
                    condition: condition_node.clone(),
                    body: body_node
                },
            };
            
            match pre_eval {
                true => {
                    // Loop effect is technically equal to Condition U loop_effect,
                    // however effect unions are monotonic, so
                    // Condition U (... U Condition U ...) = (... U Condition U ...) 
                    FMIRNode {
                        _type: loop_effect.apply(FMIRType::pure(MIRType::unit())),
                        body: FMIRNodeBody::Then {
                            first: condition_node,
                            second: FRc::new(loop_node),
                        },
                    }
                },
                false => loop_node,
            }
        },
        
        MIRExpressionKind::For { init, condition, increment, body } => {
            let init_node = convert_expression(init);
            let condition_node = convert_expression(condition);
            let increment_node = convert_expression(increment);
            let body_node = convert_expression(body);
            
            let loop_effect = init_node._type
                .union(&condition_node._type)
                .union(&increment_node._type)
                .union(&body_node._type);
                
            FMIRNode {
                _type: loop_effect.clone().apply(FMIRType::pure(MIRType::unit())),
                body: FMIRNodeBody::CLoop {
                    condition: FRc::new(condition_node),
                    body: FRc::new(FMIRNode {
                        _type: loop_effect.apply(FMIRType::pure(MIRType::unit())),
                        body: FMIRNodeBody::Then {
                            first: FRc::new(body_node),
                            second: FRc::new(increment_node),
                        },
                    }),
                },
            }
        },
        
        MIRExpressionKind::Return { value } => {
            let ret_node = value.as_ref().map(
                |expr| convert_expression(expr)
            ).unwrap_or(FMIRNode::unit());
            
            FMIRNode {
                _type: FMIRType::CMonad {
                    inner: Box::new(FMIRType::pure(MIRType::unit())),
                    effect: CVMOperation::Unsafe,
                },
                body: FMIRNodeBody::CReturn {
                    value: FRc::new(ret_node),
                },
            }
        }
        
        _ => todo!("Conversion for expression: {}", mir_expr),
    }
}