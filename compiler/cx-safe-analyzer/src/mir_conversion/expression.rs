use cx_mir::mir::{expression::{MIRExpression, MIRExpressionKind}, types::MIRTypeKind};
use cx_safe_ir::ast::{FMIRNode, FMIRNodeBody, FMIRType, FRc};

pub fn convert_expression(mir_expr: &MIRExpression) -> FMIRNode {
    match &mir_expr.kind {
        MIRExpressionKind::IntLiteral(value, itype, signed) => {
            FMIRNode {
                body: FMIRNodeBody::IntegerLiteral(*value),
                _type: FMIRType::Standard(
                    MIRTypeKind::Integer { _type: *itype, signed: *signed }.into()
                )
            }
        },
        
        MIRExpressionKind::FloatLiteral(value, ftype) => {
            FMIRNode {
                body: FMIRNodeBody::FloatLiteral(*value),
                _type: FMIRType::Standard(
                    MIRTypeKind::Float { _type: *ftype }.into()
                )
            }
        },
        
        MIRExpressionKind::Block { statements } => {
            if statements.is_empty() {
                return FMIRNode {
                    body: FMIRNodeBody::Unit,
                    _type: FMIRType::Standard(MIRTypeKind::Unit.into()),
                };
            }
          
            let mut fmir_statements = statements.iter()
                .map(|stmt| convert_expression(stmt))
                .collect::<Vec<_>>();
          
            fmir_statements.into_iter()
                .rfold(None, |acc, stmt| {
                    match acc {
                        None => Some(stmt),
                        Some(prev) => Some(FMIRNode {
                            body: FMIRNodeBody::Then {
                                first: FRc::new(prev),
                                second: FRc::new(stmt),
                            },
                            _type: stmt._type.clone(),
                        }),
                    }
                })
                .unwrap()
        },
        
        _ => todo!()
    }
}