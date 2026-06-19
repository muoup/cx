use cx_mir::mir::{
    expression::{MIRExpression, MIRExpressionKind},
    r#type::{MIRFloatType, MIRIntegerType, MIRType, MIRTypeKind},
};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

#[derive(Clone, Debug)]
pub struct ComptimeValue {
    pub kind: ComptimeKind,
    pub token_range: TokenRange,
}

#[derive(Clone, Debug)]
pub enum ComptimeKind {
    Integer {
        val: i64,
        itype: MIRIntegerType,
        signed: bool,
    },
    Float {
        val: FloatWrapper,
        ftype: MIRFloatType,
    },
    Unit,

    #[allow(dead_code)]
    Emit(MIRExpression),
}

impl ComptimeValue {
    #[allow(dead_code)]
    pub fn ty(&self) -> MIRType {
        match &self.kind {
            ComptimeKind::Integer { itype, signed, .. } => MIRTypeKind::Integer {
                _type: *itype,
                signed: *signed,
            }
            .into(),
            ComptimeKind::Float { ftype, .. } => MIRTypeKind::Float { _type: *ftype }.into(),
            ComptimeKind::Unit => MIRType::unit(),
            ComptimeKind::Emit(expr) => expr._type.clone(),
        }
    }

    #[allow(dead_code)]
    pub fn into_expression(self) -> MIRExpression {
        match self.kind {
            ComptimeKind::Integer { val, itype, signed } => MIRExpression {
                kind: MIRExpressionKind::IntLiteral(val),
                _type: MIRTypeKind::Integer {
                    _type: itype,
                    signed,
                }
                .into(),
                token_range: self.token_range,
            },
            ComptimeKind::Float { val, ftype } => MIRExpression {
                kind: MIRExpressionKind::FloatLiteral(val),
                _type: MIRTypeKind::Float { _type: ftype }.into(),
                token_range: self.token_range,
            },
            ComptimeKind::Unit => MIRExpression {
                kind: MIRExpressionKind::Unit,
                _type: MIRType::unit(),
                token_range: self.token_range,
            },
            ComptimeKind::Emit(expr) => expr,
        }
    }

    pub fn as_integer(&self) -> Option<i64> {
        match self.kind {
            ComptimeKind::Integer { val, .. } => Some(val),
            _ => None,
        }
    }
}
