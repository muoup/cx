use cx_thir::thir::{
    expression::{THIRExpression, THIRExpressionKind},
    r#type::{THIRFloatType, THIRIntType, THIRType, THIRTypeKind},
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
        itype: THIRIntType,
        signed: bool,
    },
    Float {
        val: FloatWrapper,
        ftype: THIRFloatType,
    },
    Unit,

    #[allow(dead_code)]
    Emit(THIRExpression),
}

impl ComptimeValue {
    #[allow(dead_code)]
    pub fn ty(&self) -> THIRType {
        match &self.kind {
            ComptimeKind::Integer { itype, signed, .. } => THIRTypeKind::Integer {
                _type: *itype,
                signed: *signed,
            }
            .into(),
            ComptimeKind::Float { ftype, .. } => THIRTypeKind::Float { _type: *ftype }.into(),
            ComptimeKind::Unit => THIRType::unit(),
            ComptimeKind::Emit(expr) => expr._type.clone(),
        }
    }

    #[allow(dead_code)]
    pub fn into_expression(self) -> THIRExpression {
        match self.kind {
            ComptimeKind::Integer { val, itype, signed } => THIRExpression {
                kind: THIRExpressionKind::IntLiteral(val),
                _type: THIRTypeKind::Integer {
                    _type: itype,
                    signed,
                }
                .into(),
                token_range: self.token_range,
            },
            ComptimeKind::Float { val, ftype } => THIRExpression {
                kind: THIRExpressionKind::FloatLiteral(val),
                _type: THIRTypeKind::Float { _type: ftype }.into(),
                token_range: self.token_range,
            },
            ComptimeKind::Unit => THIRExpression {
                kind: THIRExpressionKind::Unit,
                _type: THIRType::unit(),
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
