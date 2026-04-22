use cx_mir::mir::{
    expression::{MIRCoercion, MIRExpression, MIRExpressionKind},
    r#type::{MIRIntegerType, MIRType, MIRTypeKind},
};
use cx_util::CXResult;

use crate::{environment::TypeEnvironment, type_checking::coercion::implicit::try_implicit_cast};

pub mod implicit;

pub fn try_explicit_cast(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
    target_type: &MIRType,
) -> CoercionResult {
    try_implicit_cast(env, expr, target_type).or_else(|expr| {
        let from_type = expr.get_type();
        let coerced = |conversion: MIRCoercion| {
            let coerced = MIRExpression {
                token_range: expr.token_range.clone(),
                _type: target_type.clone(),
                kind: MIRExpressionKind::TypeConversion {
                    operand: Box::new(expr.clone()),
                    conversion,
                },
            };

            CoercionResult::some(coerced)
        };

        match (&from_type.kind, &target_type.kind) {
            (MIRTypeKind::PointerTo { .. }, MIRTypeKind::PointerTo { .. }) => {
                coerced(MIRCoercion::ReinterpretBits)
            }

            (MIRTypeKind::PointerTo { .. }, MIRTypeKind::Integer { _type, .. }) => {
                coerced(MIRCoercion::PtrToInt { to_type: *_type })
            }

            (MIRTypeKind::Integer { signed, .. }, MIRTypeKind::PointerTo { .. }) => {
                coerced(MIRCoercion::IntToPtr { sextend: *signed })
            }

            (
                MIRTypeKind::PointerTo {
                    inner_type: ptr_inner,
                },
                MIRTypeKind::MemoryReference {
                    inner_type: str_inner,
                },
            ) if matches!(
                env.type_context.get(*ptr_inner.as_ref()).map(|ty| &ty.kind),
                Some(MIRTypeKind::Integer {
                    _type: MIRIntegerType::I8,
                    ..
                })
            ) && matches!(
                env.type_context.get(*str_inner.as_ref()).map(|ty| &ty.kind),
                Some(MIRTypeKind::Str)
            ) =>
            {
                coerced(MIRCoercion::CStrToStr)
            }

            _ => CoercionResult::none(expr),
        }
    })
}

pub enum CoercionResult {
    CastNotFound(MIRExpression),

    Success {
        expr: MIRExpression,
    },

    Error {
        message: String,
        expr: MIRExpression,
    },
}

impl CoercionResult {
    pub fn or_else<F>(self, f: F) -> Self
    where
        F: FnOnce(MIRExpression) -> Self,
    {
        match self {
            CoercionResult::CastNotFound(expr) => f(expr),

            _ => self,
        }
    }

    pub fn some(expr: MIRExpression) -> Self {
        CoercionResult::Success { expr }
    }

    pub fn none(expr: MIRExpression) -> Self {
        CoercionResult::CastNotFound(expr)
    }

    pub fn error(message: String, expr: MIRExpression) -> Self {
        CoercionResult::Error { message, expr }
    }
    
    pub fn as_err<F>(self, on_err: F) -> CXResult<MIRExpression> 
    where
        F: FnOnce(MIRExpression, String) -> CXResult<MIRExpression>,
    {
        match self {
            CoercionResult::Success { expr } => Ok(expr),
            CoercionResult::CastNotFound(expr) => Ok(expr),
            CoercionResult::Error { message, expr } => on_err(expr, message)
        }
    }
    
    pub fn as_cast<F>(self, on_uncasted: F) -> CXResult<MIRExpression> 
    where
        F: FnOnce(MIRExpression, Option<String>) -> CXResult<MIRExpression>,
    {
        match self {
            CoercionResult::Success { expr } => Ok(expr),
            CoercionResult::CastNotFound(expr) => on_uncasted(expr, None),
            CoercionResult::Error { message, expr } => on_uncasted(expr, Some(message))
        }
    }
}

// Shared Helper Functions

pub(crate) fn coercion_expr(
    expr: MIRExpression,
    target_type: &MIRType,
) -> MIRExpression {
    MIRExpression {
        token_range: expr.token_range.clone(),
        _type: target_type.clone(),
        kind: MIRExpressionKind::TypeConversion {
            operand: Box::new(expr),
            conversion: MIRCoercion::ReinterpretBits,
        },
    }
}