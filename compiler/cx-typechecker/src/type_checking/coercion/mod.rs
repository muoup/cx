use cx_mir::mir::{
    expression::{MIRCoercion, MIRExpression, MIRExpressionKind},
    r#type::{MIRType, MIRTypeKind},
};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    type_checking::coercion::implicit::conversion::try_implicit_coercion,
};

pub mod implicit;

pub fn try_explicit_cast(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
    target_type: &MIRType,
) -> CXResult<CoercionResult> {
    Ok(
        try_implicit_coercion(env, expr, target_type)?.or_else(|expr| {
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

                _ => CoercionResult::none(expr),
            }
        }),
    )
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

    pub fn catch_err<F>(self, on_err: F) -> CXResult<MIRExpression>
    where
        F: FnOnce(MIRExpression, String) -> CXResult<MIRExpression>,
    {
        match self {
            CoercionResult::Success { expr } => Ok(expr),
            CoercionResult::CastNotFound(expr) => Ok(expr),
            CoercionResult::Error { message, expr } => on_err(expr, message),
        }
    }

    pub fn assert_cast<F>(self, on_uncasted: F) -> CXResult<MIRExpression>
    where
        F: FnOnce(MIRExpression, Option<String>) -> CXResult<MIRExpression>,
    {
        match self {
            CoercionResult::Success { expr } => Ok(expr),
            CoercionResult::CastNotFound(expr) => on_uncasted(expr, None),
            CoercionResult::Error { message, expr } => on_uncasted(expr, Some(message)),
        }
    }
}
