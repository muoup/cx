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

            CoercionResult::success(coerced)
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

            _ => CoercionResult::unapplied(expr),
        }
    })
}

pub enum CoercionObstacle {
    NoValidCoercion,
    Uncopyable,
}

pub enum CoercionResult {
    Success {
        expr: MIRExpression,
    },

    Unapplied {
        expr: MIRExpression,
        cause: Option<CoercionObstacle>,
    },
}

impl CoercionResult {
    pub fn or_else<F>(self, f: F) -> CXResult<Self>
    where
        F: FnOnce(MIRExpression) -> CXResult<Self>,
    {
        Ok(match self {
            CoercionResult::Unapplied {
                expr,
                cause: base_cause,
            } => match f(expr)? {
                CoercionResult::Unapplied {
                    expr,
                    cause: new_cause,
                } => CoercionResult::Unapplied {
                    expr,
                    cause: new_cause.or(base_cause),
                },

                misc => misc,
            },

            _ => self,
        })
    }

    pub fn success(expr: MIRExpression) -> CXResult<Self> {
        Ok(CoercionResult::Success { expr })
    }

    pub fn unapplied(expr: MIRExpression) -> CXResult<Self> {
        Ok(CoercionResult::Unapplied { expr, cause: None })
    }

    pub fn tried_application(expr: MIRExpression, cause: CoercionObstacle) -> CXResult<Self> {
        Ok(CoercionResult::Unapplied {
            expr,
            cause: Some(cause),
        })
    }

    pub fn catch_incomplete<F>(self, on_uncasted: F) -> CXResult<MIRExpression>
    where
        F: FnOnce(MIRExpression, Option<String>) -> CXResult<MIRExpression>,
    {
        match self {
            CoercionResult::Success { expr } => Ok(expr),
            CoercionResult::IncompleteCast(expr) => on_uncasted(expr, None),
        }
    }
}
