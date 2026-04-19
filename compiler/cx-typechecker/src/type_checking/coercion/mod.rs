use cx_mir::mir::{expression::MIRExpression, program::MIRBaseMappings, r#type::MIRType};

use crate::{environment::TypeEnvironment, type_checking::coercion::implicit::try_implicit_cast};

pub mod implicit;

pub fn try_explicit_cast(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: MIRExpression,
    target_type: &MIRType,
) -> CoercionResult {
    try_implicit_cast(env, base_data, expr, target_type).or_else(|expr| CoercionResult::none(expr))
}

pub enum CoercionResult {
    CastNotFound(MIRExpression),

    Success {
        expr: MIRExpression,
        new_type: MIRType,
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

    pub fn some(expr: MIRExpression, new_type: MIRType) -> Self {
        CoercionResult::Success { expr, new_type }
    }

    pub fn none(expr: MIRExpression) -> Self {
        CoercionResult::CastNotFound(expr)
    }

    pub fn error(message: String, expr: MIRExpression) -> Self {
        CoercionResult::Error { message, expr }
    }
}
