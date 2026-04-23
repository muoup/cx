use cx_mir::mir::expression::MIRExpression;
use cx_util::CXResult;

pub enum CoercionObstacle {
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

    pub fn catch_unapplied<F>(self, on_unapplied: F) -> CXResult<MIRExpression>
    where
        F: FnOnce(MIRExpression, Option<CoercionObstacle>) -> CXResult<MIRExpression>,
    {
        match self {
            CoercionResult::Success { expr } => Ok(expr),
            CoercionResult::Unapplied { expr, cause } => on_unapplied(expr, cause),
        }
    }
}
