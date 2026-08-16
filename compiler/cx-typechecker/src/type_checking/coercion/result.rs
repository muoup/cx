use cx_log::CXResult;
use cx_thir::thir::expression::THIRExpression;

pub enum CoercionObstacle {
    Uncopyable,
}

pub enum CoercionResult {
    Success {
        expr: THIRExpression,
    },

    Unapplied {
        expr: THIRExpression,
        cause: Option<CoercionObstacle>,
    },
}

impl CoercionResult {
    #[allow(dead_code)]
    pub fn and_then<F>(self, f: F) -> CXResult<Self>
    where
        F: FnOnce(THIRExpression) -> CXResult<Self>,
    {
        Ok(match self {
            CoercionResult::Success { expr } => f(expr)?,
            unapplied => unapplied,
        })
    }

    pub fn or_else<F>(self, f: F) -> CXResult<Self>
    where
        F: FnOnce(THIRExpression) -> CXResult<Self>,
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

    pub fn success(expr: THIRExpression) -> CXResult<Self> {
        Ok(CoercionResult::Success { expr })
    }

    pub fn unapplied(expr: THIRExpression) -> CXResult<Self> {
        Ok(CoercionResult::Unapplied { expr, cause: None })
    }

    pub fn unapplied_with_obstacle(
        expr: THIRExpression,
        cause: CoercionObstacle,
    ) -> CXResult<Self> {
        Ok(CoercionResult::Unapplied {
            expr,
            cause: Some(cause),
        })
    }

    pub fn catch_unapplied<F>(self, on_unapplied: F) -> CXResult<THIRExpression>
    where
        F: FnOnce(THIRExpression, Option<CoercionObstacle>) -> CXResult<THIRExpression>,
    {
        match self {
            CoercionResult::Success { expr } => Ok(expr),
            CoercionResult::Unapplied { expr, cause } => on_unapplied(expr, cause),
        }
    }
}
