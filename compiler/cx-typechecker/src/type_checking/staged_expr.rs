use cx_hir::ast::expression::HIRExpression;
use cx_log::CXResult;
use cx_thir::{EnvironmentNamespace, thir::{comptime::{THIRStagedExpr, THIRStagedParameter}, expression::{THIRExpression, THIRExpressionKind}}};

use crate::{environment::TypeEnvironment, type_checking::result::TypecheckResult};

pub struct StagedExprResult {
    inner: THIRStagedExpr
}

impl StagedExprResult {
    pub fn map_expr<F>(mut self, f: F) -> CXResult<Self>
        where F: FnOnce(THIRExpression) -> CXResult<THIRExpression> {

        self.inner = self.inner.map_expr(f)?;
        Ok(self)
    }

    pub fn add_parameters(mut self, params: Vec<THIRStagedParameter>) -> Self {
        self.inner.add_params(params);
        self
    }

    pub fn into_expr(self) -> THIRExpression {
        THIRExpression {
            _type: self.inner.expr()._type.clone(),
            token_range: self.inner.expr().token_range.clone(),
            kind: THIRExpressionKind::StagedExpression(self.inner),
        }
    }
}

pub fn typecheck_staged_expr(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    inner: &HIRExpression
) -> CXResult<TypecheckResult> {
    todo!()
}