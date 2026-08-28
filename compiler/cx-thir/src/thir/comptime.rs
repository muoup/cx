use cx_log::CXResult;
use cx_util::identifier::CXIdent;

use crate::thir::{
    data::{THIRComptimeFnPrototype, THIRType},
    expression::{THIRExpression, THIRLocalID},
};

#[derive(Debug, Clone)]
pub struct THIRComptimeFn {
    pub prototype: THIRComptimeFnPrototype,
    pub body: Option<THIRExpression>,
}

#[derive(Debug, Clone)]
pub struct THIRStagedExpr {
    expr: Box<THIRExpression>,
    params: Vec<THIRStagedParameter>,
    effects: THIRStagedEffects,
}

#[derive(Debug, Clone, Default)]
pub struct THIRStagedEffects {
    pub breaks: bool,
    pub continues: bool,
    pub yield_type: Option<THIRType>,
}

#[derive(Debug, Clone)]
pub struct THIRStagedParameter {
    pub name: CXIdent,
    pub local_id: THIRLocalID,
    pub ty: THIRType,
}

impl THIRStagedExpr {
    pub fn new(expr: Box<THIRExpression>) -> Self {
        Self {
            expr,
            params: vec![],
            effects: THIRStagedEffects::default(),
        }
    }

    pub fn map_expr<F>(self, f: F) -> CXResult<Self>
    where
        F: FnOnce(THIRExpression) -> CXResult<THIRExpression>,
    {
        let expr = f(*self.expr)?;

        Ok(Self {
            expr: Box::new(expr),
            params: self.params,
            effects: self.effects,
        })
    }

    pub fn expr(&self) -> &THIRExpression {
        &self.expr
    }

    pub fn add_params(&mut self, params: Vec<THIRStagedParameter>) {
        self.params.extend(params);
    }

    pub fn params(&self) -> &[THIRStagedParameter] {
        &self.params
    }

    pub fn set_effects(&mut self, effects: THIRStagedEffects) {
        self.effects = effects;
    }

    pub fn effects(&self) -> &THIRStagedEffects {
        &self.effects
    }
}
