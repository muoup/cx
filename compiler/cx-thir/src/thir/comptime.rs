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

    breaks: bool,
    continues: bool,
    yields: Option<THIRType>,
    returns: Option<THIRType>,
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
            breaks: false,
            continues: false,
            yields: None,
            returns: None,
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
            breaks: self.breaks,
            continues: self.continues,
            yields: self.yields,
            returns: self.returns,
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

    pub fn add_break(&mut self) {
        self.breaks = true;
    }

    pub fn breaks(&self) -> bool {
        self.breaks
    }

    pub fn add_continue(&mut self) {
        self.continues = true;
    }

    pub fn continues(&self) -> bool {
        self.continues
    }

    pub fn set_yield(&mut self, ty: THIRType) {
        self.yields = Some(ty);
    }

    pub fn yields(&self) -> Option<&THIRType> {
        self.yields.as_ref()
    }

    pub fn set_return(&mut self, ty: THIRType) {
        self.returns = Some(ty);
    }

    pub fn returns(&self) -> Option<&THIRType> {
        self.returns.as_ref()
    }
}
