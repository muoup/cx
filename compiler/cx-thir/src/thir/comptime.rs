use cx_log::CXResult;
use cx_util::identifier::CXIdent;

use crate::thir::{
    data::{THIRComptimeFnPrototype, THIRFunctionBody, THIRType},
    expression::{THIRExpression, THIRLocalID},
};

#[derive(Debug, Clone)]
pub struct THIRComptimeFn {
    prototype: THIRComptimeFnPrototype,
    body: Option<THIRFunctionBody>,
    context: THIRStagingContext,
}

impl THIRComptimeFn {
    pub fn new(
        prototype: THIRComptimeFnPrototype,
        body: Option<THIRFunctionBody>,
        context: THIRStagingContext,
    ) -> Self {
        Self {
            prototype,
            body,
            context,
        }
    }

    pub fn prototype(&self) -> &THIRComptimeFnPrototype {
        &self.prototype
    }

    pub fn body(&self) -> Option<&THIRFunctionBody> {
        self.body.as_ref()
    }

    pub fn context(&self) -> &THIRStagingContext {
        &self.context
    }
}

#[derive(Debug, Clone, Default)]
pub struct THIRStagingContext {
    return_type: Option<THIRType>,
    yield_type: Option<THIRType>,
}

impl THIRStagingContext {
    pub fn new(return_type: Option<THIRType>, yield_type: Option<THIRType>) -> Self {
        Self {
            return_type,
            yield_type,
        }
    }

    pub fn return_type(&self) -> Option<&THIRType> {
        self.return_type.as_ref()
    }

    pub fn yield_type(&self) -> Option<&THIRType> {
        self.yield_type.as_ref()
    }

    pub fn set_yield_type(&mut self, yield_type: Option<THIRType>) {
        self.yield_type = yield_type;
    }
}

#[derive(Debug, Clone)]
pub struct THIRStagedExpr {
    expr: Box<THIRExpression>,
    params: Vec<THIRStagedParameter>,
    captures: Vec<THIRLocalID>,
}

#[derive(Debug, Clone)]
pub struct THIRStagedParameter {
    name: CXIdent,
    local_id: THIRLocalID,
    ty: THIRType,
}

impl THIRStagedParameter {
    pub fn new(name: CXIdent, local_id: THIRLocalID, ty: THIRType) -> Self {
        Self { name, local_id, ty }
    }

    pub fn name(&self) -> &CXIdent {
        &self.name
    }

    pub fn local_id(&self) -> THIRLocalID {
        self.local_id
    }

    pub fn ty(&self) -> &THIRType {
        &self.ty
    }
}

impl THIRStagedExpr {
    pub fn new(expr: Box<THIRExpression>) -> Self {
        Self {
            expr,
            params: vec![],
            captures: vec![],
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
            captures: self.captures,
        })
    }

    pub fn expr(&self) -> &THIRExpression {
        &self.expr
    }

    pub fn add_params(&mut self, params: Vec<THIRStagedParameter>) {
        self.params.extend(params);
    }

    pub fn set_captures(&mut self, captures: Vec<THIRLocalID>) {
        self.captures = captures;
    }

    pub fn params(&self) -> &[THIRStagedParameter] {
        &self.params
    }

    pub fn captures(&self) -> &[THIRLocalID] {
        &self.captures
    }
}
