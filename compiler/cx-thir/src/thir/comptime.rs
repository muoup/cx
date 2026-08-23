use crate::thir::{
    data::THIRComptimeFnPrototype,
    expression::THIRExpression,
};

#[derive(Debug, Clone)]
pub struct THIRComptimeFn {
    pub prototype: THIRComptimeFnPrototype,
    pub body: Option<THIRExpression>,
}
