use crate::parse::ast::{Expression, ValueType};
use crate::parse::verify::context::VerifyContext;

pub(crate) fn verify_expression(context: &mut VerifyContext, expr: &mut Expression) -> Option<ValueType> {
    Some(ValueType::Unit)
}