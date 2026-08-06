use std::fmt::{Display, Formatter, Result};

use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::thir::{
    data::{THIRFloatType, THIRType},
    expression::THIRLocalID,
};

#[derive(Clone, Debug)]
pub enum THIRPattern {
    Integer(i64),
    Float(FloatWrapper, THIRFloatType),
    TaggedUnionVariant {
        sum_type: THIRType,
        variant_index: usize,
        inner_name: Option<CXIdent>,
        inner_local_id: Option<THIRLocalID>,
    },
}

impl Display for THIRPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            THIRPattern::Integer(value) => write!(f, "{value}"),
            THIRPattern::Float(value, _type) => write!(f, "{_type} {value}"),
            THIRPattern::TaggedUnionVariant {
                variant_index,
                inner_name,
                ..
            } => {
                write!(f, "variant {variant_index}")?;
                if let Some(inner_name) = inner_name {
                    write!(f, " (bind {inner_name})")?;
                }
                Ok(())
            }
        }
    }
}
