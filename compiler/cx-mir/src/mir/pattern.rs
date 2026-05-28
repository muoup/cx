use std::fmt::{Display, Formatter, Result};

use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};
use speedy::{Readable, Writable};

use crate::mir::data::{MIRFloatType, MIRType};

#[derive(Clone, Debug, Readable, Writable)]
pub enum MIRPattern {
    Integer(i64),
    Float(FloatWrapper, MIRFloatType),
    TaggedUnionVariant {
        sum_type: MIRType,
        variant_index: usize,
        inner_name: Option<CXIdent>,
    },
}

impl Display for MIRPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            MIRPattern::Integer(value) => write!(f, "{value}"),
            MIRPattern::Float(value, _type) => write!(f, "{_type} {value}"),
            MIRPattern::TaggedUnionVariant {
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
