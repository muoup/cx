use chl_tokens::TokenRange;
use chl_util::{identifier::CHLIdent, unsafe_float::FloatWrapper};

use crate::ast::expression::HIRExpression;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HIRBindingMode {
    Owned,
    Reference,
    ConstReference,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HIRPattern {
    Wildcard,
    Binding {
        name: CHLIdent,
        mode: HIRBindingMode,
        span: TokenRange,
    },

    Integer(i128),
    Float(FloatWrapper),
    // An existing value the subject is compared against
    Value(HIRExpression),
    // Resolved in the subject's type unless a qualifier names the sum
    Variant {
        qualifier: Option<Box<HIRExpression>>,
        name: CHLIdent,
        inner: Option<Box<HIRPattern>>,
    },
}
