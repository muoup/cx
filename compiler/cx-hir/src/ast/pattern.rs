use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

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
        name: CXIdent,
        mode: HIRBindingMode,
    },

    Integer(i64),
    Float(FloatWrapper),
    // An existing value the subject is compared against
    Value(HIRExpression),
    // Resolved in the subject's type unless a qualifier names the sum
    Variant {
        qualifier: Option<Box<HIRExpression>>,
        name: CXIdent,
        inner: Option<Box<HIRPattern>>,
    },
}
