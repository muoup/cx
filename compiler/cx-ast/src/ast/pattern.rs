use crate::ast::template::CXTemplateInput;
use cx_util::{identifier::CXIdent, namespace::QualifiedName, unsafe_float::FloatWrapper};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CXPattern {
    Binding(CXIdent),

    Integer(i64),
    Float(FloatWrapper),
    Variant {
        constructor: QualifiedName,
        template_input: Option<CXTemplateInput>,
        inner: Option<Box<CXPattern>>,
    },
}
