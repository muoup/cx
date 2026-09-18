use crate::ast::template::HIRTemplateInput;
use cx_namespace::module::QualifiedName;
use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HIRPattern {
    Binding(CXIdent),

    Integer(i64),
    Float(FloatWrapper),
    Variant {
        constructor: QualifiedName,
        template_input: Option<HIRTemplateInput>,
        inner: Option<Box<HIRPattern>>,
    },
}
