use cx_util::{identifier::CXIdent, namespace::QualifiedName, unsafe_float::FloatWrapper};
use speedy::{Readable, Writable};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub enum CXPattern {
    Binding(CXIdent),

    Integer(i64),
    Float(FloatWrapper),
    Variant {
        union_name: QualifiedName,
        variant_name: CXIdent,
        inner: Option<Box<CXPattern>>,
    },
}
