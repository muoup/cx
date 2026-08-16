use speedy::{Readable, Writable};

pub use cx_util::linkage::LinkageMode;

pub type HIRTypeQualifiers = u8;

pub const HIR_CONST: HIRTypeQualifiers = 1 << 0;
pub const HIR_VOLATILE: HIRTypeQualifiers = 1 << 1;
pub const HIR_RESTRICT: HIRTypeQualifiers = 1 << 2;
pub const HIR_THREAD_LOCAL: HIRTypeQualifiers = 1 << 3;
pub const HIR_UNION: HIRTypeQualifiers = 1 << 4;

pub use cx_preparse_data::VisibilityMode;

#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq, Readable, Writable)]
pub enum HIRSymbolNameScheme {
    #[default]
    Namespaced,
    Unmangled,
}
