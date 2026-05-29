use speedy::{Readable, Writable};

pub type CXTypeQualifiers = u8;

pub const CX_CONST: CXTypeQualifiers = 1 << 0;
pub const CX_VOLATILE: CXTypeQualifiers = 1 << 1;
pub const CX_RESTRICT: CXTypeQualifiers = 1 << 2;
pub const CX_THREAD_LOCAL: CXTypeQualifiers = 1 << 3;
pub const CX_UNION: CXTypeQualifiers = 1 << 4;

pub use cx_preparse_data::VisibilityMode;

#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq, Readable, Writable)]
pub enum CXLinkageMode {
    #[default]
    Standard,
    Static,
    Extern,
}