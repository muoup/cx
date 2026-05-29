use speedy::{Readable, Writable};

pub type CXTypeQualifiers = u8;

pub const CX_CONST: CXTypeQualifiers = 1 << 0;
pub const CX_VOLATILE: CXTypeQualifiers = 1 << 1;
pub const CX_RESTRICT: CXTypeQualifiers = 1 << 2;
pub const CX_THREAD_LOCAL: CXTypeQualifiers = 1 << 3;
pub const CX_UNION: CXTypeQualifiers = 1 << 4;

#[derive(Debug, Default, Hash, Clone, PartialOrd, PartialEq, Eq, Copy, Readable, Writable)]
pub enum VisibilityMode {
    #[default]
    Private,
    Package,
    Public,
}

#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq, Readable, Writable)]
pub enum CXLinkageMode {
    #[default]
    Standard,
    Static,
    Extern,
}

impl From<cx_preparse_data::VisibilityMode> for VisibilityMode {
    fn from(value: cx_preparse_data::VisibilityMode) -> Self {
        match value {
            cx_preparse_data::VisibilityMode::Private => Self::Private,
            cx_preparse_data::VisibilityMode::Package => Self::Package,
            cx_preparse_data::VisibilityMode::Public => Self::Public,
        }
    }
}