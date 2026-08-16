/// Linkage controls how a symbol is emitted and resolved across translation
/// units. It is shared by HIR, THIR, MIR, and code generation rather than
/// belonging to any one language representation.
#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
pub enum LinkageMode {
    #[default]
    Standard,
    Static,
    Extern,
}

impl std::fmt::Display for LinkageMode {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Standard => formatter.write_str("standard"),
            Self::Static => formatter.write_str("static"),
            Self::Extern => formatter.write_str("extern"),
        }
    }
}
