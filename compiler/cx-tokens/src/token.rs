use std::{path::Path, sync::Arc};

use cx_util::namespace::EnvironmentNamespace;
use speedy::{Context, Readable, Reader, Writable, Writer};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokenRange {
    Internal,
    Error(String),
    Source {
        namespace: EnvironmentNamespace,
        start_token: usize,
        end_token: usize,
    },
}

impl TokenRange {
    pub fn new(
        start_token: usize,
        end_token: usize,
        namespace: impl Into<EnvironmentNamespace>,
    ) -> Self {
        Self::Source {
            namespace: namespace.into(),
            start_token,
            end_token,
        }
    }

    pub fn internal() -> Self {
        Self::Internal
    }

    pub fn error(message: impl Into<String>) -> Self {
        Self::Error(message.into())
    }

    pub fn namespace(&self) -> Option<&EnvironmentNamespace> {
        match self {
            Self::Source { namespace, .. } => Some(namespace),
            Self::Internal | Self::Error(_) => None,
        }
    }

    pub fn start_token(&self) -> Option<usize> {
        match self {
            Self::Source { start_token, .. } => Some(*start_token),
            Self::Internal | Self::Error(_) => None,
        }
    }

    pub fn end_token(&self) -> Option<usize> {
        match self {
            Self::Source { end_token, .. } => Some(*end_token),
            Self::Internal | Self::Error(_) => None,
        }
    }

    pub fn source_bounds(&self) -> Option<(&EnvironmentNamespace, usize, usize)> {
        match self {
            Self::Source {
                namespace,
                start_token,
                end_token,
            } => Some((namespace, *start_token, *end_token)),
            Self::Internal | Self::Error(_) => None,
        }
    }
}

impl<'a, C: Context> Readable<'a, C> for TokenRange {
    fn read_from<R: Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        match reader.read_u8()? {
            0 => Ok(TokenRange::Internal),
            1 => Ok(TokenRange::Error(String::read_from(reader)?)),
            2 => Ok(TokenRange::Source {
                namespace: EnvironmentNamespace::read_from(reader)?,
                start_token: reader.read_u64()? as usize,
                end_token: reader.read_u64()? as usize,
            }),
            _ => Ok(TokenRange::Error(
                "Invalid serialized token range".to_string(),
            )),
        }
    }
}

impl<C: Context> Writable<C> for TokenRange {
    fn write_to<T: ?Sized + Writer<C>>(&self, writer: &mut T) -> Result<(), C::Error> {
        match self {
            TokenRange::Internal => writer.write_u8(0),
            TokenRange::Error(message) => {
                writer.write_u8(1)?;
                message.write_to(writer)
            }
            TokenRange::Source {
                namespace,
                start_token,
                end_token,
            } => {
                writer.write_u8(2)?;
                namespace.write_to(writer)?;
                writer.write_u64(*start_token as u64)?;
                writer.write_u64(*end_token as u64)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,

    pub byte_start_index: usize,
    pub byte_end_index: usize,
    pub file_origin: Arc<Path>,
}

impl Token {
    pub fn new(kind: TokenKind, byte_range: (usize, usize), file_origin: Arc<Path>) -> Self {
        Self {
            kind,
            byte_start_index: byte_range.0,
            byte_end_index: byte_range.1,
            file_origin,
        }
    }

    pub fn new_unknown(kind: TokenKind) -> Token {
        Self {
            kind,
            byte_start_index: 0,
            byte_end_index: 0,
            file_origin: Arc::from(Path::new("")),
        }
    }
}

#[macro_export]
macro_rules! keyword {
    () => {
        cx_tokens::token::TokenKind::Keyword(_)
    };

    ($($name:ident),+) => {
        $(cx_tokens::token::TokenKind::Keyword(cx_tokens::token::KeywordType::$name))|+
    };
}

#[macro_export]
macro_rules! specifier {
    () => {
        cx_tokens::token::TokenKind::Specifier(_)
    };

    ($($name:ident),+) => {
        $(cx_tokens::token::TokenKind::Specifier(cx_tokens::token::SpecifierType::$name))|+
    }
}

#[macro_export]
macro_rules! intrinsic {
    () => {
        cx_tokens::token::TokenKind::Intrinsic(_)
    };

    ($name:ident) => {
        cx_tokens::token::TokenKind::Intrinsic(cx_tokens::token::IntrinsicType::$name)
    };
}

#[macro_export]
macro_rules! operator {
    () => {
        cx_tokens::token::TokenKind::Operator(_)
    };

    ($name:ident) => {
        cx_tokens::token::TokenKind::Operator(cx_tokens::token::OperatorType::$name)
    };
}

#[macro_export]
macro_rules! punctuator {
    () => {
        cx_tokens::token::TokenKind::Punctuator(_)
    };

    ($name:ident) => {
        cx_tokens::token::TokenKind::Punctuator(cx_tokens::token::PunctuatorType::$name)
    };
}

#[macro_export]
macro_rules! identifier {
    () => {
        cx_tokens::token::TokenKind::Identifier(_)
    };

    ($name:ident) => {
        cx_tokens::token::TokenKind::Identifier($name)
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    IncludeBegin,
    IncludeEnd,

    Assignment(Option<OperatorType>),
    Operator(OperatorType),

    Specifier(SpecifierType),
    Keyword(KeywordType),
    Intrinsic(IntrinsicType),
    Punctuator(PunctuatorType),

    Identifier(String),
    CompilerIdentifier(String),
    StringLiteral(String),
    IntLiteral(IntegerLiteral),
    FloatLiteral(FloatLiteral),
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum IntegerBase {
    Decimal,
    Octal,
    Hexadecimal,
    Binary,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum IntegerLength {
    Default,
    Long,
    LongLong,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct IntegerSuffix {
    pub unsigned: bool,
    pub length: IntegerLength,
}

impl Default for IntegerSuffix {
    fn default() -> Self {
        Self {
            unsigned: false,
            length: IntegerLength::Default,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct IntegerLiteral {
    pub magnitude: u64,
    pub base: IntegerBase,
    pub suffix: IntegerSuffix,
}

impl IntegerLiteral {
    pub const fn decimal(magnitude: u64) -> Self {
        Self {
            magnitude,
            base: IntegerBase::Decimal,
            suffix: IntegerSuffix {
                unsigned: false,
                length: IntegerLength::Default,
            },
        }
    }

    pub fn source_text(self) -> String {
        let value = match self.base {
            IntegerBase::Decimal => self.magnitude.to_string(),
            IntegerBase::Octal => format!("0{:o}", self.magnitude),
            IntegerBase::Hexadecimal => format!("0x{:x}", self.magnitude),
            IntegerBase::Binary => format!("0b{:b}", self.magnitude),
        };
        let unsigned = if self.suffix.unsigned { "u" } else { "" };
        let length = match self.suffix.length {
            IntegerLength::Default => "",
            IntegerLength::Long => "l",
            IntegerLength::LongLong => "ll",
        };
        format!("{value}{unsigned}{length}")
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum FloatSuffix {
    Default,
    Float,
    LongDouble,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct FloatLiteral {
    pub value: f64,
    pub suffix: FloatSuffix,
}

impl FloatLiteral {
    pub fn source_text(self) -> String {
        match self.suffix {
            FloatSuffix::Default => self.value.to_string(),
            FloatSuffix::Float => format!("{}f", self.value),
            FloatSuffix::LongDouble => format!("{}l", self.value),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum OperatorType {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    NotEqual,
    Less,
    Greater,
    Equal,
    LessEqual,
    GreaterEqual,

    DoubleAmpersand,
    DoubleBar,
    Exclamation,
    Ampersand,
    Bar,
    Caret,
    Tilda,
    Increment,
    Decrement,

    Comma,
    Access,
    ScopeRes,

    Move,
    Is,
    Pipe,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum PunctuatorType {
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Ellipsis,
    Colon,
    Period,
    QuestionMark,
    Hash,

    ThickArrow, /* (=>) */
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum KeywordType {
    If,
    Else,
    While,
    For,
    Do,
    Break,
    Continue,
    Return,
    Switch,
    Case,
    Default,

    Struct,
    Enum,
    Union,

    Typedef,

    Static,
    Extern,
    Const,
    Register,
    Volatile,
    Inline,
    Restrict,

    Sizeof,

    // CX Specific
    Import,
    As,
    Strong,
    Weak,
    Template,
    Class,
    Match,
    Yield,
    Comptime,
    Expr,
    Emit,
    Where,
    Safe,

    Precondition,
    Postcondition,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum IntrinsicType {
    Void,
    Bool,
    Char,
    Short,
    Int,
    Long,
    Auto,
    Float,
    Double,
    Unsigned,
    Signed,
    Complex,
}

impl IntrinsicType {
    pub fn as_str(&self) -> &'static str {
        match self {
            IntrinsicType::Void => "void",
            IntrinsicType::Bool => "bool",
            IntrinsicType::Char => "char",
            IntrinsicType::Short => "short",
            IntrinsicType::Int => "int",
            IntrinsicType::Long => "long",
            IntrinsicType::Auto => "auto",
            IntrinsicType::Float => "float",
            IntrinsicType::Double => "double",
            IntrinsicType::Unsigned => "unsigned",
            IntrinsicType::Signed => "signed",
            IntrinsicType::Complex => "_Complex",
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum SpecifierType {
    Const,
    Volatile,
    Restrict,
    Inline,
    Extern,
    Static,
    Public,
    Private,
    ThreadLocal,
}

impl TokenKind {
    pub fn from_str(str: String) -> TokenKind {
        match str.trim() {
            "if" => TokenKind::Keyword(KeywordType::If),
            "else" => TokenKind::Keyword(KeywordType::Else),
            "while" => TokenKind::Keyword(KeywordType::While),
            "for" => TokenKind::Keyword(KeywordType::For),
            "do" => TokenKind::Keyword(KeywordType::Do),
            "break" => TokenKind::Keyword(KeywordType::Break),
            "continue" => TokenKind::Keyword(KeywordType::Continue),
            "return" => TokenKind::Keyword(KeywordType::Return),
            "switch" => TokenKind::Keyword(KeywordType::Switch),
            "case" => TokenKind::Keyword(KeywordType::Case),
            "default" => TokenKind::Keyword(KeywordType::Default),
            "struct" => TokenKind::Keyword(KeywordType::Struct),
            "enum" => TokenKind::Keyword(KeywordType::Enum),
            "union" => TokenKind::Keyword(KeywordType::Union),
            "typedef" => TokenKind::Keyword(KeywordType::Typedef),

            "int" => TokenKind::Intrinsic(IntrinsicType::Int),
            "long" => TokenKind::Intrinsic(IntrinsicType::Long),
            "short" => TokenKind::Intrinsic(IntrinsicType::Short),
            "float" => TokenKind::Intrinsic(IntrinsicType::Float),
            "double" => TokenKind::Intrinsic(IntrinsicType::Double),
            "_Complex" | "__complex" | "__complex__" => {
                TokenKind::Intrinsic(IntrinsicType::Complex)
            }

            "char" => TokenKind::Intrinsic(IntrinsicType::Char),
            "void" => TokenKind::Intrinsic(IntrinsicType::Void),
            "auto" => TokenKind::Intrinsic(IntrinsicType::Auto),
            "unsigned" => TokenKind::Intrinsic(IntrinsicType::Unsigned),
            "signed" => TokenKind::Intrinsic(IntrinsicType::Signed),
            "register" => TokenKind::Keyword(KeywordType::Register),
            "sizeof" => TokenKind::Keyword(KeywordType::Sizeof),

            "public" => TokenKind::Specifier(SpecifierType::Public),
            "private" => TokenKind::Specifier(SpecifierType::Private),
            "volatile" => TokenKind::Specifier(SpecifierType::Volatile),
            "inline" => TokenKind::Specifier(SpecifierType::Inline),
            "extern" => TokenKind::Specifier(SpecifierType::Extern),
            "static" => TokenKind::Specifier(SpecifierType::Static),
            "restrict" => TokenKind::Specifier(SpecifierType::Restrict),
            "const" => TokenKind::Specifier(SpecifierType::Const),
            "thread_local" => TokenKind::Specifier(SpecifierType::ThreadLocal),

            // CX Extensions
            "import" => TokenKind::Keyword(KeywordType::Import),
            "as" => TokenKind::Keyword(KeywordType::As),

            "weak" => TokenKind::Keyword(KeywordType::Weak),
            "move" => TokenKind::Operator(OperatorType::Move),

            "template" => TokenKind::Keyword(KeywordType::Template),

            "class" => TokenKind::Keyword(KeywordType::Class),

            "match" => TokenKind::Keyword(KeywordType::Match),
            "yield" => TokenKind::Keyword(KeywordType::Yield),
            "comptime" => TokenKind::Keyword(KeywordType::Comptime),
            "expr" => TokenKind::Keyword(KeywordType::Expr),
            "emit" => TokenKind::Keyword(KeywordType::Emit),
            "is" => TokenKind::Operator(OperatorType::Is),

            "safe" => TokenKind::Keyword(KeywordType::Safe),
            "where" => TokenKind::Keyword(KeywordType::Where),
            "pre" => TokenKind::Keyword(KeywordType::Precondition),
            "post" => TokenKind::Keyword(KeywordType::Postcondition),

            _ if str.starts_with('@') => {
                TokenKind::CompilerIdentifier(str.trim_start_matches('@').to_string())
            }
            _ => TokenKind::Identifier(str),
        }
    }
}
