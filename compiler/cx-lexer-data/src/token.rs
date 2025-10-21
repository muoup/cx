use std::sync::Arc;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,

    pub line: u32,
    pub start_index: usize,
    pub end_index: usize,
    pub file_origin: Arc<str>
}

#[macro_export]
macro_rules! keyword {
    () => {
        cx_lexer_data::token::TokenKind::Keyword(_)
    };

    ($($name:ident),+) => {
        $(cx_lexer_data::token::TokenKind::Keyword(cx_lexer_data::token::KeywordType::$name))|+
    };
}

#[macro_export]
macro_rules! specifier {
    () => {
        cx_lexer_data::token::TokenKind::Specifier(_)
    };

    ($($name:ident),+) => {
        $(cx_lexer_data::token::TokenKind::Specifier(cx_lexer_data::token::SpecifierType::$name))|+
    }
}

#[macro_export]
macro_rules! intrinsic {
    () => {
        cx_lexer_data::token::TokenKind::Intrinsic(_)
    };

    ($name:ident) => {
        cx_lexer_data::token::TokenKind::Intrinsic(cx_lexer_data::token::IntrinsicType::$name)
    };
}

#[macro_export]
macro_rules! operator {
    () => {
        cx_lexer_data::token::TokenKind::Operator(_)
    };

    ($name:ident) => {
        cx_lexer_data::token::TokenKind::Operator(cx_lexer_data::token::OperatorType::$name)
    };
}

#[macro_export]
macro_rules! punctuator {
    () => {
        cx_lexer_data::token::TokenKind::Punctuator(_)
    };

    ($name:ident) => {
        cx_lexer_data::token::TokenKind::Punctuator(cx_lexer_data::token::PunctuatorType::$name)
    };
}

#[macro_export]
macro_rules! identifier {
    () => {
        cx_lexer_data::token::TokenKind::Identifier(_)
    };

    ($name:ident) => {
        cx_lexer_data::token::TokenKind::Identifier($name)
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Assignment(Option<OperatorType>),
    Operator(OperatorType),

    Specifier(SpecifierType),
    Keyword(KeywordType),
    Intrinsic(IntrinsicType),
    Punctuator(PunctuatorType),

    Identifier(String),
    StringLiteral(String),
    IntLiteral(i64),
    FloatLiteral(f64),
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
    DoubleLT,
    DoubleGT,

    Increment,
    Decrement,

    Comma,
    ArrayIndex,
    Access,
    ScopeRes,

    Move,
    Is,
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
    Defer,
    Strong,
    Weak,
    New,
    Template,
    Type,
    Class,
    Match,
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
            "defer" => TokenKind::Keyword(KeywordType::Defer),

            "strong" => TokenKind::Keyword(KeywordType::Strong),
            "weak" => TokenKind::Keyword(KeywordType::Weak),
            "move" => TokenKind::Operator(OperatorType::Move),

            "new" => TokenKind::Keyword(KeywordType::New),

            "template" => TokenKind::Keyword(KeywordType::Template),
            "type" => TokenKind::Keyword(KeywordType::Type),

            "class" => TokenKind::Keyword(KeywordType::Class),

            "match" => TokenKind::Keyword(KeywordType::Match),
            "is" => TokenKind::Operator(OperatorType::Is),

            _ => TokenKind::Identifier(str),
        }
    }
}
