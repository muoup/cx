#[derive(Debug, PartialEq, Copy, Clone)]
pub enum OperatorType {
    Add, Subtract, Multiply, Divide, Modulo,

    NotEqual, Less, Greater, Equal, LessEqual, GreaterEqual,

    LAnd, LOr, LNot, BitAnd, BitOr, BitXor, BitNot,
    LShift, RShift,

    Increment, Decrement,

    ArrayIndex,

    Access, PointerAccess, AddressOf, /* Dereference = Multiply */
    ScopeRes
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum PunctuatorType {
    OpenParen, CloseParen,
    OpenBracket, CloseBracket,
    OpenBrace, CloseBrace,
    Comma, Semicolon,
    Colon, Period,
    QuestionMark
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum KeywordType {
    If, Else,
    While, For, Do,
    Break, Continue, Return,
    Switch, Case, Default,

    Struct, Enum, Union,

    Typedef,

    Static, Extern, Const, Register,
    Volatile, Inline, Restrict,

    Sizeof,

    // CX Specific
    Import,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum IntrinsicType {
    Void, Bool, Char, Short, Int, Long, Auto,
    Float, Double,
    Unsigned, Signed,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum SpecifierType {
    Const, Volatile, Restrict,
    Inline, Extern, Static,
    Public, Private,
    ThreadLocal
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
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

impl Token {
    pub(crate) fn from_str(str: String) -> Token {
        match str.trim() {
            "if" => Token::Keyword(KeywordType::If),
            "else" => Token::Keyword(KeywordType::Else),
            "while" => Token::Keyword(KeywordType::While),
            "for" => Token::Keyword(KeywordType::For),
            "do" => Token::Keyword(KeywordType::Do),
            "break" => Token::Keyword(KeywordType::Break),
            "continue" => Token::Keyword(KeywordType::Continue),
            "return" => Token::Keyword(KeywordType::Return),
            "switch" => Token::Keyword(KeywordType::Switch),
            "case" => Token::Keyword(KeywordType::Case),
            "default" => Token::Keyword(KeywordType::Default),
            "struct" => Token::Keyword(KeywordType::Struct),
            "enum" => Token::Keyword(KeywordType::Enum),
            "union" => Token::Keyword(KeywordType::Union),
            "typedef" => Token::Keyword(KeywordType::Typedef),
            "int" => Token::Intrinsic(IntrinsicType::Int),
            "float" => Token::Intrinsic(IntrinsicType::Float),
            "char" => Token::Intrinsic(IntrinsicType::Char),
            "void" => Token::Intrinsic(IntrinsicType::Void),
            "auto" => Token::Intrinsic(IntrinsicType::Auto),
            "unsigned" => Token::Intrinsic(IntrinsicType::Unsigned),
            "signed" => Token::Intrinsic(IntrinsicType::Signed),
            "register" => Token::Keyword(KeywordType::Register),
            "sizeof" => Token::Keyword(KeywordType::Sizeof),

            "public" => Token::Specifier(SpecifierType::Public),
            "private" => Token::Specifier(SpecifierType::Private),
            "volatile" => Token::Specifier(SpecifierType::Volatile),
            "inline" => Token::Specifier(SpecifierType::Inline),
            "extern" => Token::Specifier(SpecifierType::Extern),
            "static" => Token::Specifier(SpecifierType::Static),
            "restrict" => Token::Specifier(SpecifierType::Restrict),
            "const" => Token::Specifier(SpecifierType::Const),
            "thread_local" => Token::Specifier(SpecifierType::ThreadLocal),

            _ => Token::Identifier(str),
        }
    }
}

impl OperatorType {
    pub(crate) fn precedence(&self) -> i32 {
        match self {
            OperatorType::LAnd => 2,
            OperatorType::LOr => 3,
            OperatorType::Equal | OperatorType::NotEqual => 4,
            OperatorType::Less | OperatorType::LessEqual | OperatorType::Greater | OperatorType::GreaterEqual => 5,
            OperatorType::Add | OperatorType::Subtract => 6,
            OperatorType::Multiply | OperatorType::Divide | OperatorType::Modulo => 7,
            OperatorType::LShift | OperatorType::RShift => 8,
            OperatorType::BitAnd | OperatorType::BitXor | OperatorType::BitOr => 9,
            OperatorType::LNot | OperatorType::BitNot | OperatorType::Less | OperatorType::Greater => 10,
            OperatorType::BitAnd | OperatorType::BitOr | OperatorType::BitXor => 11,

            _ => 100
        }
    }
}