#[derive(Debug, PartialEq)]
pub enum OperatorType {
    Plus, Minus, Multiply, Divide, Modulo,
    Equal, Exclamation, LessThan, GreaterThan,
    Tilde, Ampersand, Pipe, Caret, Semicolon
}

#[derive(Debug, PartialEq)]
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

    Void, Bool, Char, Short, Int, Long, Auto,
    Float, Double,
    Unsigned, Signed,

    Static, Extern, Const, Register,
    Volatile, Inline, Restrict,

    Sizeof
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),

    Operator(OperatorType),
    Keyword(KeywordType),
    Punctuator(PunctuatorType),

    StringLiteral(String),
    IntLiteral(i64),
    FloatLiteral(f64),
}

impl Token {
    pub(crate) fn try_op(c: char) -> Option<Token> {
        match c {
            '+' => Some(Token::Operator(OperatorType::Plus)),
            '-' => Some(Token::Operator(OperatorType::Minus)),
            '*' => Some(Token::Operator(OperatorType::Multiply)),
            '/' => Some(Token::Operator(OperatorType::Divide)),
            '%' => Some(Token::Operator(OperatorType::Modulo)),
            '=' => Some(Token::Operator(OperatorType::Equal)),
            '!' => Some(Token::Operator(OperatorType::Exclamation)),
            '<' => Some(Token::Operator(OperatorType::LessThan)),
            '>' => Some(Token::Operator(OperatorType::GreaterThan)),
            '~' => Some(Token::Operator(OperatorType::Tilde)),
            '&' => Some(Token::Operator(OperatorType::Ampersand)),
            '|' => Some(Token::Operator(OperatorType::Pipe)),
            '^' => Some(Token::Operator(OperatorType::Caret)),
            _ => None,
        }
    }

    pub(crate) fn try_punc(c: char) -> Option<Token> {
        match c {
            '(' => Some(Token::Punctuator(PunctuatorType::OpenParen)),
            ')' => Some(Token::Punctuator(PunctuatorType::CloseParen)),
            '[' => Some(Token::Punctuator(PunctuatorType::OpenBracket)),
            ']' => Some(Token::Punctuator(PunctuatorType::CloseBracket)),
            '{' => Some(Token::Punctuator(PunctuatorType::OpenBrace)),
            '}' => Some(Token::Punctuator(PunctuatorType::CloseBrace)),
            ',' => Some(Token::Punctuator(PunctuatorType::Comma)),
            ';' => Some(Token::Punctuator(PunctuatorType::Semicolon)),
            ':' => Some(Token::Punctuator(PunctuatorType::Colon)),
            '.' => Some(Token::Punctuator(PunctuatorType::Period)),
            '?' => Some(Token::Punctuator(PunctuatorType::QuestionMark)),
            _ => None,
        }
    }

    pub(crate) fn try_number(str: &str) -> Option<(Token, usize)> {
        if str.is_empty() || !str.chars().next().unwrap().is_digit(10) {
            return None;
        }

        let mut end = 0;
        let mut dot = false;
        for (i, c) in str.chars().enumerate() {
            if c == '.' {
                if dot {
                    break;
                }
                dot = true;
            } else if !c.is_digit(10) {
                break;
            }
            end = i;
        }
        let num = &str[..=end];
        if dot {
            Some((Token::FloatLiteral(num.parse().unwrap()), end))
        } else {
            Some((Token::IntLiteral(num.parse().unwrap()), end))
        }
    }

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
            "int" => Token::Keyword(KeywordType::Int),
            "float" => Token::Keyword(KeywordType::Float),
            "char" => Token::Keyword(KeywordType::Char),
            "void" => Token::Keyword(KeywordType::Void),
            "auto" => Token::Keyword(KeywordType::Auto),
            "unsigned" => Token::Keyword(KeywordType::Unsigned),
            "signed" => Token::Keyword(KeywordType::Signed),
            "static" => Token::Keyword(KeywordType::Static),
            "extern" => Token::Keyword(KeywordType::Extern),
            "const" => Token::Keyword(KeywordType::Const),
            "register" => Token::Keyword(KeywordType::Register),
            "volatile" => Token::Keyword(KeywordType::Volatile),
            "inline" => Token::Keyword(KeywordType::Inline),
            "restrict" => Token::Keyword(KeywordType::Restrict),
            "sizeof" => Token::Keyword(KeywordType::Sizeof),
            _ => Token::Identifier(str),
        }
    }
}