pub enum OperatorType {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    BitwiseOR, BitwiseAND, BitwiseXOR, BitwiseNOT,
    LogicalOR, LogicalAND, LogicalNOT, LogicalXOR
}

pub enum KeywordType {
    If, Else, While, For, Break, Continue, Return, Switch,
    Case, Default, Struct, Enum
}

pub enum Token {
    Identifier(String),
    Keyword(String),

    StringLiteral(String),
    IntLiteral(i64),
    FloatLiteral(f64),

    Operator(OperatorType),
}