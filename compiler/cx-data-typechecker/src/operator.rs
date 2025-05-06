pub enum CXIntBinOp {
    Add, Sub, Mul, Div, Mod,
    BitAnd, BitOr, BitXor,
    Shl, Shr,

    LogicalAnd, LogicalOr,

    Equal, NotEqual,
    Less, Greater, LessEqual, GreaterEqual
}

pub enum CXFloatBinOp {
    Add, Sub, Mul, Div,

    Equal, NotEqual,
    Less, Greater, LessEqual, GreaterEqual
}