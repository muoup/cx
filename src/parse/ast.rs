use crate::lex::token::OperatorType;
use crate::parse::pass_bytecode::context::FunctionPrototype;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug)]
pub struct AST {
    pub statements: Vec<GlobalStatement>,
    pub public_interface: Vec<usize>
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarInitialization {
    pub name: String,
    pub type_: ValueType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Integer { bytes: u8, signed: bool },
    Float { bytes: u8 },
    Structured { fields: Vec<VarInitialization> },
    Unit,

    PointerTo(Box<ValueType>),
    Array {
        size: usize,
        _type: Box<ValueType>
    },
    Opaque {
        name: String,
        size: usize
    },

    Identifier(String)
}

impl Display for ValueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Integer { bytes, signed } => {
                let signed_str = if *signed { "i" } else { "u" };
                let signed_bytes = *bytes * 8;
                write!(f, "{}i{}", signed_str, signed_bytes)
            },
            ValueType::Float { bytes } => {
                let float_bytes = *bytes * 8;
                write!(f, "f{}", float_bytes)
            },
            ValueType::Structured { fields } => {
                let field_strs = fields.iter()
                    .map(|field| format!("{}: {}", field.name, field.type_))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{ {} }}", field_strs)
            },
            ValueType::Unit => write!(f, "()"),
            ValueType::PointerTo(inner) => {
                write!(f, "*{}", inner)
            },
            ValueType::Array { size, _type } => {
                write!(f, "[{}; {}]", size, _type)
            },
            ValueType::Opaque { name, size } => {
                write!(f, "OP_{}(\"{}\")", size, name)
            },
            ValueType::Identifier(name) => {
                write!(f, "{}", name)
            }
        }
    }
}

#[derive(Debug)]
pub enum GlobalStatement {
    Function {
        prototype: FunctionPrototype,
        body: Option<Vec<Expression>>
    },
    TemplatedFunction {
        prototype: FunctionPrototype,
        templated_types: Vec<String>,
        body: Vec<Expression>
    },
    MemberFunction {
        struct_parent: String,
        prototype: FunctionPrototype,
        body: Option<Vec<Expression>>
    },
    Enum {
        name: String,
        fields: Vec<(String, i32)>
    },
    TypeDeclaration {
        name: Option<String>,
        type_: ValueType,
    },
    GlobalVariable {
        name: String,
        type_: ValueType,
        value: Option<Expression>
    },
    Import {
        path: String
    },
    HandledInternally
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralExpression {
    IntLiteral {
        val: i64,
        bytes: u8
    },
    FloatLiteral {
        val: f64,
        bytes: u8
    },
    StringLiteral(String)
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntegerCastType {
    IReduce,
    SignExtend,
    ZeroExtend
}

#[derive(Debug, Clone, PartialEq)]
pub enum RValueExpression {
    Identifier(String),
    MemberFunctionCall {
        struct_parent: Box<Expression>,
        name: String,
        args: Vec<Expression>
    },
    DirectFunctionCall {
        name: String,
        args: Vec<Expression>
    },
    UnaryOperation {
        operator: OperatorType,
        operand: Option<Box<Expression>>
    },
    FunctionCall {
        func: Box<Expression>,
        args: Vec<Expression>
    },
    BinaryOperation {
        operator: OperatorType,
        left: Box<Expression>,
        right: Box<Expression>
    },
    Assignment {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: Option<OperatorType>
    },
    IntegerCast {
        expr: Box<Expression>,
        type_: ValueType,
        cast_type: IntegerCastType
    },
    FloatCast {
        expr: Box<Expression>,
        type_: ValueType
    },
    LoadedLValue {
        lvalue: Box<Expression>
    },
    StructuredInitializer {
        fields: Vec<(Option<String>, Expression)>
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ControlExpression {
    Return(Option<Box<Expression>>),
    Continue, Break,
    If {
        condition: Box<Expression>,
        then: Vec<Expression>,
        else_: Vec<Expression>
    },
    ForLoop {
        init: Box<Expression>,
        condition: Box<Expression>,
        increment: Box<Expression>,
        body: Vec<Expression>
    },
    Loop {
        condition: Box<Expression>,
        body: Vec<Expression>,

        evaluate_condition_first: bool
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValueExpression {
    Identifier(String),

    Initialization(VarInitialization),

    Variable {
        name: String,
        _type: ValueType
    },

    DereferencedPointer {
        pointer: Box<Expression>,
    },

    StructField {
        struct_: Box<Expression>,
        field_name: String
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(LiteralExpression),
    Control(ControlExpression),
    RValue(RValueExpression),
    LValue(LValueExpression),

    Unit,
}