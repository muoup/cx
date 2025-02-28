use crate::lex::token::OperatorType;
use std::fmt::Debug;
use std::rc::Rc;
use crate::parse::verify::context::FunctionPrototype;

#[derive(Debug)]
pub struct AST {
    pub statements: Vec<GlobalStatement>
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

    Identifier(String)
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefinition {
    pub name: Option<String>,
    pub fields: Vec<(String, ValueType)>
}

#[derive(Debug)]
pub enum FirstPassGlobals {
    Struct {
        name: String,
        fields: Vec<(String, ValueType)>
    }
}

#[derive(Debug)]
pub enum SecondPassGlobals {
    Function {
        prototype: Rc<FunctionPrototype>,
        body: Option<Vec<Expression>>
    },
}

#[derive(Debug)]
pub enum ThirdPassGlobals {
    GlobalExpression(Expression)
}

#[derive(Debug)]
pub enum GlobalStatement {
    Function {
        prototype: FunctionPrototype,
        body: Option<Vec<Expression>>
    },
    TypeDeclaration {
        name: Option<String>,
        type_: ValueType,
    },
    GlobalVariable {
        name: String,
        type_: ValueType,
        value: Option<Expression>
    }
}

#[derive(Debug)]
pub enum UnverifiedGlobalStatement {
    Function {
        return_type: ValueType,
        name_header: Expression,
        params: Vec<Expression>,
        body: Option<Vec<Expression>>
    },

    StructDeclaration {
        name: String,
        field_declarations: Vec<Expression>,
    },

    GlobalVariable {
        left: Expression,
        value: Option<Expression>
    }
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