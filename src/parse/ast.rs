use crate::lex::token::OperatorType;
use crate::parse::verify::{ValueTypeRef};
use std::fmt::Debug;

pub struct AST {
    pub root: Root
}

#[derive(Debug)]
pub struct Root {
    pub global_stmts: Vec<GlobalStatement>,
}

pub struct StructDefinition {
    pub name: Option<String>,
    pub fields: Vec<(String, ValueTypeRef)>
}

#[derive(Debug)]
pub enum GlobalStatement {
    Function {
        name: String,
        arguments: Vec<Expression>,
        return_type: Expression,
        body: Option<Vec<Expression>>
    },

    // Both typedef and non-typedef declarations will be squeezed into this variant
    // C++ allows implicit typedef, so why shouldn't we?
    TypeDeclaration {
        name: Option<String>,
        type_: ValueTypeRef,
    },
    GlobalVariable {
        name: String,
        type_: ValueTypeRef,
        value: Option<Expression>
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum ValueExpression {
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
    VariableReference {
        name: String,
        lval_type: ValueTypeRef
    },
}

#[derive(Debug, Clone)]
pub enum ControlExpression {
    Return(Box<Expression>),
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

#[derive(Debug, Clone)]
pub enum UnverifiedExpression {
    Identifier(String),

    CompoundIdentifier {
        identifier: String,
        suffix: Box<Expression>
    },

    Cast {
        expr: Box<Expression>,
        type_: String
    },

    FunctionCall {
        name: Box<Expression>,
        args: Vec<Expression>
    },

    UnaryOperation {
        operator: OperatorType,
        operand: Box<Expression>
    },

    BinaryOperation {
        operator: OperatorType,
        left: Box<Expression>,
        right: Box<Expression>
    }
}

#[derive(Debug, Clone)]
pub enum ValueType {
    Integer { bytes: u8, signed: bool },
    Float { bytes: u8 },
    Structured { fields: Vec<(String, ValueTypeRef)> },
    Unit,

    Standard(ValueTypeRef),
    PointerTo(Box<ValueType>),
    Array {
        size: usize,
        type_: Box<ValueType>
    }
}

#[derive(Debug, Clone)]
pub enum LValueExpression {
    Alloca {
        name: String,
        type_: ValueTypeRef,
    },
    Value {
        name: String,
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(LiteralExpression),
    Value(ValueExpression),
    Control(ControlExpression),
    LValue(LValueExpression),

    // Any expression that should be eliminated by the type checker,
    // if the codegen phase encounters this, behavior is undefined (likely a panic)
    Unverified(UnverifiedExpression),

    NOP,
    Unit,
}