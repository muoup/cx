use crate::lex::token::OperatorType;
use std::fmt::Debug;
use std::rc::Rc;
use crate::parse::verify::context::FunctionPrototype;

#[derive(Debug)]
pub struct UnverifiedAST {
    pub statements: Vec<UnverifiedGlobalStatement>
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Integer { bytes: u8, signed: bool },
    Float { bytes: u8 },
    Structured { fields: Rc<[(String, ValueType)]> },
    Unit,

    PointerTo(Rc<ValueType>),
    Array {
        size: usize,
        type_: Rc<ValueType>
    },

    Unverified(String)
}

pub struct StructDefinition {
    pub name: Option<String>,
    pub fields: Vec<(String, ValueType)>
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameter {
    pub name: String,
    pub type_: ValueType
}

#[derive(Debug)]
pub enum GlobalStatement {
    Function {
        prototype: Rc<FunctionPrototype>,
        body: Option<Vec<Expression>>
    },

    // Both typedef and non-typedef declarations will be squeezed into this variant
    // C++ allows implicit typedef, so why shouldn't we?
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
        return_type: Expression,
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
    StructFieldReference {
        struct_: Box<Expression>,
        field: String,
        field_type: ValueType
    },
    VariableReference {
        name: String,
        lval_type: ValueType
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

    CompoundExpression {
        prefix: Box<Expression>,
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
pub enum LValueExpression {
    Alloca {
        name: String,
        type_: ValueType,
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