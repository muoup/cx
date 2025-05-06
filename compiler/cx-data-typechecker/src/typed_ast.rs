use cx_data_ast::parse::ast::{CXParameter, FunctionMap, TypeMap};
use cx_data_ast::parse::value_type::CXValType;
use crate::operator::CXIntBinOp;

pub struct CXTypedAST {
    pub func_map: FunctionMap,
    pub type_map: TypeMap,

    pub function_declarations: Vec<CXTypedFunction>,
}

pub struct CXTypedFunction {
    pub name: String,
    pub params: Vec<CXParameter>,
    pub return_type: CXValType,
    pub body: CXTypedExpr,
}

pub struct CXTypedExpr {
    pub expr: CXTypedExpr,
    pub type_: CXValType,
}

pub enum CXTypedEForm {
    VariableReference {
        name: String,
        type_: CXValType,
    },
    VariableDeclaration {
        name: String,
        type_: CXValType,
    },

    IntegerBinOp {
        left: Box<CXTypedExpr>,
        right: Box<CXTypedExpr>,
        op: CXIntBinOp,
    },

    FloatBinOp {
        left: Box<CXTypedExpr>,
        right: Box<CXTypedExpr>,
        op: CXIntBinOp,
    },

    StringLiteral {
        content: String,
    },

    Literal {
        val: u64,
    },

    DirectCall {
        method_name: String,
        args: Vec<CXTypedExpr>,
    },

    IndirectCall {
        method_ref: Box<CXTypedExpr>,
        args: Vec<CXTypedExpr>,
    },

    ImplicitCast {
        value: Box<CXTypedExpr>,
        type_: CXValType,
    },

    ImplicitLoad {
        value: Box<CXTypedExpr>,
        type_: CXValType,
    }
}
