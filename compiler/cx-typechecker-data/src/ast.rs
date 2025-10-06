use crate::cx_types::{CXFunctionPrototype, CXType};
use crate::function_map::{CXFnData, CXFnMap};
use crate::{CXTypeData, CXTypeMap};
use cx_parsing_data::parse::ast::{CXBinOp, CXCastType, CXUnOp};
use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};
use std::collections::HashMap;

#[derive(Debug, Clone, Readable, Writable)]
pub struct TCBaseMappings {
    pub type_data: CXTypeData,
    pub fn_map: CXFnData,
    pub global_variables: HashMap<String, TCGlobalVariable>,
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct TCAST {
    pub source_file: String,

    pub type_map: CXTypeMap,
    pub fn_map: CXFnMap,

    pub destructors_required: Vec<CXType>,
    pub global_variables: Vec<TCGlobalVariable>,
    pub function_defs: Vec<TCFunctionDef>,
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct FunctionTemplateRequest {
    pub module_origin: Option<CXIdent>,
    pub name: CXIdent,
    pub type_arguments: Vec<CXType>,
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct TCFunctionDef {
    pub prototype: CXFunctionPrototype,
    pub body: Box<TCExpr>,
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum TCGlobalVariable {
    // Currently used with enum constants
    UnaddressableConstant {
        name: CXIdent,
        val: i64,
    },
    StringLiteral {
        name: CXIdent,
        value: String,
    },
    Variable {
        name: CXIdent,
        _type: CXType,
        initializer: Option<i64>,
    },
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct TCExpr {
    pub _type: CXType,
    pub kind: TCExprKind,
}

impl Default for TCExpr {
    fn default() -> Self {
        TCExpr {
            _type: CXType::default(),
            kind: TCExprKind::Taken,
        }
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum TCExprKind {
    Taken,
    Unit,

    Block {
        statements: Vec<TCExpr>,
    },

    IntLiteral {
        value: i64,
    },

    FloatLiteral {
        value: f64,
    },

    SizeOf {
        _type: CXType,
    },

    VariableDeclaration {
        type_: CXType,
        name: CXIdent,
    },

    GlobalVariableReference {
        name: CXIdent,
    },

    VariableReference {
        name: CXIdent,
    },

    FunctionReference,

    MemberFunctionReference {
        target: Box<TCExpr>,
    },

    FunctionCall {
        function: Box<TCExpr>,
        arguments: Vec<TCExpr>,
        direct_call: bool,
    },

    Access {
        struct_type: CXType,
        target: Box<TCExpr>,
        field: CXIdent,
    },

    Assignment {
        target: Box<TCExpr>,
        value: Box<TCExpr>,
        additional_op: Option<CXBinOp>,
    },

    BinOp {
        lhs: Box<TCExpr>,
        rhs: Box<TCExpr>,
        op: CXBinOp,
    },

    UnOp {
        operand: Box<TCExpr>,
        operator: CXUnOp,
    },

    If {
        condition: Box<TCExpr>,
        then_branch: Box<TCExpr>,
        else_branch: Option<Box<TCExpr>>,
    },

    While {
        condition: Box<TCExpr>,
        body: Box<TCExpr>,
        pre_eval: bool,
    },

    For {
        init: Box<TCExpr>,
        condition: Box<TCExpr>,
        increment: Box<TCExpr>,
        body: Box<TCExpr>,
    },

    CSwitch {
        condition: Box<TCExpr>,
        block: Vec<TCExpr>,
        cases: Vec<(u64, usize)>, // (case value, index of the case body)
        default_case: Option<usize>,
    },

    Match {
        condition: Box<TCExpr>,
        cases: Vec<TCTagMatch>, // (tag value, case body)
        default_case: Option<Box<TCExpr>>,
    },

    ConstructorMatchIs {
        expr: Box<TCExpr>,

        union_type: CXType,
        variant_type: CXType,
        variant_tag: u64,

        var_name: CXIdent,
    },

    ImplicitLoad {
        operand: Box<TCExpr>,
    },

    TemporaryBuffer {
        _type: CXType,
    },

    Coercion {
        operand: Box<TCExpr>,
        cast_type: CXCastType,
    },

    Defer {
        operand: Box<TCExpr>,
    },

    New {
        _type: CXType,
        array_length: Option<Box<TCExpr>>,
    },

    Move {
        operand: Box<TCExpr>,
    },

    Return {
        value: Option<Box<TCExpr>>,
    },
    
    BufferReturn {
        value: Box<TCExpr>,
    },

    InitializerList {
        indices: Vec<TCInitIndex>,
    },
    
    Copy {
        expr: Box<TCExpr>,
    },

    TypeConstructor {
        name: CXIdent,

        union_type: CXType,
        variant_type: CXType,
        variant_index: usize,

        input: Box<TCExpr>,
    },

    Break,
    Continue,
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct TCInitIndex {
    pub name: Option<String>,
    pub value: TCExpr,
    pub index: usize,
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct TCTagMatch {
    pub tag_value: u64,
    pub body: Box<TCExpr>,
    pub variant_type: CXType,
    pub instance_name: CXIdent,
}
