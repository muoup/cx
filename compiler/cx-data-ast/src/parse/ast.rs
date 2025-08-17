use crate::parse::identifier::CXIdent;
use crate::parse::maps::{CXDestructorMap, CXFunctionMap, CXTypeMap};
use crate::parse::value_type::CXType;
use std::collections::HashMap;
use speedy::{Readable, Writable};
use uuid::Uuid;
use crate::parse::CXFunctionIdentifier;
use crate::parse::template::CXTemplateInput;
use crate::preparse::pp_type::{CXNaiveTemplateInput, CXNaiveType};

#[derive(Debug, Default)]
pub struct CXAST {
    // Path to .cx file
    pub file_path: String,

    // Prefix for internal paths (i.e. {internal_path}.[o|cx-types|cx-functions])
    pub internal_path: String,
    
    pub imports: Vec<String>,
    pub global_stmts: Vec<CXGlobalStmt>,
    
    pub type_map: CXTypeMap,
    pub function_map: CXFunctionMap,
    pub destructor_map: CXDestructorMap,
    
    pub global_variables: HashMap<String, CXGlobalVariable>
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXParameter {
    pub name: Option<CXIdent>,
    pub _type: CXType,
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXFunctionPrototype {
    pub name: CXFunctionIdentifier,
    pub return_type: CXType,
    pub params: Vec<CXParameter>,
    pub var_args: bool
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXGlobalStmt {
    TypeDecl {
        name: Option<String>,
        type_: CXType
    },

    GlobalVariable {
        name: CXIdent,
        type_: CXType,
        initializer: Option<CXExpr>
    },

    FunctionPrototype {
        prototype: CXFunctionPrototype,
    },
    
    FunctionDefinition {
        prototype: CXFunctionPrototype,
        body: Box<CXExpr>,
    },
    
    DestructorDefinition {
        type_name: String,
        body: Box<CXExpr>,
    },
    
    TemplatedFunction {
        prototype: CXFunctionPrototype,
        body: Box<CXExpr>
    },
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXUnOp {
    Dereference, AddressOf,
    Negative,
    BNot, LNot,
    ArrayIndex,
    InitializerIndex,
    
    ExplicitCast(CXNaiveType),

    PreIncrement(i8),
    PostIncrement(i8),
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXBinOp {
    Add, Subtract, Multiply, Divide, Modulus,
    Less, Greater, LessEqual, GreaterEqual,
    Equal, NotEqual,

    LAnd, LOr, BitAnd, BitOr, BitXor,
    LShift, RShift,

    Comma,

    Assign(Option<Box<CXBinOp>>),

    Access, MethodCall, ArrayIndex
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXInitIndex {
    pub name: Option<String>,
    pub value: CXExpr,
    pub index: usize,
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXGlobalVariable {
    GlobalConstant {
        // if the constant cannot be addressed (e.g. intrinsic generated constants like enum values),
        // these values do not need to be generated in the final binary
        //
        // anonymous - true if the constant fits this criteria, false if it can be addressed
        anonymous: bool,
        constant: CXGlobalConstant
    },
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXGlobalConstant {
    Int(i32)
}

#[derive(Debug, Readable, Writable)]
pub struct CXExpr {
    pub uuid: u64,
    pub kind: CXExprKind,

    pub start_index: usize,
    pub end_index: usize,
}

impl Clone for CXExpr {
    fn clone(&self) -> Self {
        CXExpr {
            uuid: Uuid::new_v4().as_u128() as u64,
            kind: self.kind.clone(),

            start_index: self.start_index,
            end_index: self.end_index,
        }
    }
}

impl Default for CXExpr {
    fn default() -> Self {
        CXExpr {
            uuid: 0,
            kind: CXExprKind::Taken,

            start_index: 0,
            end_index: 0,
        }
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXExprKind {
    Taken,
    Unit,
    
    TemplatedFnIdent {
        fn_name: CXIdent,
        template_input: CXNaiveTemplateInput
    },
    Identifier(CXIdent),

    IntLiteral {
        val: i64,
        bytes: u8,
    },
    FloatLiteral {
        val: f64,
        bytes: u8,
    },
    StringLiteral {
        val: String,
    },

    If {
        condition: Box<CXExpr>,
        then_branch: Box<CXExpr>,
        else_branch: Option<Box<CXExpr>>
    },
    While {
        condition: Box<CXExpr>,
        body: Box<CXExpr>,
        pre_eval: bool
    },
    For {
        init: Box<CXExpr>,
        condition: Box<CXExpr>,
        increment: Box<CXExpr>,
        body: Box<CXExpr>
    },
    Switch {
        condition: Box<CXExpr>,
        block: Vec<CXExpr>,
        cases: Vec<(u64, usize)>,
        default_case: Option<usize>
    },

    SizeOf {
        expr: Box<CXExpr>
    },
    VarDeclaration {
        type_: CXType,
        name: CXIdent
    },
    BinOp {
        lhs: Box<CXExpr>,
        rhs: Box<CXExpr>,
        op: CXBinOp
    },
    UnOp {
        operand: Box<CXExpr>,
        operator: CXUnOp
    },

    Block {
        exprs: Vec<CXExpr>,
        value: Option<Box<CXExpr>>
    },

    Break,
    Continue,

    Return {
        value: Option<Box<CXExpr>>
    },
    
    Defer {
        expr: Box<CXExpr>
    },

    ImplicitCast {
        expr: Box<CXExpr>,
        from_type: CXType,
        to_type: CXType,
        cast_type: CXCastType
    },

    ImplicitLoad {
        expr: Box<CXExpr>,
        loaded_type: CXType
    },
    
    New {
        _type: CXType,
        array_length: Option<Box<CXExpr>>
    },
    Move {
        expr: Box<CXExpr>
    },

    GetFunctionAddr {
        func_name: Box<CXExpr>,
        func_sig: CXType
    },

    InitializerList {
        indices: Vec<CXInitIndex>,
    },
}

impl CXExprKind {
    pub fn into_expr(self, start_index: usize, end_index: usize) -> CXExpr {
        let (start_index, end_index) = if start_index > end_index {
            (0, 0)
        } else {
            (start_index, end_index)
        };
        
        CXExpr {
            uuid: Uuid::new_v4().as_u128() as u64,
            kind: self,

            start_index,
            end_index,
        }
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXCastType {
    IntegralCast,
    FloatCast,
    IntToFloat,
    FloatToInt,
    BitCast,
    IntegralTrunc,
    IntToPtrDiff,
    PtrToInt,
    IntToPtr,
    FunctionToPointerDecay,
    
    // The difference between a memory reference and a bare type is that a memory reference
    // is stored in memory. A structured type is itself a memory reference despite this
    // dichotomy, so when attempting to convert from a mem(struct) to struct, this is
    // used to create an explicit no-op to appease the typechecker.
    FauxLoad,
    
    AddPointerTag,
    RemovePointerTag
}