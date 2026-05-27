use std::collections::HashMap;
use std::sync::Arc;

use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, module_path::ModulePath, unsafe_float::FloatWrapper};
use speedy::{Readable, Writable};
use uuid::Uuid;

use crate::{
    data::{CXFunctionPrototype, CXTemplateInput, CXTemplatePrototype, CXType, ModuleResource},
    type_map::{CXFnMap, CXTypeMap},
};

#[derive(Debug, Default)]
pub struct CXAST {
    // Path to .cx file
    pub file_path: String,

    // Prefix for internal paths (i.e. {internal_path}.[o|cx-types|cx-functions])
    pub internal_path: String,

    pub imports: Vec<ModulePath>,
    pub function_stmts: Vec<CXFunctionStmt>,

    pub global_variables: HashMap<String, ModuleResource<CXGlobalVariable>>,
    pub type_data: CXTypeMap,
    pub function_data: CXFnMap,
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXFunctionStmt {
    TypeDecl {
        name: Option<String>,
        _type: CXType,
    },

    FunctionDefinition {
        prototype: CXFunctionPrototype,
        body: Box<CXExpression>,
    },

    TemplatedFunction {
        prototype: CXFunctionPrototype,
        template_prototype: CXTemplatePrototype,
        body: Box<CXExpression>,
    },
}

#[derive(Debug, Default, Hash, Clone, PartialOrd, PartialEq, Eq, Copy, Readable, Writable)]
pub enum VisibilityMode {
    #[default]
    Private,
    Package,
    Public,
}

impl From<cx_preparse_data::VisibilityMode> for VisibilityMode {
    fn from(value: cx_preparse_data::VisibilityMode) -> Self {
        match value {
            cx_preparse_data::VisibilityMode::Private => Self::Private,
            cx_preparse_data::VisibilityMode::Package => Self::Package,
            cx_preparse_data::VisibilityMode::Public => Self::Public,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub enum CXUnOp {
    Dereference,
    AddressOf,
    Negative,
    BNot,
    LNot,

    ExplicitCast(CXType),

    PreIncrement(i8),
    PostIncrement(i8),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub enum CXBinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,

    LAnd,
    LOr,
    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,

    Comma,

    Assign(Option<Box<CXBinOp>>),

    Access,
    ScopeRes,
    MethodCall,
    ArrayIndex,

    Is,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub struct CXInitIndex {
    pub name: Option<String>,
    pub value: CXExpression,
    pub index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub struct CXUnpackBinding {
    pub field: CXIdent,
    pub binding: CXIdent,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub struct CXEnumVariant {
    pub name: CXIdent,
    pub value: Option<CXExpression>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub enum CXGlobalVariable {
    EnumDefinition {
        variants: Vec<CXEnumVariant>,
    },

    Standard {
        _type: CXType,
        is_mutable: bool,
        initializer: Option<CXExpression>,
    },
}

#[derive(Debug, PartialEq, Eq, Hash, Readable, Writable)]
pub struct CXExpression {
    pub uuid: u64,
    pub kind: CXExprKind,
    pub range: TokenRange,
}

impl Clone for CXExpression {
    fn clone(&self) -> Self {
        CXExpression {
            uuid: Uuid::new_v4().as_u128() as u64,
            kind: self.kind.clone(),
            range: self.range.clone(),
        }
    }
}

impl Default for CXExpression {
    fn default() -> Self {
        CXExpression {
            uuid: 0,
            kind: CXExprKind::Taken,
            range: TokenRange::default(),
        }
    }
}

impl CXExpression {
    pub fn token_range(&self) -> &TokenRange {
        &self.range
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub enum CXExprKind {
    Taken,
    Unit,

    TemplatedIdentifier {
        name: CXIdent,
        template_input: CXTemplateInput,
    },
    Identifier(CXIdent),

    IntLiteral {
        val: i64,
        bytes: u8,
    },
    FloatLiteral {
        val: FloatWrapper,
        bytes: u8,
    },
    StringLiteral {
        val: String,
    },

    If {
        condition: Box<CXExpression>,
        then_branch: Box<CXExpression>,
        else_branch: Option<Box<CXExpression>>,
    },
    Ternary {
        condition: Box<CXExpression>,
        then_branch: Box<CXExpression>,
        else_branch: Box<CXExpression>,
    },
    While {
        condition: Box<CXExpression>,
        body: Box<CXExpression>,
        pre_eval: bool,
    },
    For {
        init: Box<CXExpression>,
        condition: Box<CXExpression>,
        increment: Box<CXExpression>,
        body: Box<CXExpression>,
    },

    Match {
        condition: Box<CXExpression>,
        arms: Vec<(CXExpression, CXExpression)>, // (value, block)
        default: Option<Box<CXExpression>>,
    },

    Switch {
        condition: Box<CXExpression>,
        block: Vec<CXExpression>,
        cases: Vec<(u64, usize)>, // (block index, value)
        default_case: Option<usize>,
    },

    SizeOfExpr {
        expr: Box<CXExpression>,
    },
    SizeOfType {
        _type: CXType,
    },

    VarDeclaration {
        _type: CXType,
        name: CXIdent,
        initial_value: Option<Box<CXExpression>>,
    },
    TypeConstructor {
        union_name: CXIdent,
        variant_name: CXIdent,
        inner: Box<CXExpression>,
    },
    BinOp {
        lhs: Box<CXExpression>,
        rhs: Box<CXExpression>,
        op: CXBinOp,
    },
    UnOp {
        operand: Box<CXExpression>,
        operator: CXUnOp,
    },

    Block {
        exprs: Vec<CXExpression>,
    },

    Break,
    Continue,

    Return {
        value: Option<Box<CXExpression>>,
    },

    Unsafe {
        expr: Box<CXExpression>,
    },
    Leak {
        expr: Box<CXExpression>,
    },
    Adopt {
        expr: Box<CXExpression>,
    },
    Unpack {
        expr: Box<CXExpression>,
        bindings: Vec<CXUnpackBinding>,
    },

    Move {
        expr: Box<CXExpression>,
    },

    InitializerList {
        indices: Vec<CXInitIndex>,
    },
}

impl CXExprKind {
    pub fn into_expr(self, start_index: usize, end_index: usize) -> CXExpression {
        let (start_index, end_index) = if start_index > end_index {
            (0, 0)
        } else {
            (start_index, end_index)
        };

        CXExpression {
            uuid: Uuid::new_v4().as_u128() as u64,
            kind: self,
            range: TokenRange::new(start_index, end_index, Arc::from("")),
        }
    }

    pub fn into_expr_with_origin(
        self,
        start_index: usize,
        end_index: usize,
        file_origin: Arc<str>,
    ) -> CXExpression {
        let (start_index, end_index) = if start_index > end_index {
            (0, 0)
        } else {
            (start_index, end_index)
        };

        CXExpression {
            uuid: Uuid::new_v4().as_u128() as u64,
            kind: self,
            range: TokenRange::new(start_index, end_index, file_origin),
        }
    }

    pub fn block_terminating(&self) -> bool {
        matches!(
            self,
            CXExprKind::Return { .. }
                | CXExprKind::Break
                | CXExprKind::Continue
                | CXExprKind::Taken
        )
    }
}
