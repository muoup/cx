use std::rc::Rc;

use cx_mir::mir::types::{MIRFunctionPrototype, MIRType};

pub type FRc<T> = Rc<T>;

#[derive(Clone, Debug)]
pub struct FMIRFunction {
    pub prototype: MIRFunctionPrototype,
    pub body: FMIRNode,
}

#[derive(Clone, Debug)]
pub enum FMIRType {
    CMonad {
        inner: Box<FMIRType>,
        context: FRc<FMIRNode>,
    },

    Mapping {
        from_type: Box<FMIRType>,
        to_type: Box<FMIRType>,
        body: FRc<FMIRNode>,
    },

    Standard(MIRType),
}

#[derive(Clone, Debug)]
pub struct FMIRNode {
    pub _type: FMIRType,
    pub body: FMIRNodeBody,
}

#[derive(Clone, Debug)]
pub enum FMIRNodeBody {
    Application {
        function: FRc<FMIRNode>,
        argument: FRc<FMIRNode>,
    },

    // ----- Monadic Abstractions -----
    /// _read :: Ptr a -> CMonad a
    Read,

    // _write :: a -> Ptr a -> CMonad ()
    Write,

    /// _alloca :: a -> CMonad (Ptr a)
    Alloca,

    /// _pure :: a -> CMonad a
    Pure,

    /// >>= :: CMonad a -> (a -> CMonad b) -> CMonad b
    Bind {
        monad: FRc<FMIRNode>,
        function: FRc<FMIRNode>,
    },

    /// >> :: CMonad a -> CMonad b -> CMonad b
    Then {
        first: FRc<FMIRNode>,
        second: FRc<FMIRNode>,
    },

    // ----- Control Flow -----
    /// if ... then ... else ...
    If {
        condition: FRc<FMIRNode>,
        then_branch: FRc<FMIRNode>,
        else_branch: FRc<FMIRNode>,
    },

    /// _cloop :: CMonad bool -> CMonad ()
    CLoop {
        condition: FRc<FMIRNode>,
        body: FRc<FMIRNode>,
    },

    /// _creturn :: a -> CMonad a
    ///
    /// Early returns may seem counter to pure functional semantics, however you can think of them
    /// as a mapping to a CMonad in which all subsequent CMonad actions are skipped. The value
    /// that is generated after is thus dead and optimized away, and when reaching the true end
    /// of a function, a CMonad wraps this current state to ensure that we escape the skip-all
    /// context.
    CReturn {
        value: FRc<FMIRNode>,
    },

    // ----- Literals -----
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    Unit,
}

impl FMIRType {
    pub fn pure_type(inner: FMIRType) -> Self {
        FMIRType::CMonad {
            inner: Box::new(inner),
            context: Rc::new(FMIRNode {
                _type: FMIRType::Standard(MIRType::unit()),
                body: FMIRNodeBody::Pure,
            }),
        }
    }
}

impl FMIRNode {
    pub fn pure(inner: FMIRNode) -> Self {
        FMIRNode {
            _type: FMIRType::pure_type(inner._type.clone()),
            body: FMIRNodeBody::Pure,
        }
    }

    pub fn unit() -> Self {
        FMIRNode {
            _type: FMIRType::Standard(MIRType::unit()),
            body: FMIRNodeBody::Unit,
        }
    }

    pub fn cloop(condition: Rc<FMIRNode>, body: Rc<FMIRNode>) -> Self {
        FMIRNode {
            _type: FMIRType::pure_type(FMIRType::Standard(MIRType::unit())),
            body: FMIRNodeBody::CLoop { condition, body },
        }
    }

    pub fn if_else(
        condition: Rc<FMIRNode>,
        then_branch: Rc<FMIRNode>,
        else_branch: Option<Rc<FMIRNode>>,
    ) -> Self {
        FMIRNode {
            _type: FMIRType::Standard(MIRType::unit()),
            body: FMIRNodeBody::If {
                condition,
                then_branch,
                else_branch: else_branch.unwrap_or(Rc::new(FMIRNode::pure(FMIRNode::unit()))),
            },
        }
    }
}
