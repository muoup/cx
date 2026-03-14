use std::rc::Rc;

use cx_mir::mir::expression::MIRSourceRange;
use cx_mir::mir::types::{MIRFunctionPrototype, MIRType};
use cx_util::identifier::CXIdent;

use crate::intrinsic::FMIRIntrinsicFunction;

pub type FRc<T> = Rc<T>;

/// Stack variable reference with depth for shadowing support
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum MemoryLocation {
    /// Stack variable with name and depth (for shadowing)
    Stack { name: String, depth: usize },
    /// Function parameter
    Parameter(String),
    /// Global/static variable
    Global(String),
}

/// Useful result type for calculating new state of computations
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MonadicState {
    Pure,
    Operation(CVMOperation),
}

/// Memory CMonad - tracks what memory operations a computation performs
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CVMOperation {
    /// Explicit unsafe block - black box to analysis
    /// Like Rust's `unsafe { ... }`
    Unsafe,
    /// Known memory access pattern
    /// Library-defined types (ref<T>, cref<T>, box<T>) use this
    Access {
        reads: Vec<MemoryLocation>,
        writes: Vec<MemoryLocation>,
    },
}

impl CVMOperation {
    fn dedup_locations(locations: &mut Vec<MemoryLocation>) {
        locations.sort();
        locations.dedup();
    }

    pub fn union(&self, other: &Self) -> Self {
        match (self, other) {
            (CVMOperation::Unsafe, _) | (_, CVMOperation::Unsafe) => CVMOperation::Unsafe,
            (
                CVMOperation::Access {
                    reads: left_reads,
                    writes: left_writes,
                },
                CVMOperation::Access {
                    reads: right_reads,
                    writes: right_writes,
                },
            ) => {
                let mut reads = left_reads.clone();
                reads.extend(right_reads.iter().cloned());
                Self::dedup_locations(&mut reads);

                let mut writes = left_writes.clone();
                writes.extend(right_writes.iter().cloned());
                Self::dedup_locations(&mut writes);

                CVMOperation::Access { reads, writes }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct FMIRFunction {
    pub prototype: MIRFunctionPrototype,
    pub body: FMIRNode,
}

#[derive(Clone, Debug)]
pub enum FMIRType {
    /// Leaf/primitive type - represents base types like i32, bool, ()
    /// These are implicitly pure (CMonad::Pure)
    Pure { mir_type: MIRType },

    /// All values are wrapped in CMonad - Pure = non-CMonadful
    CMonad {
        inner: Box<FMIRType>,
        operation: CVMOperation,
    },

    /// Function types - unchanged
    Mapping {
        from_type: Box<FMIRType>,
        to_type: Box<FMIRType>,
        body: FRc<FMIRNode>,
    },
}

#[derive(Clone, Debug)]
pub struct FMIRNode {
    pub _type: FMIRType,
    pub body: FMIRNodeBody,
    pub source_range: Option<FMIRSourceRange>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FMIRSourceRange {
    pub start_token: usize,
    pub end_token: usize,
}

impl From<&MIRSourceRange> for FMIRSourceRange {
    fn from(value: &MIRSourceRange) -> Self {
        Self {
            start_token: value.start_token,
            end_token: value.end_token,
        }
    }
}

#[derive(Clone, Debug)]
pub enum FMIRNodeBody {
    Application {
        function: FRc<FMIRNode>,
        argument: FRc<FMIRNode>,
    },

    IntrinsicFunction(FMIRIntrinsicFunction),

    // ===== CMonad Intrinsics =====
    /// _unsafe_block :: CMonad a -> CMonad a
    /// Mark a computation as unsafe - black box to analysis
    /// This is how raw C code enters the system
    UnsafeBlock,

    /// _compiler_assert :: CMonad ()
    CompilerAssert {
        condition: FRc<FMIRNode>,
        message: String,
    },

    /// _declare_access :: CMonad a
    /// Intrinsics for library code to declare what it accesses
    /// Library-defined ref<T>::read() would lower to:
    ///   _declare_access [reads: [self]] [writes: []] >> load_operation
    DeclareAccess {
        reads: Vec<MemoryLocation>,
        writes: Vec<MemoryLocation>,
    },

    // ===== Monadic Combinators =====
    /// _pure :: a -> CMonad a
    Pure,

    /// >>= :: CMonad a -> (a -> CMonad b) -> CMonad b
    Bind {
        monad: FRc<FMIRNode>,
        capture: CXIdent,
        function: FRc<FMIRNode>,
    },

    /// >> :: CMonad a -> CMonad b -> CMonad b
    Then {
        first: FRc<FMIRNode>,
        second: FRc<FMIRNode>,
    },

    // ===== Memory Operations =====
    /// _alloca :: CMonad (Ptr a)
    /// Stack allocation - returns unsafe CMonad (conservative)
    Alloca,

    /// _load :: Ptr a -> CMonad a
    /// Load from pointer - returns unsafe CMonad (conservative)
    /// Library ref<T>::read() wraps this with DeclareAccess
    Load {
        pointer: FRc<FMIRNode>,
    },

    /// _store :: Ptr a -> a -> CMonad ()
    /// Store to pointer - returns unsafe CMonad (conservative)
    /// Library ref<T>::write() wraps this with DeclareAccess
    Store {
        pointer: FRc<FMIRNode>,
        value: FRc<FMIRNode>,
    },

    // ===== Control Flow =====
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
    // /
    /// Early returns may seem counter to pure functional semantics, however you can think of them
    /// as a mapping to an CMonad in which all subsequent CMonad actions are skipped. The value
    /// that is generated after is thus dead and optimized away, and when reaching the true end
    /// of a function, an CMonad wraps this current state to ensure that we escape the skip-all
    /// context.
    CReturn {
        value: FRc<FMIRNode>,
    },

    /// Alias for source-language variables/functions (not intrinsic IDs).
    VariableAlias {
        name: String,
    },

    // ===== Literals =====
    IntegerLiteral(i64),
    FloatLiteral(f64),
    BooleanLiteral(bool),
    Unit,
}

impl FMIRType {
    /// Create a leaf type (primitive like i32, bool, etc.)
    pub fn pure(mir_type: MIRType) -> Self {
        FMIRType::Pure { mir_type }
    }

    /// Create an unsafe CMonad (black box to analysis)
    pub fn unsafe_effect(inner: FMIRType) -> Self {
        FMIRType::CMonad {
            inner: Box::new(inner),
            operation: CVMOperation::Unsafe,
        }
    }

    /// Create an access CMonad with known reads/writes
    pub fn access(
        inner: FMIRType,
        reads: Vec<MemoryLocation>,
        writes: Vec<MemoryLocation>,
    ) -> Self {
        FMIRType::CMonad {
            inner: Box::new(inner),
            operation: CVMOperation::Access { reads, writes },
        }
    }

    /// Get the inner type, stripping the CMonad
    pub fn inner_type(&self) -> &FMIRType {
        match self {
            FMIRType::CMonad { inner, .. } => inner.as_ref(),
            FMIRType::Pure { .. } | FMIRType::Mapping { .. } => self,
        }
    }

    pub fn get_operation(&self) -> Option<&CVMOperation> {
        match self {
            FMIRType::CMonad { operation, .. } => Some(operation),
            FMIRType::Pure { .. } | FMIRType::Mapping { .. } => None,
        }
    }

    pub fn identity(&self) -> MonadicState {
        match self.get_operation() {
            Some(operation) => MonadicState::Operation(operation.clone()),
            None => MonadicState::Pure,
        }
    }

    /// Union: combine two CMonads (for Then/Bind sequencing)
    /// The result is the least upper bound - must account for both
    pub fn union(&self, other: &Self) -> MonadicState {
        match (self.get_operation(), other.get_operation()) {
            (Some(left), Some(right)) => MonadicState::Operation(left.union(right)),
            (None, None) => MonadicState::Pure,

            (Some(other), None) | (None, Some(other)) => MonadicState::Operation(other.clone()),
        }
    }
}

impl FMIRNode {
    /// Create a unit value
    pub fn unit() -> Self {
        FMIRNode {
            _type: FMIRType::pure(MIRType::unit()),
            body: FMIRNodeBody::Unit,
            source_range: None,
        }
    }

    /// Create a CLoop node
    pub fn cloop(condition: Rc<FMIRNode>, body: Rc<FMIRNode>) -> Self {
        FMIRNode {
            _type: FMIRType::unsafe_effect(FMIRType::pure(MIRType::unit())),
            body: FMIRNodeBody::CLoop { condition, body },
            source_range: None,
        }
    }

    /// Create an if-else node
    pub fn if_else(
        condition: Rc<FMIRNode>,
        then_branch: Rc<FMIRNode>,
        else_branch: Option<Rc<FMIRNode>>,
    ) -> Self {
        FMIRNode {
            _type: FMIRType::pure(MIRType::unit()),
            body: FMIRNodeBody::If {
                condition,
                then_branch,
                else_branch: else_branch.unwrap_or(Rc::new(FMIRNode::unit())),
            },
            source_range: None,
        }
    }
}

impl MonadicState {
    pub fn apply(self, _type: FMIRType) -> FMIRType {
        match self {
            MonadicState::Pure => _type,
            MonadicState::Operation(operation) => FMIRType::CMonad {
                inner: Box::new(_type),
                operation,
            },
        }
    }

    pub fn union(self, other: &FMIRType) -> MonadicState {
        match (self, other.identity()) {
            (MonadicState::Pure, MonadicState::Pure) => MonadicState::Pure,
            (MonadicState::Operation(e), MonadicState::Pure)
            | (MonadicState::Pure, MonadicState::Operation(e)) => MonadicState::Operation(e),
            (MonadicState::Operation(left), MonadicState::Operation(right)) => {
                MonadicState::Operation(left.union(&right))
            }
        }
    }
}
