use std::rc::Rc;

use cx_mir::mir::types::{MIRFunctionPrototype, MIRType};

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

/// Memory effect - tracks what memory operations a computation performs
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

impl CVMOperation {}

#[derive(Clone, Debug)]
pub struct FMIRFunction {
    pub prototype: MIRFunctionPrototype,
    pub body: FMIRNode,
}

#[derive(Clone, Debug)]
pub enum FMIRType {
    /// Leaf/primitive type - represents base types like i32, bool, ()
    /// These are implicitly pure (Effect::Pure)
    Pure { mir_type: MIRType },

    /// All values are wrapped in Effect - Pure = non-effectful
    CMonad {
        inner: Box<FMIRType>,
        effect: CVMOperation,
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
}

#[derive(Clone, Debug)]
pub enum FMIRNodeBody {
    Application {
        function: FRc<FMIRNode>,
        argument: FRc<FMIRNode>,
    },

    // ===== Effect Intrinsics =====
    /// _unsafe_block :: Effect a -> Effect a
    /// Mark a computation as unsafe - black box to analysis
    /// This is how raw C code enters the system
    UnsafeBlock,

    /// _declare_access :: Effect a
    /// Intrinsics for library code to declare what it accesses
    /// Library-defined ref<T>::read() would lower to:
    ///   _declare_access [reads: [self]] [writes: []] >> load_operation
    DeclareAccess {
        reads: Vec<MemoryLocation>,
        writes: Vec<MemoryLocation>,
    },

    // ===== Monadic Combinators =====
    /// _pure :: a -> Effect a
    Pure,

    /// >>= :: Effect a -> (a -> Effect b) -> Effect b
    Bind {
        monad: FRc<FMIRNode>,
        function: FRc<FMIRNode>,
    },

    /// >> :: Effect a -> Effect b -> Effect b
    Then {
        first: FRc<FMIRNode>,
        second: FRc<FMIRNode>,
    },

    // ===== Memory Operations =====
    /// _alloca :: Effect (Ptr a)
    /// Stack allocation - returns unsafe effect (conservative)
    Alloca,

    /// _load :: Ptr a -> Effect a
    /// Load from pointer - returns unsafe effect (conservative)
    /// Library ref<T>::read() wraps this with DeclareAccess
    Load,

    /// _store :: Ptr a -> a -> Effect ()
    /// Store to pointer - returns unsafe effect (conservative)
    /// Library ref<T>::write() wraps this with DeclareAccess
    Store,

    // ===== Control Flow =====
    /// if ... then ... else ...
    If {
        condition: FRc<FMIRNode>,
        then_branch: FRc<FMIRNode>,
        else_branch: FRc<FMIRNode>,
    },

    /// _cloop :: Effect bool -> Effect ()
    CLoop {
        condition: FRc<FMIRNode>,
        body: FRc<FMIRNode>,
    },

    /// _creturn :: a -> Effect a
    ///
    /// Early returns may seem counter to pure functional semantics, however you can think of them
    /// as a mapping to an Effect in which all subsequent Effect actions are skipped. The value
    /// that is generated after is thus dead and optimized away, and when reaching the true end
    /// of a function, an Effect wraps this current state to ensure that we escape the skip-all
    /// context.
    CReturn {
        value: FRc<FMIRNode>,
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

    /// Create an unsafe effect (black box to analysis)
    pub fn unsafe_effect(inner: FMIRType) -> Self {
        FMIRType::CMonad {
            inner: Box::new(inner),
            effect: CVMOperation::Unsafe,
        }
    }

    /// Create an access effect with known reads/writes
    pub fn access(
        inner: FMIRType,
        reads: Vec<MemoryLocation>,
        writes: Vec<MemoryLocation>,
    ) -> Self {
        FMIRType::CMonad {
            inner: Box::new(inner),
            effect: CVMOperation::Access { reads, writes },
        }
    }

    /// Get the inner type, stripping the effect
    pub fn inner_type(&self) -> &FMIRType {
        match self {
            FMIRType::CMonad { inner, .. } => inner.as_ref(),
            FMIRType::Pure { .. } | FMIRType::Mapping { .. } => self,
        }
    }

    pub fn get_operation(&self) -> Option<&CVMOperation> {
        match self {
            FMIRType::CMonad { effect, .. } => Some(effect),
            FMIRType::Pure { .. } | FMIRType::Mapping { .. } => None,
        }
    }
    
    pub fn identity(&self) -> MonadicState {
        match self.get_operation() {
            Some(effect) => MonadicState::Operation(effect.clone()),
            None => MonadicState::Pure,
        }
    }

    /// Union: combine two effects (for Then/Bind sequencing)
    /// The result is the least upper bound - must account for both
    pub fn union(&self, other: &Self) -> MonadicState {
        match (self.get_operation(), other.get_operation()) {
            (Some(CVMOperation::Unsafe), _) | (_, Some(CVMOperation::Unsafe)) => {
                MonadicState::Operation(CVMOperation::Unsafe)
            }

            (
                Some(CVMOperation::Access {
                    reads: r1,
                    writes: w1,
                }),
                Some(CVMOperation::Access {
                    reads: r2,
                    writes: w2,
                }),
            ) => {
                // Merge the access lists, de-duplicating
                let mut reads = r1.clone();
                reads.extend(r2.iter().cloned());
                reads.sort();
                reads.dedup();

                let mut writes = w1.clone();
                writes.extend(w2.iter().cloned());
                writes.sort();
                writes.dedup();

                MonadicState::Operation(CVMOperation::Access { reads, writes })
            },
            
            (None, None) => MonadicState::Pure,
            
            (Some(other), None) |
            (None, Some(other)) => MonadicState::Operation(other.clone()), 
        }
    }
}

impl FMIRNode {
    /// Create a unit value
    pub fn unit() -> Self {
        FMIRNode {
            _type: FMIRType::pure(MIRType::unit()),
            body: FMIRNodeBody::Unit,
        }
    }

    /// Create a CLoop node
    pub fn cloop(condition: Rc<FMIRNode>, body: Rc<FMIRNode>) -> Self {
        FMIRNode {
            _type: FMIRType::unsafe_effect(FMIRType::pure(MIRType::unit())),
            body: FMIRNodeBody::CLoop { condition, body },
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
        }
    }
}

impl MonadicState {
    pub fn apply(self, _type: FMIRType) -> FMIRType {
        match self {
            MonadicState::Pure => _type,
            MonadicState::Operation(effect) => FMIRType::CMonad { inner: Box::new(_type), effect },
        }
    }
    
    pub fn union(self, other: &FMIRType) -> MonadicState {
        match (self, other.identity()) {
            (MonadicState::Pure, MonadicState::Pure) => MonadicState::Pure,
            (MonadicState::Operation(e), MonadicState::Pure) |
            (MonadicState::Pure, MonadicState::Operation(e)) => MonadicState::Operation(e),
            (MonadicState::Operation(e1), MonadicState::Operation(e2)) => {
                match (e1, e2) {
                    (CVMOperation::Unsafe, _) | (_, CVMOperation::Unsafe) => {
                        MonadicState::Operation(CVMOperation::Unsafe)
                    }
                    (
                        CVMOperation::Access { reads: r1, writes: w1 },
                        CVMOperation::Access { reads: r2, writes: w2 },
                    ) => {
                        let mut reads = r1;
                        reads.extend(r2);
                        reads.sort();
                        reads.dedup();

                        let mut writes = w1;
                        writes.extend(w2);
                        writes.sort();
                        writes.dedup();

                        MonadicState::Operation(CVMOperation::Access { reads, writes })
                    }
                }
            }
        }
    }
}