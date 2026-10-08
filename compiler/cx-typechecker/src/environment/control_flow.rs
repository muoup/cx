use std::rc::Rc;

use cx_hir::ast::expression::HIRExpression;
use cx_log::{CXRawResult, catalogue::typecheck};
use cx_thir::thir::r#type::THIRType;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ControlTarget {
    Local,
    Staged,
    Invalid,
}

#[derive(Clone, Default)]
pub struct ScopeEffects {
    pub yield_type: Option<THIRType>,
    pub yield_has_value: Option<bool>,
}

#[derive(Clone)]
pub struct YieldState {
    pub target: ControlTarget,
    pub expected_type: Option<THIRType>,
    pub saw_value: bool,
    pub saw_empty: bool,
}

pub struct ControlFlow {
    scopes: Vec<Scope>,
}

struct Scope {
    handles_break: bool,
    handles_continue: bool,
    handles_yield: bool,
    expected_yield_type: Option<THIRType>,
    staged_boundary: bool,
    effects: ScopeEffects,
    block: Option<BlockCursor>,
}

struct BlockCursor {
    statements: Rc<[HIRExpression]>,
    next: usize,
}

impl ControlFlow {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    pub fn push_scope(&mut self, handles_break: bool, handles_continue: bool) {
        self.scopes.push(Scope {
            handles_break,
            handles_continue,
            handles_yield: false,
            expected_yield_type: None,
            staged_boundary: false,
            effects: ScopeEffects::default(),
            block: None,
        });
    }

    pub fn push_yield_scope(&mut self, expected_type: Option<THIRType>) {
        self.scopes.push(Scope {
            handles_break: false,
            handles_continue: false,
            handles_yield: true,
            expected_yield_type: expected_type,
            staged_boundary: false,
            effects: ScopeEffects::default(),
            block: None,
        });
    }

    pub fn push_staged_scope(&mut self) {
        self.scopes.push(Scope {
            handles_break: false,
            handles_continue: false,
            handles_yield: false,
            expected_yield_type: None,
            staged_boundary: true,
            effects: ScopeEffects::default(),
            block: None,
        });
    }

    /// Marks the innermost scope as the scope of a block with these statements.
    pub fn enter_block(&mut self, statements: &Rc<[HIRExpression]>) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.block = Some(BlockCursor {
                statements: statements.clone(),
                next: 0,
            });
        }
    }

    /// The innermost scope, provided it is a block's. Staged boundaries are looked through, as
    /// they are what a `then` sits behind.
    fn block_scope(&self) -> Option<usize> {
        let innermost = self.scopes.iter().rposition(|scope| !scope.staged_boundary)?;
        self.scopes[innermost].block.is_some().then_some(innermost)
    }

    pub fn in_block(&self) -> bool {
        self.block_scope().is_some()
    }

    /// Advances the current block, returning its statements and the index of the one to check.
    pub fn next_block_statement(&mut self) -> Option<(Rc<[HIRExpression]>, usize)> {
        let scope = self.block_scope()?;
        let cursor = self.scopes[scope].block.as_mut()?;
        let index = cursor.next;
        if index == cursor.statements.len() {
            return None;
        }

        cursor.next += 1;
        Some((cursor.statements.clone(), index))
    }

    pub fn at_function_root(&self) -> bool {
        self.scopes.len() == 1
    }

    pub fn pop_scope(&mut self) -> CXRawResult<ScopeEffects> {
        let Some(scope) = self.scopes.pop() else {
            return Err(typecheck::POP_EMPTY_SCOPE.bind(()));
        };

        if !scope.staged_boundary
            && let Some(parent) = self.scopes.last_mut()
        {
            if !scope.handles_yield && parent.effects.yield_type.is_none() {
                parent.effects.yield_type = scope.effects.yield_type.clone();
                parent.effects.yield_has_value = scope.effects.yield_has_value;
            }
        }

        Ok(scope.effects)
    }

    pub fn break_target(&self) -> ControlTarget {
        self.target(|scope| scope.handles_break)
    }

    pub fn continue_target(&self) -> ControlTarget {
        self.target(|scope| scope.handles_continue)
    }

    fn target(&self, handles: impl Fn(&Scope) -> bool) -> ControlTarget {
        for scope in self.scopes.iter().rev() {
            if scope.staged_boundary {
                return ControlTarget::Staged;
            }
            if handles(scope) {
                return ControlTarget::Local;
            }
        }
        ControlTarget::Invalid
    }

    pub fn yield_state(&self) -> YieldState {
        let mut expected_type = None;
        let mut saw_value = false;
        let mut saw_empty = false;

        for scope in self.scopes.iter().rev() {
            if let Some(yield_type) = &scope.effects.yield_type {
                expected_type.get_or_insert_with(|| yield_type.clone());
                saw_value |= scope.effects.yield_has_value == Some(true);
                saw_empty |= scope.effects.yield_has_value == Some(false);
            }

            if scope.staged_boundary {
                return YieldState {
                    target: ControlTarget::Staged,
                    expected_type,
                    saw_value,
                    saw_empty,
                };
            }
            if scope.handles_yield {
                return YieldState {
                    target: ControlTarget::Local,
                    expected_type: scope.expected_yield_type.clone().or(expected_type),
                    saw_value,
                    saw_empty,
                };
            }
        }

        YieldState {
            target: ControlTarget::Invalid,
            expected_type,
            saw_value,
            saw_empty,
        }
    }

    pub fn record_yield(&mut self, ty: THIRType, has_value: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.effects.yield_type.get_or_insert(ty);
            scope.effects.yield_has_value.get_or_insert(has_value);
        }
    }

    pub fn yield_result_type(&self, effects: &ScopeEffects) -> Option<THIRType> {
        effects.yield_type.clone()
    }
}
