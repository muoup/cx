use cx_log::error::{CXRawResult, message::CXStdErrMessage};
use cx_thir::thir::r#type::THIRType;
use cx_tokens::TokenRange;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ControlTarget {
    Local,
    Staged,
    Invalid,
}

#[derive(Clone, Default)]
pub struct ScopeEffects {
    pub break_range: Option<TokenRange>,
    pub continue_range: Option<TokenRange>,
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
        });
    }

    pub fn at_function_root(&self) -> bool {
        self.scopes.len() == 1
    }

    pub fn pop_scope(&mut self) -> CXRawResult<ScopeEffects> {
        let Some(scope) = self.scopes.pop() else {
            return CXStdErrMessage::result(
                "TYPE ERROR",
                "Attempted to pop a scope from an empty scope stack".to_string(),
            );
        };

        if !scope.staged_boundary
            && let Some(parent) = self.scopes.last_mut()
        {
            if !scope.handles_break && parent.effects.break_range.is_none() {
                parent.effects.break_range = scope.effects.break_range.clone();
            }
            if !scope.handles_continue && parent.effects.continue_range.is_none() {
                parent.effects.continue_range = scope.effects.continue_range.clone();
            }
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

    pub fn record_break(&mut self, range: TokenRange) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.effects.break_range.get_or_insert(range);
        }
    }

    pub fn record_continue(&mut self, range: TokenRange) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.effects.continue_range.get_or_insert(range);
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
