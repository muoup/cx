use cx_hir::ast::expression::HIRExpression;
use cx_log::error::CXRawResult;
use cx_tokens::TokenRange;

/// A snapshot of typechecking reachability at each active control-flow scope.
///
/// Ownership state deliberately does not live here. Move validity and
/// `@nodrop` discharge are checked after lowering, where the complete MIR CFG
/// and lexical scope markers are available.
#[derive(Clone)]
pub struct ControlFlowSnapshot {
    pub reachable: Vec<bool>,
}

#[derive(Clone)]
pub struct ControlFlowArrow {
    pub label: String,
    pub snapshot: ControlFlowSnapshot,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ScopeId(usize);

impl ScopeId {
    pub fn new(index: usize) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScopeArrowSink {
    Merge,
    LoopContinue,
    LoopExit,
    LoopPendingIncrement,
}

#[derive(Clone)]
pub struct ScopeExitTarget {
    pub target_scope: ScopeId,
    pub sink: ScopeArrowSink,
    pub label: String,
}

#[derive(Clone)]
pub struct MergeScopeState {
    pub incoming_arrows: Vec<ControlFlowArrow>,
    pub include_current_snapshot: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoopScopeKind {
    While,
    For,
}

#[derive(Clone)]
pub struct LoopScopeState {
    pub loop_kind: LoopScopeKind,
    pub entry_snapshot: ControlFlowSnapshot,
    pub continue_arrows: Vec<ControlFlowArrow>,
    pub exit_arrows: Vec<ControlFlowArrow>,
    pub pending_increment_arrows: Vec<ControlFlowArrow>,
}

#[derive(Clone)]
pub enum ScopeFlowKind {
    Plain,
    Merge(MergeScopeState),
    Loop(LoopScopeState),
}

#[derive(Clone)]
pub struct Scope {
    pub has_break_merge: bool,
    pub has_continue_merge: bool,
    pub reachable: bool,
    pub anchor_range: Option<TokenRange>,
    pub natural_exit_target: Option<ScopeExitTarget>,
    pub flow_kind: ScopeFlowKind,
}

pub struct ControlFlow {
    scope_stack: Vec<Scope>,
}

impl ControlFlow {
    pub fn new() -> Self {
        Self {
            scope_stack: Vec::new(),
        }
    }

    pub fn push_scope(&mut self, has_break_merge: bool, has_continue_merge: bool) {
        self.scope_stack.push(Scope {
            has_break_merge,
            has_continue_merge,
            reachable: true,
            anchor_range: None,
            natural_exit_target: None,
            flow_kind: ScopeFlowKind::Plain,
        });
    }

    pub fn pop_scope(&mut self) -> CXRawResult<()> {
        let Some(scope) = self.scope_stack.last().cloned() else {
            panic!("Scope stack has uneven push/pop");
        };

        let current_snapshot = scope.reachable.then(|| self.current_snapshot());
        self.scope_stack.pop();

        let outgoing_snapshot = self.resolve_scope_flow(&scope, current_snapshot.as_ref());
        let final_reachable = outgoing_snapshot.is_some();

        if let Some(target) = scope.natural_exit_target
            && final_reachable
        {
            let snapshot = outgoing_snapshot.expect("reachable scope has an outgoing snapshot");
            self.enqueue_scope_arrow(&target, snapshot);
        } else if let Some(parent) = self.scope_stack.last_mut() {
            parent.reachable = final_reachable;
        }

        CXRawResult::Ok(())
    }

    pub fn current_snapshot(&self) -> ControlFlowSnapshot {
        ControlFlowSnapshot {
            reachable: self
                .scope_stack
                .iter()
                .map(|scope| scope.reachable)
                .collect(),
        }
    }

    pub fn restore_snapshot(&mut self, snapshot: &ControlFlowSnapshot) {
        for (scope, reachable) in self.scope_stack.iter_mut().zip(&snapshot.reachable) {
            scope.reachable = *reachable;
        }
    }

    pub fn current_scope_index(&self) -> ScopeId {
        ScopeId(self.scope_stack.len() - 1)
    }

    pub fn set_scope_anchor(&mut self, expr: &HIRExpression) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.anchor_range = Some(expr.token_range().clone());
        }
    }

    pub fn configure_merge_scope(
        &mut self,
        expr: &HIRExpression,
        include_current_snapshot: Option<&str>,
    ) {
        let range = expr.token_range().clone();
        let scope = self
            .scope_stack
            .last_mut()
            .expect("Missing scope to configure");
        scope.anchor_range = Some(range);
        scope.flow_kind = ScopeFlowKind::Merge(MergeScopeState {
            incoming_arrows: Vec::new(),
            include_current_snapshot: include_current_snapshot.map(str::to_string),
        });
    }

    pub fn configure_loop_scope(&mut self, expr: &HIRExpression, loop_kind: LoopScopeKind) {
        let entry_snapshot = self.current_snapshot();
        let range = expr.token_range().clone();
        let scope = self
            .scope_stack
            .last_mut()
            .expect("Missing scope to configure");
        scope.anchor_range = Some(range);
        scope.flow_kind = ScopeFlowKind::Loop(LoopScopeState {
            loop_kind,
            entry_snapshot,
            continue_arrows: Vec::new(),
            exit_arrows: Vec::new(),
            pending_increment_arrows: Vec::new(),
        });
    }

    pub fn set_scope_fallthrough_target(&mut self, target: ScopeExitTarget) {
        let scope = self
            .scope_stack
            .last_mut()
            .expect("Missing scope to configure");
        scope.natural_exit_target = Some(target);
    }

    pub fn enqueue_scope_arrow(&mut self, target: &ScopeExitTarget, snapshot: ControlFlowSnapshot) {
        let arrow = ControlFlowArrow {
            label: target.label.clone(),
            snapshot,
        };
        let scope = self
            .scope_stack
            .get_mut(target.target_scope.index())
            .expect("Invalid target scope for control-flow arrow");

        match (&mut scope.flow_kind, target.sink) {
            (ScopeFlowKind::Merge(state), ScopeArrowSink::Merge) => {
                state.incoming_arrows.push(arrow)
            }
            (ScopeFlowKind::Loop(state), ScopeArrowSink::LoopContinue) => {
                state.continue_arrows.push(arrow)
            }
            (ScopeFlowKind::Loop(state), ScopeArrowSink::LoopExit) => state.exit_arrows.push(arrow),
            (ScopeFlowKind::Loop(state), ScopeArrowSink::LoopPendingIncrement) => {
                state.pending_increment_arrows.push(arrow)
            }
            _ => panic!("Invalid control-flow arrow sink for scope"),
        }
    }

    pub fn take_pending_increment_arrows(&mut self, scope: ScopeId) -> Vec<ControlFlowArrow> {
        let scope = self
            .scope_stack
            .get_mut(scope.index())
            .expect("Invalid loop scope for pending increment arrows");

        match &mut scope.flow_kind {
            ScopeFlowKind::Loop(state) => std::mem::take(&mut state.pending_increment_arrows),
            _ => panic!("Pending increment arrows requested from non-loop scope"),
        }
    }

    pub fn loop_entry_snapshot(&self, scope: ScopeId) -> ControlFlowSnapshot {
        let scope = self
            .scope_stack
            .get(scope.index())
            .expect("Invalid loop scope for entry snapshot");

        match &scope.flow_kind {
            ScopeFlowKind::Loop(state) => state.entry_snapshot.clone(),
            _ => panic!("Loop entry snapshot requested from non-loop scope"),
        }
    }

    pub fn nearest_break_scope(&self) -> Option<ScopeId> {
        self.scope_stack
            .iter()
            .rposition(|scope| scope.has_break_merge)
            .map(ScopeId)
    }

    pub fn nearest_continue_scope(&self) -> Option<ScopeId> {
        self.scope_stack
            .iter()
            .rposition(|scope| scope.has_continue_merge)
            .map(ScopeId)
    }

    pub fn break_arrow_sink(&self, scope: ScopeId) -> ScopeArrowSink {
        match self
            .scope_stack
            .get(scope.index())
            .map(|scope| &scope.flow_kind)
        {
            Some(ScopeFlowKind::Loop(_)) => ScopeArrowSink::LoopExit,
            _ => ScopeArrowSink::Merge,
        }
    }

    pub fn continue_arrow_sink(&self, scope: ScopeId) -> ScopeArrowSink {
        match self
            .scope_stack
            .get(scope.index())
            .map(|scope| &scope.flow_kind)
        {
            Some(ScopeFlowKind::Loop(state)) if state.loop_kind == LoopScopeKind::For => {
                ScopeArrowSink::LoopPendingIncrement
            }
            Some(ScopeFlowKind::Loop(_)) => ScopeArrowSink::LoopContinue,
            _ => ScopeArrowSink::Merge,
        }
    }

    pub fn mark_jump_unreachable(&mut self, target_scope: ScopeId) {
        for idx in (target_scope.index() + 1..self.scope_stack.len()).rev() {
            let scope = &mut self.scope_stack[idx];
            scope.reachable = false;

            if scope.natural_exit_target.is_some() {
                break;
            }
        }
    }

    pub fn mark_current_scope_unreachable(&mut self) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.reachable = false;
        }
    }

    pub fn set_scope_reachable(&mut self, scope: ScopeId, reachable: bool) {
        if let Some(scope) = self.scope_stack.get_mut(scope.index()) {
            scope.reachable = reachable;
        }
    }

    pub fn is_scope_reachable(&self, scope: ScopeId) -> bool {
        self.scope_stack
            .get(scope.index())
            .map(|scope| scope.reachable)
            .unwrap_or(false)
    }

    pub fn is_current_scope_reachable(&self) -> bool {
        self.scope_stack
            .last()
            .map(|scope| scope.reachable)
            .unwrap_or(true)
    }

    fn resolve_scope_flow(
        &self,
        scope: &Scope,
        current_snapshot: Option<&ControlFlowSnapshot>,
    ) -> Option<ControlFlowSnapshot> {
        match &scope.flow_kind {
            ScopeFlowKind::Plain => current_snapshot.cloned(),
            ScopeFlowKind::Merge(state) => {
                if state.include_current_snapshot.is_some() {
                    current_snapshot.cloned().or_else(|| {
                        state
                            .incoming_arrows
                            .first()
                            .map(|arrow| arrow.snapshot.clone())
                    })
                } else {
                    state
                        .incoming_arrows
                        .first()
                        .map(|arrow| arrow.snapshot.clone())
                        .or_else(|| current_snapshot.cloned())
                }
            }
            ScopeFlowKind::Loop(state) => state
                .exit_arrows
                .first()
                .map(|arrow| arrow.snapshot.clone()),
        }
    }
}
