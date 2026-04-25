use std::path::Path;

use cx_ast::ast::CXExpression;
use cx_mir::mir::expression::MIRExpression;
use cx_tokens::TokenRange;
use cx_tokens::token::Token;
use cx_util::CXResult;
use cx_util::scoped_map::ScopedMap;

use crate::log::TypeError;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BindingMoveState {
    Available,
    Moved,
    ConditionallyMoved,
}

#[derive(Clone, Debug)]
pub struct TrackedBindingState {
    pub state: BindingMoveState,
    pub nodrop: bool,
}

#[derive(Clone)]
pub struct ControlFlowSnapshot {
    pub symbol_table: ScopedMap<MIRExpression>,
    pub tracked_bindings: ScopedMap<TrackedBindingState>,
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
    pub join_name: String,
    pub join_range: TokenRange,
    pub entry_snapshot: ControlFlowSnapshot,
    pub incoming_arrows: Vec<ControlFlowArrow>,
    pub include_current_snapshot: Option<String>,
    pub require_nodrop_discharge: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoopScopeKind {
    While,
    For,
}

#[derive(Clone)]
pub struct LoopScopeState {
    pub loop_kind: LoopScopeKind,
    pub join_range: TokenRange,
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
    symbol_table: ScopedMap<MIRExpression>,
    tracked_bindings: ScopedMap<TrackedBindingState>,
    scope_stack: Vec<Scope>,
}

impl ControlFlow {
    pub fn new() -> Self {
        Self {
            symbol_table: ScopedMap::new(),
            tracked_bindings: ScopedMap::new(),
            scope_stack: Vec::new(),
        }
    }

    pub fn push_scope(&mut self, has_break_merge: bool, has_continue_merge: bool) {
        self.symbol_table.push_scope();
        self.tracked_bindings.push_scope();
        self.scope_stack.push(Scope {
            has_break_merge,
            has_continue_merge,
            reachable: true,
            anchor_range: None,
            natural_exit_target: None,
            flow_kind: ScopeFlowKind::Plain,
        });
    }

    pub fn pop_scope(&mut self, compilation_unit: &Path, tokens: &[Token]) -> CXResult<()> {
        let Some(scope) = self.scope_stack.last().cloned() else {
            panic!("Scope stack has uneven push/pop");
        };

        let current_scope_nodrop = self.current_scope_nodrop_bindings();
        let current_snapshot = scope.reachable.then(|| self.current_snapshot());
        self.symbol_table.pop_scope();
        self.tracked_bindings.pop_scope();
        self.scope_stack.pop().unwrap();

        let outgoing_snapshot =
            self.resolve_scope_flow(compilation_unit, tokens, &scope, current_snapshot.as_ref())?;
        let final_reachable = outgoing_snapshot.is_some();

        if final_reachable
            && !current_scope_nodrop
                .iter()
                .all(|(_, binding)| binding.state == BindingMoveState::Moved)
        {
            let live = current_scope_nodrop
                .into_iter()
                .filter(|(_, binding)| binding.state != BindingMoveState::Moved)
                .map(|(name, _)| name)
                .collect::<Vec<_>>();

            if let Some(range) = scope.anchor_range.as_ref() {
                return Self::type_error_at_range(
                    compilation_unit,
                    tokens,
                    range,
                    format!(
                        "nodrop local(s) reach scope end without move or @leak: {}",
                        live.join(", ")
                    ),
                    Vec::new(),
                );
            }
        }

        if let Some(target) = scope.natural_exit_target
            && final_reachable
        {
            let snapshot = outgoing_snapshot.unwrap_or_else(|| self.current_snapshot());
            self.enqueue_scope_arrow(&target, snapshot);
        } else if let Some(parent) = self.scope_stack.last_mut() {
            parent.reachable = final_reachable;
        }

        Ok(())
    }

    pub fn insert_symbol(&mut self, name: String, value: MIRExpression) {
        self.symbol_table.insert(name, value);
    }

    pub fn symbol_value(&self, name: &str) -> Option<&MIRExpression> {
        self.symbol_table.get(name)
    }

    pub fn current_snapshot(&self) -> ControlFlowSnapshot {
        ControlFlowSnapshot {
            symbol_table: self.symbol_table.clone(),
            tracked_bindings: self.tracked_bindings.clone(),
        }
    }

    pub fn restore_snapshot(&mut self, snapshot: &ControlFlowSnapshot) {
        self.symbol_table = snapshot.symbol_table.clone();
        self.tracked_bindings = snapshot.tracked_bindings.clone();
    }

    pub fn current_scope_index(&self) -> ScopeId {
        ScopeId(self.scope_stack.len() - 1)
    }

    pub fn set_scope_anchor(&mut self, expr: &CXExpression) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.anchor_range = Some(expr.token_range().clone());
        }
    }

    pub fn configure_merge_scope(
        &mut self,
        expr: &CXExpression,
        join_name: impl Into<String>,
        include_current_snapshot: Option<&str>,
        require_nodrop_discharge: bool,
    ) {
        let entry_snapshot = self.current_snapshot();
        let range = expr.token_range().clone();

        let scope = self
            .scope_stack
            .last_mut()
            .expect("Missing scope to configure");
        scope.anchor_range = Some(range.clone());
        scope.flow_kind = ScopeFlowKind::Merge(MergeScopeState {
            join_name: join_name.into(),
            join_range: range,
            entry_snapshot,
            incoming_arrows: Vec::new(),
            include_current_snapshot: include_current_snapshot.map(str::to_string),
            require_nodrop_discharge,
        });
    }

    pub fn configure_loop_scope(&mut self, expr: &CXExpression, loop_kind: LoopScopeKind) {
        let entry_snapshot = self.current_snapshot();
        let range = expr.token_range().clone();

        let scope = self
            .scope_stack
            .last_mut()
            .expect("Missing scope to configure");
        scope.anchor_range = Some(range.clone());
        scope.flow_kind = ScopeFlowKind::Loop(LoopScopeState {
            loop_kind,
            join_range: range,
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

    pub fn track_binding(&mut self, name: String, nodrop: bool) {
        self.tracked_bindings.insert(
            name,
            TrackedBindingState {
                state: BindingMoveState::Available,
                nodrop,
            },
        );
    }

    pub fn tracked_binding(&self, name: &str) -> Option<&TrackedBindingState> {
        self.tracked_bindings.get(name)
    }

    pub fn set_tracked_binding_state(&mut self, name: &str, state: BindingMoveState) {
        let Some(binding) = self.tracked_bindings.get(name).cloned() else {
            return;
        };

        self.tracked_bindings
            .insert(name.to_string(), TrackedBindingState { state, ..binding });
    }

    pub fn tracked_bindings_snapshot(
        &self,
    ) -> std::collections::HashMap<String, TrackedBindingState> {
        self.tracked_bindings
            .iter()
            .map(|(name, binding)| (name.clone(), binding.clone()))
            .collect()
    }

    pub fn nodrop_bindings_in_nonfinal_state(&self) -> Vec<String> {
        self.tracked_bindings
            .iter()
            .filter(|(_, binding)| binding.nodrop && binding.state != BindingMoveState::Moved)
            .map(|(name, _)| name.clone())
            .collect()
    }

    fn current_scope_nodrop_bindings(&self) -> Vec<(String, TrackedBindingState)> {
        if self.scope_stack.is_empty() {
            return Vec::new();
        }

        self.tracked_bindings
            .get_all_at_level(self.tracked_bindings.scope_depth())
            .filter(|(_, binding)| binding.nodrop)
            .map(|(name, binding)| (name.clone(), binding.clone()))
            .collect()
    }

    fn resolve_scope_flow(
        &mut self,
        compilation_unit: &Path,
        tokens: &[Token],
        scope: &Scope,
        current_snapshot: Option<&ControlFlowSnapshot>,
    ) -> CXResult<Option<ControlFlowSnapshot>> {
        match &scope.flow_kind {
            ScopeFlowKind::Plain => Ok(current_snapshot.cloned()),
            ScopeFlowKind::Merge(state) => {
                let mut arrows = state.incoming_arrows.clone();
                if let Some(label) = &state.include_current_snapshot
                    && let Some(snapshot) = current_snapshot
                {
                    arrows.push(ControlFlowArrow {
                        label: label.clone(),
                        snapshot: snapshot.clone(),
                    });
                }

                let Some(merged_bindings) = Self::merge_binding_states(
                    compilation_unit,
                    tokens,
                    &state.entry_snapshot,
                    &arrows,
                    &state.join_range,
                    &state.join_name,
                )?
                else {
                    return Ok(None);
                };

                if state.require_nodrop_discharge {
                    let live = merged_bindings
                        .iter()
                        .filter(|(_, binding)| {
                            binding.nodrop && binding.state != BindingMoveState::Moved
                        })
                        .map(|(name, _)| name.clone())
                        .collect::<Vec<_>>();

                    if !live.is_empty() {
                        return Self::type_error_at_range(
                            compilation_unit,
                            tokens,
                            &state.join_range,
                            format!(
                                "nodrop binding(s) must be moved or @leak'ed before function exit: {}",
                                live.join(", ")
                            ),
                            Vec::new(),
                        );
                    }
                }

                self.apply_merged_bindings(&merged_bindings);
                Ok(Some(self.current_snapshot()))
            }
            ScopeFlowKind::Loop(state) => {
                let mut continue_arrows = vec![ControlFlowArrow {
                    label: "iteration entry".to_string(),
                    snapshot: state.entry_snapshot.clone(),
                }];
                continue_arrows.extend(state.continue_arrows.clone());

                let Some(loop_carried_bindings) = Self::merge_binding_states(
                    compilation_unit,
                    tokens,
                    &state.entry_snapshot,
                    &continue_arrows,
                    &state.join_range,
                    "loop continue join",
                )?
                else {
                    let Some(exit_bindings) = Self::merge_binding_states(
                        compilation_unit,
                        tokens,
                        &state.entry_snapshot,
                        &state.exit_arrows,
                        &state.join_range,
                        "loop exit join",
                    )?
                    else {
                        return Ok(None);
                    };

                    self.apply_merged_bindings(&exit_bindings);
                    return Ok(Some(self.current_snapshot()));
                };

                let mut loop_exit_snapshot = self.current_snapshot();
                for (name, binding) in &loop_carried_bindings {
                    loop_exit_snapshot
                        .tracked_bindings
                        .insert(name.clone(), binding.clone());
                }

                let mut exit_arrows = state.exit_arrows.clone();
                exit_arrows.push(ControlFlowArrow {
                    label: "loop exit".to_string(),
                    snapshot: loop_exit_snapshot,
                });

                let Some(exit_bindings) = Self::merge_binding_states(
                    compilation_unit,
                    tokens,
                    &state.entry_snapshot,
                    &exit_arrows,
                    &state.join_range,
                    "loop exit join",
                )?
                else {
                    return Ok(None);
                };

                self.apply_merged_bindings(&exit_bindings);
                Ok(Some(self.current_snapshot()))
            }
        }
    }

    fn merge_binding_states(
        compilation_unit: &Path,
        tokens: &[Token],
        entry_snapshot: &ControlFlowSnapshot,
        arrows: &[ControlFlowArrow],
        join_range: &TokenRange,
        join_name: &str,
    ) -> CXResult<Option<Vec<(String, TrackedBindingState)>>> {
        if arrows.is_empty() {
            return Ok(None);
        }

        let mut merged_bindings = Vec::new();
        let mut inconsistent = Vec::new();

        for (name, base_binding) in entry_snapshot.tracked_bindings.iter() {
            let states = arrows
                .iter()
                .filter_map(|arrow| {
                    arrow
                        .snapshot
                        .tracked_bindings
                        .get(name)
                        .map(|binding| (arrow.label.as_str(), binding.state))
                })
                .collect::<Vec<_>>();

            if states.is_empty() {
                continue;
            }

            let first_state = states[0].1;
            let merged_state = if states.iter().all(|(_, state)| *state == first_state) {
                first_state
            } else {
                let state_summary = states
                    .iter()
                    .map(|(label, state)| {
                        format!("{label} => {}", Self::describe_move_state(*state))
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                inconsistent.push(format!("{name} [{state_summary}]"));
                BindingMoveState::ConditionallyMoved
            };

            merged_bindings.push((
                name.clone(),
                TrackedBindingState {
                    state: merged_state,
                    ..base_binding.clone()
                },
            ));
        }

        if inconsistent.is_empty() {
            return Ok(Some(merged_bindings));
        }

        let notes = inconsistent
            .iter()
            .map(|note| format!("conflict: {note}"))
            .collect::<Vec<_>>();
        Self::type_error_at_range::<Option<Vec<(String, TrackedBindingState)>>>(
            compilation_unit,
            tokens,
            join_range,
            format!("nocopy binding(s) have inconsistent move state at {join_name}"),
            notes,
        )
    }

    fn apply_merged_bindings(&mut self, merged_bindings: &[(String, TrackedBindingState)]) {
        for (name, binding) in merged_bindings {
            if self.tracked_bindings.get(name).is_some() {
                self.tracked_bindings.insert(name.clone(), binding.clone());
            }
        }
    }

    fn type_error_at_range<T>(
        compilation_unit: &Path,
        tokens: &[Token],
        range: &TokenRange,
        message: String,
        notes: Vec<String>,
    ) -> CXResult<T> {
        let (byte_start, byte_end) =
            crate::log::byte_range_for_tokens(tokens, range.start_token, range.end_token);
        let compilation_unit = (!range.file_origin.is_empty())
            .then(|| range.file_origin.as_ref().into())
            .or_else(|| {
                crate::log::file_origin_for_tokens(tokens, range.start_token, range.end_token)
            })
            .unwrap_or_else(|| compilation_unit.to_owned());
        Err(Box::new(TypeError {
            compilation_unit,
            token_start: range.start_token,
            token_end: range.end_token,
            byte_start,
            byte_end,
            message,
            notes,
        }))
    }

    fn describe_move_state(state: BindingMoveState) -> &'static str {
        match state {
            BindingMoveState::Available => "available",
            BindingMoveState::Moved => "moved",
            BindingMoveState::ConditionallyMoved => "conditionally moved",
        }
    }
}
