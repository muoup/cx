use std::collections::HashMap;

use cx_ast::ast::expression::CXExpression;
use cx_log::CXRawResult;
use cx_mir::mir::data::{MIRFunctionPrototype, MIRType};

use crate::environment::control_flow::{
    BindingMoveState, ControlFlow, ControlFlowArrow, ControlFlowSnapshot, LoopScopeKind,
    ScopeArrowSink, ScopeExitTarget, ScopeId, TrackedBindingState,
};

#[derive(Default)]
pub struct FunctionContext {
    current_function: Option<MIRFunctionPrototype>,
    flow: Option<ControlFlow>,
    require_safe: bool,
    require_pure: bool,
    unsafe_depth: usize,
    yield_stack: Vec<YieldContext>,
    defer_scopes: Vec<DeferScope>,
}

#[derive(Default)]
struct DeferScope {
    control_scope_depth: usize,
    expressions: Vec<cx_mir::mir::expression::MIRExpression>,
}

#[derive(Clone)]
pub struct FunctionModeSnapshot {
    safe_mode: bool,
    contract_pure_mode: bool,
    unsafe_depth: usize,
}

#[derive(Clone)]
pub struct YieldContext {
    pub target_scope: ScopeId,
    pub result_type: Option<MIRType>,
    pub yield_count: usize,
}

impl FunctionContext {
    pub fn begin_function(&mut self, prototype: MIRFunctionPrototype) {
        self.require_safe = prototype.signature().contract.safe;
        self.require_pure = false;
        self.unsafe_depth = 0;
        self.flow = Some(ControlFlow::new());
        self.current_function = Some(prototype);
        self.yield_stack.clear();
        self.defer_scopes.clear();
    }

    pub fn end_function(&mut self) {
        self.current_function = None;
        self.flow = None;
        self.require_safe = false;
        self.require_pure = false;
        self.unsafe_depth = 0;
        self.yield_stack.clear();
        debug_assert!(self.defer_scopes.is_empty());
    }

    pub fn current_function(&self) -> &MIRFunctionPrototype {
        self.current_function.as_ref().unwrap()
    }

    pub fn try_current_function(&self) -> Option<&MIRFunctionPrototype> {
        self.current_function.as_ref()
    }

    fn flow(&self) -> &ControlFlow {
        self.flow
            .as_ref()
            .expect("function control-flow state is only available while checking a function body")
    }

    fn flow_mut(&mut self) -> &mut ControlFlow {
        self.flow
            .as_mut()
            .expect("function control-flow state is only available while checking a function body")
    }

    pub fn push_scope(&mut self, has_break_merge: bool, has_continue_merge: bool) {
        self.flow_mut()
            .push_scope(has_break_merge, has_continue_merge);
        self.push_defer_scope();
    }

    pub fn pop_scope(&mut self) -> CXRawResult<()> {
        self.flow_mut().pop_scope()?;
        self.pop_defer_scope();
        CXRawResult::Ok(())
    }

    pub fn register_defer(&mut self, expression: cx_mir::mir::expression::MIRExpression) {
        self.defer_scopes
            .last_mut()
            .expect("defer registered outside of a function scope")
            .expressions
            .push(expression);
    }

    pub fn current_scope_cleanups(&self) -> Vec<cx_mir::mir::expression::MIRExpression> {
        self.defer_scopes
            .last()
            .map(|scope| scope.expressions.iter().rev().cloned().collect())
            .unwrap_or_default()
    }

    pub fn cleanups_exiting_to(
        &self,
        target_scope: ScopeId,
        include_target: bool,
    ) -> Vec<cx_mir::mir::expression::MIRExpression> {
        self.defer_scopes
            .iter()
            .rev()
            .filter(|scope| {
                if include_target {
                    scope.control_scope_depth >= target_scope.index()
                } else {
                    scope.control_scope_depth > target_scope.index()
                }
            })
            .flat_map(|scope| scope.expressions.iter().rev().cloned())
            .collect()
    }

    pub fn push_defer_scope(&mut self) {
        let control_scope_depth = self
            .defer_scopes
            .last()
            .map(|scope| scope.control_scope_depth)
            .unwrap_or_default()
            .max(self.current_scope_index().index());
        self.defer_scopes.push(DeferScope {
            control_scope_depth,
            expressions: Vec::new(),
        });
    }

    pub fn push_child_defer_scope(&mut self) {
        let control_scope_depth = self
            .defer_scopes
            .last()
            .map(|scope| scope.control_scope_depth)
            .unwrap_or_else(|| self.current_scope_index().index())
            + 1;
        self.defer_scopes.push(DeferScope {
            control_scope_depth,
            expressions: Vec::new(),
        });
    }

    pub fn pop_defer_scope(&mut self) {
        self.defer_scopes
            .pop()
            .expect("defer scope stack has uneven push/pop");
    }

    pub fn current_snapshot(&self) -> ControlFlowSnapshot {
        self.flow().current_snapshot()
    }

    pub fn restore_snapshot(&mut self, snapshot: &ControlFlowSnapshot) {
        self.flow_mut().restore_snapshot(snapshot);
    }

    pub fn current_scope_index(&self) -> ScopeId {
        self.flow().current_scope_index()
    }

    pub fn set_scope_anchor(&mut self, expr: &CXExpression) {
        self.flow_mut().set_scope_anchor(expr);
    }

    pub fn configure_merge_scope(
        &mut self,
        expr: &CXExpression,
        include_current_snapshot: Option<&str>,
        require_nodrop_discharge: bool,
    ) {
        self.flow_mut().configure_merge_scope(
            expr,
            include_current_snapshot,
            require_nodrop_discharge,
        );
    }

    pub fn configure_loop_scope(&mut self, expr: &CXExpression, loop_kind: LoopScopeKind) {
        self.flow_mut().configure_loop_scope(expr, loop_kind);
    }

    pub fn set_scope_fallthrough_target(&mut self, target: ScopeExitTarget) {
        self.flow_mut().set_scope_fallthrough_target(target);
    }

    pub fn enqueue_scope_arrow(&mut self, target: &ScopeExitTarget, snapshot: ControlFlowSnapshot) {
        self.flow_mut().enqueue_scope_arrow(target, snapshot);
    }

    pub fn take_pending_increment_arrows(&mut self, scope: ScopeId) -> Vec<ControlFlowArrow> {
        self.flow_mut().take_pending_increment_arrows(scope)
    }

    pub fn loop_entry_snapshot(&self, scope: ScopeId) -> ControlFlowSnapshot {
        self.flow().loop_entry_snapshot(scope)
    }

    pub fn nearest_break_scope(&self) -> Option<ScopeId> {
        self.flow().nearest_break_scope()
    }

    pub fn nearest_continue_scope(&self) -> Option<ScopeId> {
        self.flow().nearest_continue_scope()
    }

    pub fn break_arrow_sink(&self, scope: ScopeId) -> ScopeArrowSink {
        self.flow().break_arrow_sink(scope)
    }

    pub fn continue_arrow_sink(&self, scope: ScopeId) -> ScopeArrowSink {
        self.flow().continue_arrow_sink(scope)
    }

    pub fn mark_jump_unreachable(&mut self, target_scope: ScopeId) {
        self.flow_mut().mark_jump_unreachable(target_scope);
    }

    pub fn mark_current_scope_unreachable(&mut self) {
        self.flow_mut().mark_current_scope_unreachable();
    }

    pub fn set_scope_reachable(&mut self, scope: ScopeId, reachable: bool) {
        self.flow_mut().set_scope_reachable(scope, reachable);
    }

    pub fn is_scope_reachable(&self, scope: ScopeId) -> bool {
        self.flow().is_scope_reachable(scope)
    }

    pub fn is_current_scope_reachable(&self) -> bool {
        self.flow().is_current_scope_reachable()
    }

    pub fn push_yield_context(&mut self, target_scope: ScopeId, result_type: Option<MIRType>) {
        self.yield_stack.push(YieldContext {
            target_scope,
            result_type,
            yield_count: 0,
        });
    }

    pub fn pop_yield_context(&mut self) -> YieldContext {
        self.yield_stack
            .pop()
            .expect("Yield context stack has uneven push/pop")
    }

    pub fn current_yield_context(&self) -> Option<&YieldContext> {
        self.yield_stack.last()
    }

    pub fn current_yield_context_mut(&mut self) -> Option<&mut YieldContext> {
        self.yield_stack.last_mut()
    }

    pub fn current_yield_count(&self) -> usize {
        self.current_yield_context()
            .map(|context| context.yield_count)
            .unwrap_or(0)
    }

    pub fn track_binding(&mut self, name: String, nodrop: bool) {
        self.flow_mut().track_binding(name, nodrop);
    }

    pub fn tracked_binding(&self, name: &str) -> Option<&TrackedBindingState> {
        self.flow().tracked_binding(name)
    }

    pub fn set_tracked_binding_state(&mut self, name: &str, state: BindingMoveState) {
        self.flow_mut().set_tracked_binding_state(name, state);
    }

    pub fn tracked_bindings_snapshot(&self) -> HashMap<String, TrackedBindingState> {
        self.flow().tracked_bindings_snapshot()
    }

    pub fn nodrop_bindings_in_nonfinal_state(&self) -> Vec<String> {
        self.flow().nodrop_bindings_in_nonfinal_state()
    }

    pub fn in_safe_context(&self) -> bool {
        self.require_safe && self.unsafe_depth == 0
    }

    pub fn enter_unsafe(&mut self) {
        self.unsafe_depth += 1;
    }

    pub fn exit_unsafe(&mut self) {
        self.unsafe_depth -= 1;
    }

    pub fn snapshot_mode(&self) -> FunctionModeSnapshot {
        FunctionModeSnapshot {
            safe_mode: self.require_safe,
            contract_pure_mode: self.require_pure,
            unsafe_depth: self.unsafe_depth,
        }
    }

    pub fn set_contract_mode(&mut self, safe: bool) {
        self.require_safe = safe;
        self.require_pure = safe;
        self.unsafe_depth = 0;
    }

    pub fn restore_mode(&mut self, snapshot: FunctionModeSnapshot) {
        self.require_safe = snapshot.safe_mode;
        self.require_pure = snapshot.contract_pure_mode;
        self.unsafe_depth = snapshot.unsafe_depth;
    }
}
