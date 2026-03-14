use cx_tokens::token::Token;
use cx_ast::ast::CXExpr;
use cx_ast::data::{CXTemplateInput, CXFunctionKind, CXFunctionPrototype, CXType};
use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_mir::CXTypeMap;
use cx_mir::function_map::CXFnMap;
use cx_mir::intrinsic_types::INTRINSIC_TYPES;
use cx_mir::mir::expression::MIRExpression;
use cx_mir::mir::program::{MIRBaseMappings, MIRFunction, MIRGlobalVariable, MIRUnit};
use cx_mir::mir::types::{MIRFunctionPrototype, MIRType};
use cx_util::identifier::CXIdent;
use cx_util::scoped_map::ScopedMap;
use cx_util::{CXError, CXResult};
use std::collections::HashMap;
use std::path::PathBuf;

use crate::log::TypeError;
use crate::environment::function_query::{
    query_member_function, query_standard_function,
};
use crate::type_completion::{complete_prototype_no_insert, complete_type};

pub(crate) mod function_query;
pub(crate) mod name_mangling;

pub const DEFER_ACCUMULATION_REGISTER: &str = "__defer_accumulation_register";

pub struct TypeEnvironment<'a> {
    pub tokens: &'a [Token],
    pub compilation_unit: CompilationUnit,
    pub working_directory: PathBuf,

    pub module_data: &'a ModuleData,

    pub realized_types: CXTypeMap,
    pub realized_fns: CXFnMap,
    pub realized_globals: HashMap<String, MIRGlobalVariable>,

    pub requests: Vec<MIRFunctionGenRequest>,
    pub current_function: Option<MIRFunctionPrototype>,
    pub arg_vals: Vec<MIRExpression>,

    pub symbol_table: ScopedMap<MIRExpression>,
    pub tracked_bindings: ScopedMap<TrackedBindingState>,
    pub scope_stack: Vec<Scope>,
    pub safe_mode: bool,
    pub contract_pure_mode: bool,
    pub unsafe_depth: usize,

    pub in_external_templated_function: bool,

    pub generated_functions: Vec<MIRFunction>,
}

pub enum MIRFunctionGenRequest {
    Template {
        module_origin: Option<String>,
        kind: CXFunctionKind,
        input: CXTemplateInput,
    },
}

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
pub struct TokenRange {
    pub start_token: usize,
    pub end_token: usize,
}

#[derive(Clone)]
pub struct ControlFlowArrow {
    pub label: String,
    pub snapshot: ControlFlowSnapshot,
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
    pub target_scope_idx: usize,
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

impl TypeEnvironment<'_> {
    pub fn new<'a>(
        tokens: &'a [Token],
        compilation_unit: CompilationUnit,
        working_directory: PathBuf,
        module_data: &'a ModuleData,
    ) -> TypeEnvironment<'a> {
        let intrinsic_types = INTRINSIC_TYPES
            .iter()
            .map(|(name, ty)| (name.to_string(), ty.clone().into()))
            .collect::<HashMap<_, _>>();

        TypeEnvironment {
            tokens,
            compilation_unit,
            working_directory,

            module_data,

            realized_types: intrinsic_types,
            realized_fns: HashMap::new(),
            realized_globals: HashMap::new(),

            current_function: None,

            scope_stack: Vec::new(),
            requests: Vec::new(),
            symbol_table: ScopedMap::new(),
            tracked_bindings: ScopedMap::new(),

            arg_vals: Vec::new(),

            in_external_templated_function: false,
            generated_functions: Vec::new(),
            safe_mode: false,
            contract_pure_mode: false,
            unsafe_depth: 0,
        }
    }

    pub fn resolve_compilation_unit(&self, module: &str) -> CompilationUnit {
        CompilationUnit::from_rooted(module, &self.working_directory)
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

    pub fn pop_scope(&mut self) -> CXResult<()> {
        let Some(scope) = self.scope_stack.last().cloned() else {
            panic!("Scope stack has uneven push/pop");
        };

        let current_scope_nodrop = self.current_scope_nodrop_bindings();
        let current_snapshot = scope.reachable.then(|| self.current_snapshot());
        self.symbol_table.pop_scope();
        self.tracked_bindings.pop_scope();
        self.scope_stack.pop().unwrap();

        let outgoing_snapshot = self.resolve_scope_flow(&scope, current_snapshot.as_ref())?;
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
                return self.type_error_at_range(
                    range,
                    format!(
                        "TYPE ERROR:  nodrop local(s) reach scope end without move or @leak: {}",
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

    pub fn current_scope_index(&self) -> usize {
        self.scope_stack.len() - 1
    }

    pub fn set_scope_anchor(&mut self, expr: &CXExpr) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.anchor_range = Some(TokenRange {
                start_token: expr.start_index,
                end_token: expr.end_index,
            });
        }
    }

    pub fn configure_merge_scope(
        &mut self,
        expr: &CXExpr,
        join_name: impl Into<String>,
        include_current_snapshot: Option<&str>,
        require_nodrop_discharge: bool,
    ) {
        let entry_snapshot = self.current_snapshot();
        let range = TokenRange {
            start_token: expr.start_index,
            end_token: expr.end_index,
        };

        let scope = self.scope_stack.last_mut().expect("Missing scope to configure");
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

    pub fn configure_loop_scope(&mut self, expr: &CXExpr, loop_kind: LoopScopeKind) {
        let entry_snapshot = self.current_snapshot();
        let range = TokenRange {
            start_token: expr.start_index,
            end_token: expr.end_index,
        };

        let scope = self.scope_stack.last_mut().expect("Missing scope to configure");
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
        let scope = self.scope_stack.last_mut().expect("Missing scope to configure");
        scope.natural_exit_target = Some(target);
    }

    pub fn enqueue_scope_arrow(&mut self, target: &ScopeExitTarget, snapshot: ControlFlowSnapshot) {
        let arrow = ControlFlowArrow {
            label: target.label.clone(),
            snapshot,
        };
        let scope = self
            .scope_stack
            .get_mut(target.target_scope_idx)
            .expect("Invalid target scope for control-flow arrow");

        match (&mut scope.flow_kind, target.sink) {
            (ScopeFlowKind::Merge(state), ScopeArrowSink::Merge) => state.incoming_arrows.push(arrow),
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

    pub fn take_pending_increment_arrows(&mut self, scope_idx: usize) -> Vec<ControlFlowArrow> {
        let scope = self
            .scope_stack
            .get_mut(scope_idx)
            .expect("Invalid loop scope for pending increment arrows");

        match &mut scope.flow_kind {
            ScopeFlowKind::Loop(state) => std::mem::take(&mut state.pending_increment_arrows),
            _ => panic!("Pending increment arrows requested from non-loop scope"),
        }
    }

    pub fn loop_entry_snapshot(&self, scope_idx: usize) -> ControlFlowSnapshot {
        let scope = self
            .scope_stack
            .get(scope_idx)
            .expect("Invalid loop scope for entry snapshot");

        match &scope.flow_kind {
            ScopeFlowKind::Loop(state) => state.entry_snapshot.clone(),
            _ => panic!("Loop entry snapshot requested from non-loop scope"),
        }
    }

    pub fn nearest_break_scope(&self) -> Option<usize> {
        self.scope_stack
            .iter()
            .rposition(|scope| scope.has_break_merge)
    }

    pub fn nearest_continue_scope(&self) -> Option<usize> {
        self.scope_stack
            .iter()
            .rposition(|scope| scope.has_continue_merge)
    }

    pub fn break_arrow_sink(&self, scope_idx: usize) -> ScopeArrowSink {
        match self.scope_stack.get(scope_idx).map(|scope| &scope.flow_kind) {
            Some(ScopeFlowKind::Loop(_)) => ScopeArrowSink::LoopExit,
            _ => ScopeArrowSink::Merge,
        }
    }

    pub fn continue_arrow_sink(&self, scope_idx: usize) -> ScopeArrowSink {
        match self.scope_stack.get(scope_idx).map(|scope| &scope.flow_kind) {
            Some(ScopeFlowKind::Loop(state)) if state.loop_kind == LoopScopeKind::For => {
                ScopeArrowSink::LoopPendingIncrement
            }
            Some(ScopeFlowKind::Loop(_)) => ScopeArrowSink::LoopContinue,
            _ => ScopeArrowSink::Merge,
        }
    }

    pub fn mark_jump_unreachable(&mut self, target_scope_idx: usize) {
        for idx in (target_scope_idx + 1..self.scope_stack.len()).rev() {
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

    pub fn add_type(
        &mut self,
        name: String,
        _type: MIRType,
    ) -> Option<MIRType> {
        let old = self.realized_types.remove(&name);
        self.realized_types.insert(name.clone(), _type.clone());
        old
    }

    pub fn symbol_value(&self, name: &str) -> Option<&MIRExpression> {
        self.symbol_table.get(name)
    }

    pub fn get_realized_func(&self, name: &str) -> Option<MIRFunctionPrototype> {
        self.realized_fns.get(name).cloned()
    }

    pub fn get_type(&mut self, base_data: &MIRBaseMappings, name: &str) -> CXResult<MIRType> {
        let Some(_ty) = base_data.type_data.get_standard(&name.to_string()) else {
            return CXError::create_result(format!("Type not found: {}", name));
        };

        self.complete_type(base_data, &_ty.resource)
    }

    pub fn get_realized_type(&self, name: &str) -> Option<MIRType> {
        self.realized_types.get(name).cloned()
    }

    pub fn current_function(&self) -> &MIRFunctionPrototype {
        self.current_function.as_ref().unwrap()
    }

    pub fn complete_type(
        &mut self,
        base_data: &MIRBaseMappings,
        _type: &CXType,
    ) -> CXResult<MIRType> {
        complete_type(self, base_data, None, _type)
    }

    pub fn complete_prototype(
        &mut self,
        base_data: &MIRBaseMappings,
        external_module: Option<&String>,
        prototype: &CXFunctionPrototype,
    ) -> CXResult<MIRFunctionPrototype> {
        complete_prototype_no_insert(self, base_data, external_module, prototype).inspect(
            |prototype| {
                self.realized_fns
                    .insert(prototype.name.to_string(), prototype.clone());
            },
        )
    }

    pub fn is_copyable(&mut self, ty: &MIRType) -> bool {
        !self.is_nocopy(ty)
    }

    pub fn is_nocopy(&self, ty: &MIRType) -> bool {
        ty.struct_attributes()
            .map(|attributes| attributes.nocopy || attributes.nodrop)
            .unwrap_or(false)
    }

    pub fn is_nodrop(&self, ty: &MIRType) -> bool {
        ty.struct_attributes()
            .map(|attributes| attributes.nodrop)
            .unwrap_or(false)
    }

    pub fn in_safe_context(&self) -> bool {
        self.safe_mode && self.unsafe_depth == 0
    }

    pub fn track_binding(&mut self, name: String, ty: &MIRType) {
        if !self.is_nocopy(ty) {
            return;
        }

        self.tracked_bindings.insert(
            name,
            TrackedBindingState {
                state: BindingMoveState::Available,
                nodrop: self.is_nodrop(ty),
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

        self.tracked_bindings.insert(
            name.to_string(),
            TrackedBindingState { state, ..binding },
        );
    }

    pub fn current_scope_nodrop_bindings(&self) -> Vec<(String, TrackedBindingState)> {
        if self.scope_stack.is_empty() {
            return Vec::new();
        }

        self.tracked_bindings
            .get_all_at_level(self.tracked_bindings.scope_depth())
            .filter(|(_, binding)| binding.nodrop)
            .map(|(name, binding)| (name.clone(), binding.clone()))
            .collect()
    }

    pub fn tracked_bindings_snapshot(&self) -> HashMap<String, TrackedBindingState> {
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

    pub fn get_standard_function(
        &mut self,
        base_data: &MIRBaseMappings,
        expr: &CXExpr,
        key: &CXIdent,
        template_input: Option<&CXTemplateInput>,
    ) -> CXResult<MIRFunctionPrototype> {
        query_standard_function(self, base_data, expr, key, template_input)
    }

    pub fn get_member_function(
        &mut self,
        base_data: &MIRBaseMappings,
        expr: &CXExpr,
        member_type: &MIRType,
        name: &CXIdent,
        template_input: Option<&CXTemplateInput>,
    ) -> CXResult<MIRFunctionPrototype> {
        query_member_function(self, base_data, expr, member_type, name, template_input)
    }

    #[allow(dead_code)]
    fn start_defer(&mut self) {
        todo!()
    }

    pub fn in_defer<F, T>(&mut self, _: F) -> CXResult<T>
    where
        F: FnOnce(&mut Self) -> CXResult<T>,
    {
        todo!()
    }

    pub fn finish_mir_unit(self) -> CXResult<MIRUnit> {
        Ok(MIRUnit {
            functions: self.generated_functions,
            prototypes: self.realized_fns.into_values().collect(),
            global_variables: self.realized_globals.into_values().collect(),
            
            source_path: self.compilation_unit.as_path().to_owned(),
        })
    }
}

impl TypeEnvironment<'_> {
    fn resolve_scope_flow(
        &mut self,
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

                let Some(merged_bindings) = self.merge_binding_states(
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
                        return self.type_error_at_range(
                            &state.join_range,
                            format!(
                                "TYPE ERROR:  nodrop binding(s) must be moved or @leak'ed before function exit: {}",
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

                let continue_range = state.join_range.clone();
                let Some(loop_carried_bindings) = self.merge_binding_states(
                    &state.entry_snapshot,
                    &continue_arrows,
                    &continue_range,
                    "loop continue join",
                )? else {
                    let Some(exit_bindings) = self.merge_binding_states(
                        &state.entry_snapshot,
                        &state.exit_arrows,
                        &state.join_range,
                        "loop exit join",
                    )? else {
                        return Ok(None);
                    };

                    self.apply_merged_bindings(&exit_bindings);
                    return Ok(Some(self.current_snapshot()));
                };

                let loop_exit_snapshot = ControlFlowSnapshot {
                    symbol_table: self.symbol_table.clone(),
                    tracked_bindings: self.tracked_bindings.clone(),
                };
                let mut loop_exit_snapshot = loop_exit_snapshot;
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

                let Some(exit_bindings) = self.merge_binding_states(
                    &state.entry_snapshot,
                    &exit_arrows,
                    &state.join_range,
                    "loop exit join",
                )? else {
                    return Ok(None);
                };

                self.apply_merged_bindings(&exit_bindings);
                Ok(Some(self.current_snapshot()))
            }
        }
    }

    fn merge_binding_states(
        &mut self,
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
                    .map(|(label, state)| format!("{label} => {}", Self::describe_move_state(*state)))
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
        self.type_error_at_range::<Option<Vec<(String, TrackedBindingState)>>>(
            join_range,
            format!(
                "TYPE ERROR:  nocopy binding(s) have inconsistent move state at {join_name}"
            ),
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
        &self,
        range: &TokenRange,
        message: String,
        notes: Vec<String>,
    ) -> CXResult<T> {
        Err(Box::new(TypeError {
            compilation_unit: self.compilation_unit.as_path().to_owned(),
            token_start: range.start_token,
            token_end: range.end_token,
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
