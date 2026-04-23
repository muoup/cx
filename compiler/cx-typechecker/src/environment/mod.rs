use std::path::PathBuf;

use cx_ast::ast::CXExpr;
use cx_ast::data::{CXFunctionPrototype, CXTemplateInput, CXType, CXTypeKind, PredeclarationType};
use cx_mir::mir::data::{MIRFunctionPrototype, MIRType, MIRTypeContext, MIRTypeId};
use cx_mir::mir::expression::MIRExpression;
use cx_mir::mir::program::{MIRBaseMappings, MIRFunction, MIRGlobalVariable, MIRUnit};
use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_tokens::TokenRange;
use cx_tokens::token::Token;
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

use crate::environment::control_flow::ControlFlow;
use crate::environment::function_context::FunctionContext;
use crate::environment::function_query::{query_member_function, query_standard_function};
use crate::environment::items::ItemRegistry;
use crate::environment::source::SourceContext;
use crate::environment::types::{TemplateBindingFrame, TypeRegistry};
use crate::log::TypeError;
use crate::type_completion::{complete_prototype_no_insert, complete_type};

pub(crate) mod control_flow;
pub(crate) mod function_context;
pub(crate) mod function_query;
pub(crate) mod items;
pub(crate) mod name_mangling;
pub(crate) mod source;
pub(crate) mod symbols;
pub(crate) mod types;

pub use control_flow::{
    BindingMoveState, ControlFlowArrow, ControlFlowSnapshot, LoopScopeKind, ScopeArrowSink,
    ScopeExitTarget, ScopeId, TrackedBindingState,
};
pub use items::MIRFunctionGenRequest;

pub const DEFER_ACCUMULATION_REGISTER: &str = "__defer_accumulation_register";

pub struct TypeEnvironment<'a> {
    pub source: SourceContext<'a>,
    pub types: TypeRegistry,
    pub items: ItemRegistry,
    pub function: FunctionContext,
    pub flow: ControlFlow,
}

impl TypeEnvironment<'_> {
    pub fn new<'a>(
        tokens: &'a [Token],
        compilation_unit: CompilationUnit,
        working_directory: PathBuf,
        module_data: &'a ModuleData,
    ) -> TypeEnvironment<'a> {
        TypeEnvironment {
            source: SourceContext::new(tokens, compilation_unit, working_directory, module_data),
            types: TypeRegistry::with_intrinsics(),
            items: ItemRegistry::new(),
            function: FunctionContext::default(),
            flow: ControlFlow::new(),
        }
    }

    pub fn resolve_compilation_unit(&self, module: &str) -> CompilationUnit {
        self.source.resolve_compilation_unit(module)
    }

    pub fn push_scope(&mut self, has_break_merge: bool, has_continue_merge: bool) {
        self.flow.push_scope(has_break_merge, has_continue_merge);
    }

    pub fn pop_scope(&mut self) -> CXResult<()> {
        self.flow.pop_scope(self.source.compilation_unit.as_path())
    }

    pub fn insert_symbol(&mut self, name: String, value: MIRExpression) {
        self.flow.insert_symbol(name, value);
    }

    pub fn symbol_value(&self, name: &str) -> Option<&MIRExpression> {
        self.flow.symbol_value(name)
    }

    pub fn current_snapshot(&self) -> ControlFlowSnapshot {
        self.flow.current_snapshot()
    }

    pub fn restore_snapshot(&mut self, snapshot: &ControlFlowSnapshot) {
        self.flow.restore_snapshot(snapshot);
    }

    pub fn current_scope_index(&self) -> ScopeId {
        self.flow.current_scope_index()
    }

    pub fn set_scope_anchor(&mut self, expr: &CXExpr) {
        self.flow.set_scope_anchor(expr);
    }

    pub fn configure_merge_scope(
        &mut self,
        expr: &CXExpr,
        join_name: impl Into<String>,
        include_current_snapshot: Option<&str>,
        require_nodrop_discharge: bool,
    ) {
        self.flow.configure_merge_scope(
            expr,
            join_name,
            include_current_snapshot,
            require_nodrop_discharge,
        );
    }

    pub fn configure_loop_scope(&mut self, expr: &CXExpr, loop_kind: LoopScopeKind) {
        self.flow.configure_loop_scope(expr, loop_kind);
    }

    pub fn set_scope_fallthrough_target(&mut self, target: ScopeExitTarget) {
        self.flow.set_scope_fallthrough_target(target);
    }

    pub fn enqueue_scope_arrow(&mut self, target: &ScopeExitTarget, snapshot: ControlFlowSnapshot) {
        self.flow.enqueue_scope_arrow(target, snapshot);
    }

    pub fn take_pending_increment_arrows(&mut self, scope: ScopeId) -> Vec<ControlFlowArrow> {
        self.flow.take_pending_increment_arrows(scope)
    }

    pub fn loop_entry_snapshot(&self, scope: ScopeId) -> ControlFlowSnapshot {
        self.flow.loop_entry_snapshot(scope)
    }

    pub fn nearest_break_scope(&self) -> Option<ScopeId> {
        self.flow.nearest_break_scope()
    }

    pub fn nearest_continue_scope(&self) -> Option<ScopeId> {
        self.flow.nearest_continue_scope()
    }

    pub fn break_arrow_sink(&self, scope: ScopeId) -> ScopeArrowSink {
        self.flow.break_arrow_sink(scope)
    }

    pub fn continue_arrow_sink(&self, scope: ScopeId) -> ScopeArrowSink {
        self.flow.continue_arrow_sink(scope)
    }

    pub fn mark_jump_unreachable(&mut self, target_scope: ScopeId) {
        self.flow.mark_jump_unreachable(target_scope);
    }

    pub fn mark_current_scope_unreachable(&mut self) {
        self.flow.mark_current_scope_unreachable();
    }

    pub fn set_scope_reachable(&mut self, scope: ScopeId, reachable: bool) {
        self.flow.set_scope_reachable(scope, reachable);
    }

    pub fn is_scope_reachable(&self, scope: ScopeId) -> bool {
        self.flow.is_scope_reachable(scope)
    }

    pub fn is_current_scope_reachable(&self) -> bool {
        self.flow.is_current_scope_reachable()
    }

    pub fn add_type(&mut self, name: String, _type: MIRType) -> Option<MIRType> {
        self.types.add_type(name, _type)
    }

    pub fn bind_template_types(
        &mut self,
        names: &[String],
        args: &[MIRType],
    ) -> Result<TemplateBindingFrame, String> {
        self.types.bind_template_types(names, args)
    }

    pub fn restore_template_types(&mut self, frame: TemplateBindingFrame) {
        self.types.restore_template_types(frame);
    }

    pub fn get_or_create_named_type_id(&mut self, name: &str) -> MIRTypeId {
        self.types.get_or_create_named_type_id(name)
    }

    pub fn get_named_type_id(&self, name: &str) -> Option<MIRTypeId> {
        self.types.get_named_type_id(name)
    }

    pub fn mark_type_defining(&mut self, id: MIRTypeId) {
        self.types.mark_type_defining(id);
    }

    pub fn finish_type_definition(
        &mut self,
        id: MIRTypeId,
        definition: MIRType,
    ) -> Option<MIRType> {
        self.types.finish_type_definition(id, definition)
    }

    pub fn is_type_defining(&self, id: MIRTypeId) -> bool {
        self.types.is_type_defining(id)
    }

    pub fn abort_type_definition(&mut self, id: MIRTypeId) {
        self.types.abort_type_definition(id);
    }

    pub fn has_complete_named_type_definition(&self, id: MIRTypeId) -> bool {
        self.types.has_complete_named_type_definition(id)
    }

    pub fn get_named_type_definition(&self, id: MIRTypeId) -> Option<&MIRType> {
        self.types.get_named_type_definition(id)
    }

    pub fn intern_type(&mut self, ty: MIRType) -> MIRTypeId {
        self.types.intern_type(ty)
    }

    pub fn update_named_type_metadata(
        &mut self,
        id: MIRTypeId,
        new_name: CXIdent,
        template_info: Option<Box<cx_mir::mir::data::TemplateInfo>>,
    ) {
        self.types
            .update_named_type_metadata(id, new_name, template_info);
    }

    pub fn get_realized_func(&self, name: &str) -> Option<MIRFunctionPrototype> {
        self.items.get_realized_func(name)
    }

    pub fn get_type(
        &mut self,
        base_data: &MIRBaseMappings,
        expr: &CXExpr,
        name: &str,
    ) -> CXResult<MIRType> {
        let as_cx_type = CXTypeKind::Identifier {
            predeclaration: PredeclarationType::None,
            name: CXIdent::new(name),
        }
        .to_type();

        self.complete_type(base_data, expr, &as_cx_type)
    }

    pub fn get_realized_type(&self, name: &str) -> Option<MIRType> {
        self.types.get_realized_type(name)
    }

    pub fn current_function(&self) -> &MIRFunctionPrototype {
        self.function.current_function()
    }

    pub fn begin_function(&mut self, prototype: MIRFunctionPrototype) {
        self.function.begin_function(prototype);
    }

    pub fn end_function(&mut self) {
        self.function.end_function();
    }

    pub fn complete_type(
        &mut self,
        base_data: &MIRBaseMappings,
        expr: &CXExpr,
        _type: &CXType,
    ) -> CXResult<MIRType> {
        complete_type(self, base_data, None, expr, _type)
    }

    pub fn complete_prototype(
        &mut self,
        base_data: &MIRBaseMappings,
        external_module: Option<&String>,
        prototype: &CXFunctionPrototype,
    ) -> CXResult<MIRFunctionPrototype> {
        complete_prototype_no_insert(self, base_data, external_module, prototype).inspect(
            |prototype| {
                self.items
                    .realized_fns
                    .insert(prototype.name.to_string(), prototype.clone());
            },
        )
    }

    pub fn type_context(&self) -> &MIRTypeContext {
        &self.types.context
    }

    pub fn type_context_mut(&mut self) -> &mut MIRTypeContext {
        &mut self.types.context
    }

    pub fn is_copyable(&self, ty: &MIRType) -> bool {
        self.types.is_copyable(ty)
    }

    pub fn is_nocopy(&self, ty: &MIRType) -> bool {
        self.types.is_nocopy(ty)
    }

    pub fn is_nodrop(&self, ty: &MIRType) -> bool {
        self.types.is_nodrop(ty)
    }

    pub fn in_safe_context(&self) -> bool {
        self.function.in_safe_context()
    }

    pub fn track_binding(&mut self, name: String, ty: &MIRType) {
        if !self.is_nocopy(ty) {
            return;
        }

        self.flow.track_binding(name, self.is_nodrop(ty));
    }

    pub fn tracked_binding(&self, name: &str) -> Option<&TrackedBindingState> {
        self.flow.tracked_binding(name)
    }

    pub fn set_tracked_binding_state(&mut self, name: &str, state: BindingMoveState) {
        self.flow.set_tracked_binding_state(name, state);
    }

    pub fn tracked_bindings_snapshot(
        &self,
    ) -> std::collections::HashMap<String, TrackedBindingState> {
        self.flow.tracked_bindings_snapshot()
    }

    pub fn nodrop_bindings_in_nonfinal_state(&self) -> Vec<String> {
        self.flow.nodrop_bindings_in_nonfinal_state()
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

    pub fn in_defer<F, T>(&mut self, _: F) -> CXResult<T>
    where
        F: FnOnce(&mut Self) -> CXResult<T>,
    {
        todo!()
    }

    pub fn finish_mir_unit(self) -> CXResult<MIRUnit> {
        Ok(MIRUnit {
            functions: self.items.generated_functions,
            prototypes: self.items.realized_fns.into_values().collect(),
            global_variables: self.items.realized_globals.into_values().collect(),
            type_definitions: self.types.context,

            source_path: self.source.compilation_unit.as_path().to_owned(),
        })
    }

    pub fn type_error_at_range<T>(
        &self,
        range: &TokenRange,
        message: String,
        notes: Vec<String>,
    ) -> CXResult<T> {
        Err(Box::new(TypeError {
            compilation_unit: self.source.compilation_unit.as_path().to_owned(),
            token_start: range.start_token,
            token_end: range.end_token,
            message,
            notes,
        }))
    }

    pub fn type_eq(&self, type1: &MIRType, type2: &MIRType) -> bool {
        self.types.type_eq(type1, type2)
    }

    pub fn push_unsafe(&mut self) {
        self.function.enter_unsafe();
    }

    pub fn pop_unsafe(&mut self) {
        self.function.exit_unsafe();
    }

    pub fn push_contract_mode(&mut self, safe: bool) -> function_context::FunctionModeSnapshot {
        let snapshot = self.function.snapshot_mode();
        self.function.set_contract_mode(safe);
        snapshot
    }

    pub fn restore_function_mode(&mut self, snapshot: function_context::FunctionModeSnapshot) {
        self.function.restore_mode(snapshot);
    }

    pub fn set_external_templated_function(&mut self, value: bool) {
        self.items.in_external_templated_function = value;
    }

    pub fn push_generated_function(&mut self, function: MIRFunction) {
        self.items.generated_functions.push(function);
    }

    pub fn realize_global(&mut self, name: String, global: MIRGlobalVariable) {
        self.items.realized_globals.insert(name, global);
    }
}
