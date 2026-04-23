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

use crate::environment::functions::completion::{complete_prototype_no_insert, complete_type};
use crate::environment::functions::context::FunctionContext;
pub use crate::environment::functions::control_flow::{
    BindingMoveState, ControlFlowArrow, ControlFlowSnapshot, LoopScopeKind, ScopeArrowSink,
    ScopeExitTarget, ScopeId, TrackedBindingState,
};
use crate::environment::functions::query::{query_member_function, query_standard_function};
use crate::environment::items::ItemRegistry;
use crate::environment::source::SourceContext;
use crate::environment::symbols::{SymbolRegistry, TemplateBindingFrame};
use crate::log::TypeError;

pub(crate) mod functions;
pub(crate) mod items;
pub(crate) mod source;
pub(crate) mod symbols;

pub use items::MIRFunctionGenRequest;

pub const DEFER_ACCUMULATION_REGISTER: &str = "__defer_accumulation_register";

pub struct TypeEnvironment<'a> {
    pub source: SourceContext<'a>,
    pub symbols: SymbolRegistry,
    pub items: ItemRegistry,
    pub function: FunctionContext,
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
            symbols: SymbolRegistry::with_intrinsics(),
            items: ItemRegistry::new(),
            function: FunctionContext::default(),
        }
    }

    pub fn resolve_compilation_unit(&self, module: &str) -> CompilationUnit {
        self.source.resolve_compilation_unit(module)
    }

    pub fn push_scope(&mut self, has_break_merge: bool, has_continue_merge: bool) {
        self.function
            .push_scope(has_break_merge, has_continue_merge);
    }

    pub fn pop_scope(&mut self) -> CXResult<()> {
        self.function
            .pop_scope(self.source.compilation_unit.as_path())
    }

    pub fn insert_symbol(&mut self, name: String, value: MIRExpression) {
        self.function.insert_symbol(name, value);
    }

    pub fn symbol_value(&self, name: &str) -> Option<&MIRExpression> {
        self.function.symbol_value(name)
    }

    pub fn current_snapshot(&self) -> ControlFlowSnapshot {
        self.function.current_snapshot()
    }

    pub fn restore_snapshot(&mut self, snapshot: &ControlFlowSnapshot) {
        self.function.restore_snapshot(snapshot);
    }

    pub fn current_scope_index(&self) -> ScopeId {
        self.function.current_scope_index()
    }

    pub fn set_scope_anchor(&mut self, expr: &CXExpr) {
        self.function.set_scope_anchor(expr);
    }

    pub fn configure_merge_scope(
        &mut self,
        expr: &CXExpr,
        join_name: impl Into<String>,
        include_current_snapshot: Option<&str>,
        require_nodrop_discharge: bool,
    ) {
        self.function.configure_merge_scope(
            expr,
            join_name,
            include_current_snapshot,
            require_nodrop_discharge,
        );
    }

    pub fn configure_loop_scope(&mut self, expr: &CXExpr, loop_kind: LoopScopeKind) {
        self.function.configure_loop_scope(expr, loop_kind);
    }

    pub fn set_scope_fallthrough_target(&mut self, target: ScopeExitTarget) {
        self.function.set_scope_fallthrough_target(target);
    }

    pub fn enqueue_scope_arrow(&mut self, target: &ScopeExitTarget, snapshot: ControlFlowSnapshot) {
        self.function.enqueue_scope_arrow(target, snapshot);
    }

    pub fn take_pending_increment_arrows(&mut self, scope: ScopeId) -> Vec<ControlFlowArrow> {
        self.function.take_pending_increment_arrows(scope)
    }

    pub fn loop_entry_snapshot(&self, scope: ScopeId) -> ControlFlowSnapshot {
        self.function.loop_entry_snapshot(scope)
    }

    pub fn nearest_break_scope(&self) -> Option<ScopeId> {
        self.function.nearest_break_scope()
    }

    pub fn nearest_continue_scope(&self) -> Option<ScopeId> {
        self.function.nearest_continue_scope()
    }

    pub fn break_arrow_sink(&self, scope: ScopeId) -> ScopeArrowSink {
        self.function.break_arrow_sink(scope)
    }

    pub fn continue_arrow_sink(&self, scope: ScopeId) -> ScopeArrowSink {
        self.function.continue_arrow_sink(scope)
    }

    pub fn mark_jump_unreachable(&mut self, target_scope: ScopeId) {
        self.function.mark_jump_unreachable(target_scope);
    }

    pub fn mark_current_scope_unreachable(&mut self) {
        self.function.mark_current_scope_unreachable();
    }

    pub fn set_scope_reachable(&mut self, scope: ScopeId, reachable: bool) {
        self.function.set_scope_reachable(scope, reachable);
    }

    pub fn is_scope_reachable(&self, scope: ScopeId) -> bool {
        self.function.is_scope_reachable(scope)
    }

    pub fn is_current_scope_reachable(&self) -> bool {
        self.function.is_current_scope_reachable()
    }

    pub fn add_type(&mut self, name: String, _type: MIRType) -> Option<MIRType> {
        self.symbols.add_type(name, _type)
    }

    pub fn bind_template_types(
        &mut self,
        names: &[String],
        args: &[MIRType],
    ) -> Result<TemplateBindingFrame, String> {
        self.symbols.bind_template_types(names, args)
    }

    pub fn restore_template_types(&mut self, frame: TemplateBindingFrame) {
        self.symbols.restore_template_types(frame);
    }

    pub fn get_or_create_named_type_id(&mut self, name: &str) -> MIRTypeId {
        self.symbols.get_or_create_named_type_id(name)
    }

    pub fn get_named_type_id(&self, name: &str) -> Option<MIRTypeId> {
        self.symbols.get_named_type_id(name)
    }

    pub fn mark_type_defining(&mut self, id: MIRTypeId) {
        self.symbols.mark_type_defining(id);
    }

    pub fn finish_type_definition(
        &mut self,
        id: MIRTypeId,
        definition: MIRType,
    ) -> Option<MIRType> {
        self.symbols.finish_type_definition(id, definition)
    }

    pub fn is_type_defining(&self, id: MIRTypeId) -> bool {
        self.symbols.is_type_defining(id)
    }

    pub fn abort_type_definition(&mut self, id: MIRTypeId) {
        self.symbols.abort_type_definition(id);
    }

    pub fn has_complete_named_type_definition(&self, id: MIRTypeId) -> bool {
        self.symbols.has_complete_named_type_definition(id)
    }

    pub fn get_named_type_definition(&self, id: MIRTypeId) -> Option<&MIRType> {
        self.symbols.get_named_type_definition(id)
    }

    pub fn intern_type(&mut self, ty: MIRType) -> MIRTypeId {
        self.symbols.intern_type(ty)
    }

    pub fn update_named_type_metadata(
        &mut self,
        id: MIRTypeId,
        new_name: CXIdent,
        template_info: Option<Box<cx_mir::mir::data::TemplateInfo>>,
    ) {
        self.symbols
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
        self.symbols.get_realized_type(name)
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
        &self.symbols.context
    }

    pub fn type_context_mut(&mut self) -> &mut MIRTypeContext {
        &mut self.symbols.context
    }

    pub fn is_copyable(&self, ty: &MIRType) -> bool {
        self.symbols.is_copyable(ty)
    }

    pub fn is_nocopy(&self, ty: &MIRType) -> bool {
        self.symbols.is_nocopy(ty)
    }

    pub fn is_nodrop(&self, ty: &MIRType) -> bool {
        self.symbols.is_nodrop(ty)
    }

    pub fn in_safe_context(&self) -> bool {
        self.function.in_safe_context()
    }

    pub fn track_binding(&mut self, name: String, ty: &MIRType) {
        if !self.is_nocopy(ty) {
            return;
        }

        self.function.track_binding(name, self.is_nodrop(ty));
    }

    pub fn tracked_binding(&self, name: &str) -> Option<&TrackedBindingState> {
        self.function.tracked_binding(name)
    }

    pub fn set_tracked_binding_state(&mut self, name: &str, state: BindingMoveState) {
        self.function.set_tracked_binding_state(name, state);
    }

    pub fn tracked_bindings_snapshot(
        &self,
    ) -> std::collections::HashMap<String, TrackedBindingState> {
        self.function.tracked_bindings_snapshot()
    }

    pub fn nodrop_bindings_in_nonfinal_state(&self) -> Vec<String> {
        self.function.nodrop_bindings_in_nonfinal_state()
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
            type_definitions: self.symbols.context,

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
        self.symbols.type_eq(type1, type2)
    }

    pub fn push_unsafe(&mut self) {
        self.function.enter_unsafe();
    }

    pub fn pop_unsafe(&mut self) {
        self.function.exit_unsafe();
    }

    pub fn push_contract_mode(&mut self, safe: bool) -> functions::context::FunctionModeSnapshot {
        let snapshot = self.function.snapshot_mode();
        self.function.set_contract_mode(safe);
        snapshot
    }

    pub fn restore_function_mode(&mut self, snapshot: functions::context::FunctionModeSnapshot) {
        self.function.restore_mode(snapshot);
    }

    pub fn set_external_templated_function(&mut self, value: bool) {
        self.items.in_external_templated_function = value;
    }

    pub fn request_function_generation(&mut self, request: MIRFunctionGenRequest) {
        self.items.requests.push(request);
    }

    pub fn pop_function_generation_request(&mut self) -> Option<MIRFunctionGenRequest> {
        self.items.requests.pop()
    }

    pub fn push_generated_function(&mut self, function: MIRFunction) {
        self.items.generated_functions.push(function);
    }

    pub fn realize_global(&mut self, name: String, global: MIRGlobalVariable) {
        self.items.realized_globals.insert(name, global);
    }
}
