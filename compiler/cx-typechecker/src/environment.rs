use std::borrow::Borrow;

use cx_log::{
    CXRawResult, CXResult,
    error::{
        CXError, CXErrorMaybeRaw, CXRawError, context::from_token_range, message::CXStdErrMessage,
    },
};
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_pipeline_data::db::ModuleData;
use cx_target::ArchitectureConfig;
use cx_thir::{
    THIRUnit,
    thir::{
        contextual_eq::TypeContextEqual,
        data::{THIRFnPrototype, THIRType},
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

pub use crate::environment::control_flow::{ControlTarget, ScopeEffects};
use crate::{
    environment::function_context::{FunctionContext, FunctionModeSnapshot},
    symbol::registry::MIRSymbolRegistry,
};
use crate::{environment::items::ItemRegistry, log::generate_type_error};

pub(crate) mod control_flow;
pub(crate) mod function_context;
pub(crate) mod items;

pub use items::THIRFunctionGenRequest;

pub use cx_thir::thir::comptime::THIRStagingContext as StagingContext;

pub struct TypeEnvironment<'a> {
    pub module_data: &'a ModuleData,
    pub symbols: MIRSymbolRegistry<'a>,
    pub items: ItemRegistry,
    pub function: FunctionContext,

    runtime_emit_depth: usize,
    defer_depth: usize,
    staged_depth: usize,
    pub(crate) comptime_context: Option<StagingContext>,
    require_explicit_return: bool,
}

impl TypeEnvironment<'_> {
    pub fn new<'a>(
        module_data: &'a ModuleData,
        architecture: ArchitectureConfig,
        require_explicit_return: bool,
    ) -> TypeEnvironment<'a> {
        TypeEnvironment {
            symbols: MIRSymbolRegistry::new(&module_data.symbol_registry, architecture),
            module_data,
            items: ItemRegistry::new(),
            function: FunctionContext::default(),
            runtime_emit_depth: 0,
            defer_depth: 0,
            staged_depth: 0,
            comptime_context: None,
            require_explicit_return,
        }
    }

    pub fn require_explicit_return(&self) -> bool {
        self.require_explicit_return
    }

    pub fn get_intrinsic_type(&self, name: &str) -> THIRType {
        self.symbols
            .cached(&QualifiedName::new_raw(CXIdent::from(name)), false)
            .unwrap_or_else(|| panic!("intrinsic type {} not found", name))
            .as_type_id()
            .map(|id| self.symbols.resolve_type_id(id).clone())
            .unwrap()
    }

    pub fn current_function(&self) -> &THIRFnPrototype {
        self.function.current_function()
    }

    pub fn try_current_function(&self) -> Option<&THIRFnPrototype> {
        self.function.try_current_function()
    }

    pub fn in_defer<F, T>(&mut self, f: F) -> CXResult<T>
    where
        F: FnOnce(&mut Self) -> CXResult<T>,
    {
        self.defer_depth += 1;
        let result = f(self);
        self.defer_depth -= 1;
        result
    }

    pub fn in_defer_context(&self) -> bool {
        self.defer_depth > 0
    }

    pub fn in_staged<F, T>(&mut self, f: F) -> CXResult<T>
    where
        F: FnOnce(&mut Self) -> CXResult<T>,
    {
        self.function.flow_mut().push_staged_scope();
        self.staged_depth += 1;
        let result = f(self);
        self.staged_depth -= 1;
        self.function
            .pop_scope()
            .unwrap_or_else(|_| panic!("staged control-flow scope is unbalanced"));
        result
    }

    pub fn in_staged_context(&self) -> bool {
        self.staged_depth != 0
    }

    pub fn finish_thir_unit(self, source_namespace: NamespacePath) -> CXResult<THIRUnit> {
        let (functions, comptime_functions, globals) = self.items.drain_generated_items();

        Ok(THIRUnit {
            source_namespace,
            functions,
            comptime_functions,
            global_variables: globals,
            registry: self.symbols.decompose(),
        })
    }


    pub fn push_scope(
        &mut self,
        has_break_merge: bool,
        has_continue_merge: bool,
        _scope: TokenRange,
    ) {
        self.symbols.push_local_scope();
        self.function
            .flow_mut()
            .push_scope(has_break_merge, has_continue_merge);
    }

    pub fn push_yield_scope(&mut self, expected_type: Option<THIRType>) {
        self.symbols.push_local_scope();
        self.function.flow_mut().push_yield_scope(expected_type);
    }

    pub fn pop_scope(&mut self) -> CXRawResult<ScopeEffects> {
        let effects = self.function.pop_scope()?;
        self.symbols.pop_local_scope();
        Ok(effects)
    }

    pub fn push_unsafe(&mut self) {
        self.function.enter_unsafe();
    }

    pub fn pop_unsafe(&mut self) {
        self.function.exit_unsafe();
    }

    pub fn push_contract_mode(&mut self, safe: bool) -> FunctionModeSnapshot {
        let snapshot = self.function.snapshot_mode();
        self.function.set_contract_mode(safe);
        snapshot
    }

    pub fn restore_function_mode(&mut self, snapshot: FunctionModeSnapshot) {
        self.function.restore_mode(snapshot);
    }

    pub fn in_comptime_context(&self) -> bool {
        self.comptime_context.is_some()
    }

    pub fn in_runtime_emit<F, T>(&mut self, f: F) -> CXResult<T>
    where
        F: FnOnce(&mut Self) -> CXResult<T>,
    {
        self.runtime_emit_depth += 1;
        let result = f(self);
        self.runtime_emit_depth -= 1;
        result
    }

    pub fn in_runtime_emit_context(&self) -> bool {
        self.in_comptime_context() && self.runtime_emit_depth != 0
    }

    pub fn staging_context(&self) -> StagingContext {
        let mut context = self.comptime_context.clone().unwrap_or_else(|| StagingContext {
            return_type: self.try_current_function().map(|f| f.signature().return_type.clone()),
            yield_type: None,
        });
        if self.try_current_function().is_some()
            && (!self.in_comptime_context() || self.in_runtime_emit_context())
        {
            context.yield_type = self.function.flow().yield_state().expected_type.or(context.yield_type);
        }
        context
    }

    pub fn type_eq(&self, type1: &THIRType, type2: &THIRType) -> bool {
        type1.contextual_eq(type2, &self.symbols)
    }

    pub(crate) fn error(
        &self,
        range: impl Borrow<TokenRange>,
        message: impl Into<String>,
    ) -> CXError {
        generate_type_error(range.borrow(), message, Vec::new())
    }

    pub(crate) fn log_error_base<T>(&self, message: impl Into<String>) -> CXRawResult<T> {
        CXStdErrMessage::result("TYPE ERROR", message.into())
    }

    pub(crate) fn log_error<T>(
        &self,
        range: impl Borrow<TokenRange>,
        message: impl Into<String>,
    ) -> CXResult<T> {
        Err(self.error(range, message))
    }

    pub(crate) fn complete_err(&self, err: CXRawError, range: &TokenRange) -> CXError {
        CXError::new(err, from_token_range(range))
    }

    pub(crate) fn complete_maybe_err(&self, err: CXErrorMaybeRaw, range: &TokenRange) -> CXError {
        match err {
            CXErrorMaybeRaw::Complete(value) => value,
            CXErrorMaybeRaw::Raw(err) => self.complete_err(err, range),
        }
    }
}
