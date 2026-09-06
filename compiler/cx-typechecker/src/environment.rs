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
        comptime::THIRStagedEffects,
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

struct StagedContext {
    return_type: Option<THIRType>,
}

pub struct TypeEnvironment<'a> {
    pub module_data: &'a ModuleData,
    pub symbols: MIRSymbolRegistry<'a>,
    pub items: ItemRegistry,
    pub function: FunctionContext,

    comptime_emit_bases: Vec<usize>,

    runtime_emit_depth: usize,
    defer_depth: usize,
    staged_contexts: Vec<StagedContext>,
    pub(crate) staged_expansions: Vec<u64>,
    next_staged_expression_id: u64,
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
            comptime_emit_bases: Vec::new(),
            runtime_emit_depth: 0,
            defer_depth: 0,
            staged_contexts: Vec::new(),
            staged_expansions: Vec::new(),
            next_staged_expression_id: 0,
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

    pub fn in_staged<F, T>(&mut self, f: F) -> CXResult<(T, ScopeEffects)>
    where
        F: FnOnce(&mut Self) -> CXResult<T>,
    {
        let context = StagedContext {
            return_type: self.materialization_return_type(),
        };
        self.function.flow_mut().push_staged_scope();
        self.staged_contexts.push(context);
        let result = f(self);
        self.staged_contexts.pop();
        let effects = self
            .function
            .pop_scope()
            .unwrap_or_else(|_| panic!("staged control-flow scope is unbalanced"));
        result.map(|value| (value, effects))
    }

    pub fn in_staged_context(&self) -> bool {
        !self.staged_contexts.is_empty()
    }

    pub fn staged_return_type(&self) -> Option<&THIRType> {
        self.staged_contexts
            .last()
            .and_then(|context| context.return_type.as_ref())
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

    pub fn staged_effects(&self, effects: &ScopeEffects) -> THIRStagedEffects {
        THIRStagedEffects {
            breaks: effects.break_range.is_some(),
            continues: effects.continue_range.is_some(),
            yield_type: effects.yield_type.clone(),
        }
    }

    pub fn apply_staged_effects(
        &mut self,
        effects: &THIRStagedEffects,
        range: &TokenRange,
    ) -> CXResult<()> {
        if effects.breaks {
            if self.function.flow().break_target() == ControlTarget::Invalid {
                return self.log_error(
                    range,
                    "staged break has no target in the materialization context".to_string(),
                );
            }
            self.function.flow_mut().record_break(range.clone());
        }

        if effects.continues {
            if self.function.flow().continue_target() == ControlTarget::Invalid {
                return self.log_error(
                    range,
                    "staged continue has no target in the materialization context".to_string(),
                );
            }
            self.function.flow_mut().record_continue(range.clone());
        }

        if let Some(yield_type) = &effects.yield_type {
            let state = self.function.flow().yield_state();
            if state.target == ControlTarget::Invalid {
                return self.log_error(
                    range,
                    "staged yield has no target in the materialization context".to_string(),
                );
            }
            if let Some(expected_type) = state.expected_type
                && !self.type_eq(&expected_type, yield_type)
            {
                return self.log_error(
                    range,
                    format!(
                        "Staged expression yields {}, but the materialization context expects {}",
                        yield_type.display_with(&self.symbols),
                        expected_type.display_with(&self.symbols),
                    ),
                );
            }
            self.function
                .flow_mut()
                .record_yield(yield_type.clone(), !yield_type.is_void());
        }

        Ok(())
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

    pub fn enter_comptime_context(&mut self) {
        self.comptime_emit_bases.push(self.runtime_emit_depth);
    }

    pub fn exit_comptime_context(&mut self) {
        self.comptime_emit_bases
            .pop()
            .expect("Comptime context stack underflow");
    }

    pub fn in_comptime_context(&self) -> bool {
        !self.comptime_emit_bases.is_empty()
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
        self.comptime_emit_bases
            .last()
            .is_some_and(|base| self.runtime_emit_depth > *base)
    }

    pub fn materialization_return_type(&self) -> Option<THIRType> {
        if let Some(return_type) = self.staged_return_type() {
            return Some(return_type.clone());
        }
        if self.in_comptime_context() {
            return None;
        }
        self.try_current_function()
            .map(|function| function.signature().return_type.clone())
    }

    pub fn next_staged_expression_id(&mut self) -> u64 {
        let id = self.next_staged_expression_id;
        self.next_staged_expression_id += 1;
        id
    }

    pub fn push_staged_expansion(&mut self, id: u64) {
        self.staged_expansions.push(id);
    }

    pub fn pop_staged_expansion(&mut self) {
        self.staged_expansions
            .pop()
            .expect("Staged expression expansion stack underflow");
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
