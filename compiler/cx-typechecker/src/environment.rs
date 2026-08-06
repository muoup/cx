use std::borrow::Borrow;

use cx_ast::ast::modifiers::VisibilityMode;
use cx_ast::symbols::CXSymbol;
use cx_log::{
    CXRawResult, CXResult,
    error::{CXErr, CXErrMsg, CXMaybeRawErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_thir::{
    EnvironmentNamespace, THIRUnit,
    thir::contextual_eq::TypeContextEqual,
    thir::data::{THIRFnPrototype, THIRType},
    symbol::MIRSymbol,
    type_context::THIRTypeContext,
};
use cx_namespace::{THIRQualifiedLookup, result::QualifiedLookupResult};
use cx_pipeline_data::db::ModuleData;
use cx_target::ArchitectureConfig;
use cx_tokens::TokenRange;
use cx_util::namespace::QualifiedName;
use cx_util::{identifier::CXIdent, namespace::NamespacePath};

pub use crate::environment::control_flow::{
    BindingMoveState, ControlFlowArrow, ControlFlowSnapshot, LoopScopeKind, ScopeArrowSink,
    ScopeExitTarget, ScopeId, TrackedBindingState,
};
use crate::environment::items::ItemRegistry;
use crate::{environment::function_context::FunctionContext, symbol::registry::MIRSymbolRegistry};
use crate::{
    environment::function_context::FunctionModeSnapshot, symbol::resolution::resolve_symbol,
};

pub(crate) mod control_flow;
pub(crate) mod function_context;
pub(crate) mod items;

pub use items::MIRFunctionGenRequest;

pub const DEFER_ACCUMULATION_REGISTER: &str = "__defer_accumulation_register";

pub struct TypeEnvironment<'a> {
    pub module_data: &'a ModuleData,
    pub symbols: MIRSymbolRegistry<'a>,
    pub items: ItemRegistry,
    pub function: FunctionContext,
    comptime_emit_bases: Vec<usize>,
    runtime_emit_depth: usize,
    defer_depth: usize,
    staged_expansions: Vec<u64>,
    next_staged_expression_id: u64,
}

impl TypeEnvironment<'_> {
    pub fn new<'a>(
        module_data: &'a ModuleData,
        architecture: ArchitectureConfig,
    ) -> TypeEnvironment<'a> {
        TypeEnvironment {
            symbols: MIRSymbolRegistry::new(&module_data.symbol_registry, architecture),
            module_data,
            items: ItemRegistry::new(),
            function: FunctionContext::default(),
            comptime_emit_bases: Vec::new(),
            runtime_emit_depth: 0,
            defer_depth: 0,
            staged_expansions: Vec::new(),
            next_staged_expression_id: 0,
        }
    }

    pub fn get_intrinsic_type(&self, name: &str) -> THIRType {
        self.symbols
            .get_preresolved_symbol(&QualifiedName::new_raw(CXIdent::from(name)))
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

    pub fn finish_thir_unit(self, source_namespace: EnvironmentNamespace) -> CXResult<THIRUnit> {
        let (functions, globals) = self.items.drain_generated_items();

        Ok(THIRUnit {
            source_namespace,
            functions,
            global_variables: globals,
            registry: self.symbols.decompose(),
        })
    }

    pub fn push_scope(&mut self, has_break_merge: bool, has_continue_merge: bool) {
        self.symbols.push_local_scope();
        self.function
            .push_scope(has_break_merge, has_continue_merge);
    }

    pub fn pop_scope(&mut self) -> CXRawResult<()> {
        self.function.pop_scope()?;
        self.symbols.pop_local_scope();
        CXRawResult::Ok(())
    }

    pub fn push_defer_scope(&mut self) {
        self.function.push_defer_scope();
    }

    pub fn push_child_defer_scope(&mut self) {
        self.function.push_child_defer_scope();
    }

    pub fn pop_defer_scope(&mut self) {
        self.function.pop_defer_scope();
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

    pub fn get_symbol(
        &mut self,
        namespace: &EnvironmentNamespace,
        name: &QualifiedName,
    ) -> CXResult<Option<MIRSymbol>> {
        let lookup = self.lookup_symbol(namespace, name).map_err(|err| {
            CXErr::new(
                err,
                CXInternalContext::error(
                    "symbol lookup failed before a source range was available",
                ),
            )
        })?;

        lookup
            .map(|lookup| self.resolve_lookup(namespace, lookup))
            .transpose()
    }

    pub fn lookup_symbol(
        &mut self,
        namespace: &EnvironmentNamespace,
        name: &QualifiedName,
    ) -> CXRawResult<Option<SymbolLookup>> {
        let qualified_lookup = self.qualified_lookup(namespace, name);

        match qualified_lookup {
            QualifiedLookupResult::Found {
                resolved_name: _,
                value,
            } => CXRawResult::Ok(Some(value)),

            QualifiedLookupResult::NotFound => CXRawResult::Ok(None),
            QualifiedLookupResult::Ambiguous { candidates } => {
                let message = format!(
                    "Ambiguous Symbol Reference, candidates: {}",
                    candidates
                        .iter()
                        .map(|c| c.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                );

                CXStdErrMessage::result("TYPE ERROR", message)
            }
        }
    }

    fn symbol_visible_from(
        &self,
        namespace: &EnvironmentNamespace,
        candidate: &QualifiedName,
        symbol: &CXSymbol,
    ) -> bool {
        match symbol.visibility {
            VisibilityMode::Public => true,
            VisibilityMode::Package | VisibilityMode::Private => {
                if &candidate.namespace == namespace.as_namespace_path() {
                    return true;
                }

                if self
                    .symbols
                    .get_global_registry()
                    .namespaces_are_friends(namespace, &candidate.namespace)
                {
                    return true;
                }

                if matches!(symbol.visibility, VisibilityMode::Package) {
                    return candidate.namespace.strip(namespace).is_some();
                }

                false
            }
        }
    }

    pub(crate) fn resolve_lookup(
        &mut self,
        namespace: &EnvironmentNamespace,
        lookup: SymbolLookup,
    ) -> CXResult<MIRSymbol> {
        let resolved_name = lookup.resolved_name;
        if let SymbolLookupKind::Resolved(symbol) = lookup.kind {
            return Ok(symbol);
        }

        let SymbolLookupKind::Untyped(untyped_symbol) = lookup.kind else {
            unreachable!("resolved lookup was handled above")
        };

        let symbol = resolve_symbol(
            self,
            namespace,
            &EnvironmentNamespace::from(&resolved_name.namespace),
            &resolved_name.name,
            &untyped_symbol,
        )?;

        self.symbols.insert_symbol(resolved_name, symbol.clone());
        Ok(symbol)
    }

    pub fn type_eq(&self, type1: &THIRType, type2: &THIRType) -> bool {
        type1.contextual_eq(type2, &self.symbols)
    }

    pub(crate) fn error(
        &self,
        range: impl Borrow<TokenRange>,
        message: impl Into<String>,
    ) -> CXErr {
        crate::log::produce_(self.module_data, range.borrow(), message, Vec::new())
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

    pub(crate) fn complete_err(&self, err: CXErrMsg, range: &TokenRange) -> CXErr {
        CXErr::new(err, self.module_data.convert_token_range(range))
    }

    pub(crate) fn complete_maybe_err(&self, err: CXMaybeRawErr, range: &TokenRange) -> CXErr {
        match err {
            CXMaybeRawErr::Complete(value) => value,
            CXMaybeRawErr::Raw(err) => self.complete_err(err, range),
        }
    }
}

impl THIRQualifiedLookup for TypeEnvironment<'_> {
    type Output = SymbolLookup;

    fn lookup_local(
        &self,
        _lexical_namespace: &NamespacePath,
        name: &QualifiedName,
    ) -> Option<Self::Output> {
        self.symbols
            .get_local_symbol_avoiding_staged_expansions(name, &self.staged_expansions)
            .map(|sym| SymbolLookup {
                resolved_name: name.clone(),
                kind: SymbolLookupKind::Resolved(sym.clone()),
            })
    }

    fn lookup_exact(
        &self,
        lexical_namespace: &NamespacePath,
        name: &QualifiedName,
    ) -> Option<Self::Output> {
        self.symbols
            .get_preresolved_symbol(name)
            .map(|sym| SymbolLookup {
                resolved_name: name.clone(),
                kind: SymbolLookupKind::Resolved(sym.clone()),
            })
            .or_else(|| {
                self.symbols
                    .get_global_registry()
                    .resolve(name)
                    .filter(|sym| {
                        self.symbol_visible_from(
                            &EnvironmentNamespace::from(lexical_namespace),
                            name,
                            sym,
                        )
                    })
                    .map(|sym| SymbolLookup {
                        resolved_name: name.clone(),
                        kind: SymbolLookupKind::Untyped(sym.clone()),
                    })
            })
    }

    fn resolve_aliases(
        &self,
        lexical_namespace: &NamespacePath,
        namespace: &NamespacePath,
    ) -> Vec<NamespacePath> {
        self.symbols
            .get_global_registry()
            .resolve_aliases(lexical_namespace, namespace)
            .expect("failed to resolve namespace aliases")
    }
}

pub struct SymbolLookup {
    pub resolved_name: QualifiedName,
    pub kind: SymbolLookupKind,
}

pub enum SymbolLookupKind {
    Resolved(MIRSymbol),
    Untyped(CXSymbol),
}
