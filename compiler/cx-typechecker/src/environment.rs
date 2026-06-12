use std::path::PathBuf;

use cx_ast::ast::modifiers::VisibilityMode;
use cx_ast::symbols::CXSymbol;
use cx_log::CXResult;
use cx_mir::{
    EnvironmentNamespace, MIRUnit,
    mir::contextual_eq::TypeContextEqual,
    mir::data::{MIRFunctionPrototype, MIRType, MIRTypeId},
    symbol::MIRSymbol,
    type_context::MIRTypeContext,
};
use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_tokens::TokenRange;
use cx_tokens::token::Token;
use cx_util::identifier::CXIdent;
use cx_util::namespace::QualifiedName;

pub use crate::environment::functions::control_flow::{
    BindingMoveState, ControlFlowArrow, ControlFlowSnapshot, LoopScopeKind, ScopeArrowSink,
    ScopeExitTarget, ScopeId, TrackedBindingState,
};
use crate::environment::items::ItemRegistry;
use crate::environment::source::SourceContext;
use crate::{
    environment::functions::context::FunctionContext, symbol::registry::MIRSymbolRegistry,
};
use crate::{
    environment::functions::context::FunctionModeSnapshot, symbol::resolution::resolve_symbol,
};
pub(crate) mod functions;
pub(crate) mod items;
pub(crate) mod source;

pub use items::MIRFunctionGenRequest;

pub const DEFER_ACCUMULATION_REGISTER: &str = "__defer_accumulation_register";

pub struct TypeEnvironment<'a> {
    pub source: SourceContext<'a>,
    pub symbols: MIRSymbolRegistry<'a>,
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
            symbols: MIRSymbolRegistry::new(&module_data.symbol_registry),
            source: SourceContext::new(tokens, compilation_unit, working_directory, module_data),
            items: ItemRegistry::new(),
            function: FunctionContext::default(),
        }
    }

    pub fn get_intrinsic_type(&self, name: &str) -> MIRType {
        self.symbols
            .get_preresolved_symbol(&QualifiedName::new_raw(CXIdent::from(name)))
            .unwrap_or_else(|| panic!("intrinsic type {} not found", name))
            .as_type_id()
            .map(|id| self.symbols.resolve_type_id(id).clone())
            .unwrap()
    }

    pub fn current_function(&self) -> &MIRFunctionPrototype {
        self.function.current_function()
    }

    pub fn in_defer<F, T>(&mut self, f: F) -> CXResult<T>
    where
        F: FnOnce(&mut Self) -> CXResult<T>,
    {
        f(self)
    }

    pub fn finish_mir_unit(self) -> CXResult<MIRUnit> {
        let (functions, globals) = self.items.drain_generated_items();

        Ok(MIRUnit {
            functions,
            global_variables: globals,
            registry: self.symbols.decompose(),
            source_path: self.source.compilation_unit.as_path().to_owned(),
        })
    }

    pub fn push_scope(&mut self, has_break_merge: bool, has_continue_merge: bool) {
        self.symbols.push_local_scope();
        self.function
            .push_scope(has_break_merge, has_continue_merge);
    }

    pub fn pop_scope(&mut self) -> CXResult<()> {
        self.function
            .pop_scope(self.source.compilation_unit.as_path(), self.source.tokens)?;
        self.symbols.pop_local_scope();
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

    pub(crate) fn lookup_symbol(
        &mut self,
        namespace: &EnvironmentNamespace,
        name: &QualifiedName,
        range: Option<TokenRange>,
    ) -> CXResult<Option<SymbolLookup>> {
        if name.namespace.is_root()
            && let Some(local_symbol) = self.symbols.get_local_symbol(name)
        {
            return Ok(Some(SymbolLookup {
                resolved_name: name.clone(),
                kind: SymbolLookupKind::Resolved(local_symbol.clone()),
            }));
        }

        if let Some(preresolved_symbol) = self.symbols.get_preresolved_symbol(name) {
            return Ok(Some(SymbolLookup {
                resolved_name: name.clone(),
                kind: SymbolLookupKind::Resolved(preresolved_symbol.clone()),
            }));
        }

        let mut resolved = Vec::new();
        for candidate in self.symbol_lookup_candidates(namespace, name) {
            let lookup = self.lookup_candidate(namespace, &candidate);

            let Some(lookup) = lookup else {
                continue;
            };

            if name.namespace.is_root() && candidate.namespace == *namespace {
                return Ok(Some(SymbolLookup {
                    resolved_name: candidate,
                    kind: lookup,
                }));
            }

            resolved.push(SymbolLookup {
                resolved_name: candidate,
                kind: lookup,
            });
        }

        if resolved.is_empty() {
            return Ok(None);
        }

        if resolved.len() == 1 {
            return Ok(resolved.pop());
        }

        // Possibility 1 -- if one of the candidates matches exactly with the pre-aliased name, prefer it
        if let Some(exact_match) = resolved
            .iter()
            .position(|lookup| lookup.resolved_name == *name)
        {
            return Ok(Some(resolved.swap_remove(exact_match)));
        }

        // Possibility 2 -- if one of the candidates matches our namespace exactly, prefer it
        if let Some(namespace_match) = resolved
            .iter()
            .position(|lookup| lookup.resolved_name.namespace == *namespace)
        {
            return Ok(Some(resolved.swap_remove(namespace_match)));
        }

        let candidates = resolved
            .iter()
            .map(|lookup| lookup.resolved_name.as_flat_name())
            .collect::<Vec<_>>()
            .join(", ");

        Err(crate::typecheck_error!(
            self,
            range,
            "Symbol '{name}' is ambiguous; candidates: {candidates}"
        ))
    }

    pub fn get_symbol(
        &mut self,
        namespace: &EnvironmentNamespace,
        name: &QualifiedName,
    ) -> CXResult<Option<MIRSymbol>> {
        let Some(lookup) = self.lookup_symbol(namespace, name, None)? else {
            return Ok(None);
        };

        self.resolve_lookup(namespace, lookup).map(Some)
    }

    pub(crate) fn symbol_lookup_candidates(
        &self,
        namespace: &EnvironmentNamespace,
        name: &QualifiedName,
    ) -> Vec<QualifiedName> {
        self.symbols
            .get_global_registry()
            .resolve_qualified_aliases(namespace, name)
    }

    fn lookup_candidate(
        &self,
        namespace: &EnvironmentNamespace,
        candidate: &QualifiedName,
    ) -> Option<SymbolLookupKind> {
        if let Some(preresolved_symbol) = self.symbols.get_preresolved_symbol(candidate) {
            return Some(SymbolLookupKind::Resolved(preresolved_symbol.clone()));
        }

        let symbol = self.symbols.get_global_registry().resolve(candidate)?;
        self.symbol_visible_from(namespace, candidate, &symbol)
            .then_some(SymbolLookupKind::Untyped(symbol))
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
                if candidate.namespace.is_root() {
                    return true;
                }

                if &candidate.namespace == namespace {
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
            &resolved_name.namespace,
            &resolved_name.name,
            &untyped_symbol,
        )?;

        self.symbols.insert_symbol(resolved_name, symbol.clone());
        Ok(symbol)
    }
}

pub(crate) struct SymbolLookup {
    pub resolved_name: QualifiedName,
    pub kind: SymbolLookupKind,
}

pub(crate) enum SymbolLookupKind {
    Resolved(MIRSymbol),
    Untyped(CXSymbol),
}

impl TypeEnvironment<'_> {
    pub fn type_eq(&self, type1: &MIRType, type2: &MIRType) -> bool {
        type1.contextual_eq(type2, &self.symbols)
    }

    pub fn get_named_type_definition(&self, id: MIRTypeId) -> Option<&MIRType> {
        self.symbols
            .contains(id)
            .then(|| self.symbols.resolve_type_id(id))
    }
}
