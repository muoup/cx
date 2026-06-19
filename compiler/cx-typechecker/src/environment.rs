use cx_ast::ast::modifiers::VisibilityMode;
use cx_ast::symbols::CXSymbol;
use cx_log::{CXResult, CXUnspannedError};
use cx_mir::{
    EnvironmentNamespace, MIRUnit,
    mir::contextual_eq::TypeContextEqual,
    mir::data::{MIRFunctionPrototype, MIRType, MIRTypeId},
    symbol::MIRSymbol,
    type_context::MIRTypeContext,
};
use cx_namespace::{MIRQualifiedLookup, result::QualifiedLookupResult};
use cx_pipeline_data::db::ModuleData;
use cx_tokens::TokenRange;
use cx_util::namespace::QualifiedName;
use cx_util::{identifier::CXIdent, namespace::NamespacePath};

pub use crate::environment::functions::control_flow::{
    BindingMoveState, ControlFlowArrow, ControlFlowSnapshot, LoopScopeKind, ScopeArrowSink,
    ScopeExitTarget, ScopeId, TrackedBindingState,
};
use crate::{
    environment::functions::context::FunctionContext, symbol::registry::MIRSymbolRegistry,
};
use crate::{
    environment::functions::context::FunctionModeSnapshot, symbol::resolution::resolve_symbol,
};
use crate::{environment::items::ItemRegistry, log_typecheck_error};
pub(crate) mod functions;
pub(crate) mod items;

pub use items::MIRFunctionGenRequest;

pub const DEFER_ACCUMULATION_REGISTER: &str = "__defer_accumulation_register";

pub struct TypeEnvironment<'a> {
    pub current_namespace: EnvironmentNamespace,
    pub module_data: &'a ModuleData,
    pub symbols: MIRSymbolRegistry<'a>,
    pub items: ItemRegistry,
    pub function: FunctionContext,
}

impl TypeEnvironment<'_> {
    pub fn new<'a>(
        current_namespace: EnvironmentNamespace,
        module_data: &'a ModuleData,
    ) -> TypeEnvironment<'a> {
        TypeEnvironment {
            symbols: MIRSymbolRegistry::new(&module_data.symbol_registry),
            current_namespace,
            module_data,
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

    pub fn try_current_function(&self) -> Option<&MIRFunctionPrototype> {
        self.function.try_current_function()
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
            source_namespace: self.current_namespace,
        })
    }

    pub fn push_scope(&mut self, has_break_merge: bool, has_continue_merge: bool) {
        self.symbols.push_local_scope();
        self.function
            .push_scope(has_break_merge, has_continue_merge);
    }

    pub fn pop_scope(&mut self) -> CXResult<()> {
        let tokens = self.module_data.lex_tokens.get(&self.current_namespace);
        let source_path = self
            .module_data
            .unit_for_namespace(&self.current_namespace)
            .map(|unit| unit.as_path().to_owned())
            .unwrap_or_default();

        self.function
            .pop_scope(source_path.as_path(), tokens.as_ref())?;
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

    pub fn get_symbol(
        &mut self,
        namespace: &EnvironmentNamespace,
        name: &QualifiedName,
        range: Option<&TokenRange>,
    ) -> CXResult<Option<MIRSymbol>> {
        self.lookup_symbol(namespace, name, range)?
            .map(|lookup| self.resolve_lookup(namespace, lookup))
            .transpose()
    }

    pub fn lookup_symbol(
        &mut self,
        namespace: &EnvironmentNamespace,
        name: &QualifiedName,
        range: Option<&TokenRange>,
    ) -> CXResult<Option<SymbolLookup>> {
        let qualified_lookup = self.qualified_lookup(namespace, name);

        match qualified_lookup {
            QualifiedLookupResult::Found {
                resolved_name: _,
                value,
            } => Ok(Some(value)),

            QualifiedLookupResult::NotFound => Ok(None),
            QualifiedLookupResult::Ambiguous { candidates } => {
                let message = format!(
                    "Ambiguous Symbol Reference, candidates: {}",
                    candidates
                        .iter()
                        .map(|c| c.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                );

                if let Some(range) = range {
                    return log_typecheck_error!(self, range, "{}", message);
                }

                return CXUnspannedError::result("TYPE ERROR", message);
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
                if candidate.namespace.is_root() {
                    return true;
                }

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

    pub fn type_eq(&self, type1: &MIRType, type2: &MIRType) -> bool {
        type1.contextual_eq(type2, &self.symbols)
    }

    pub fn get_named_type_definition(&self, id: MIRTypeId) -> Option<&MIRType> {
        self.symbols
            .contains(id)
            .then(|| self.symbols.resolve_type_id(id))
    }
}

impl MIRQualifiedLookup for TypeEnvironment<'_> {
    type Output = SymbolLookup;

    fn lookup_local(
        &self,
        _lexical_namespace: &NamespacePath,
        name: &QualifiedName,
    ) -> Option<Self::Output> {
        self.symbols.get_local_symbol(name).map(|sym| SymbolLookup {
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
