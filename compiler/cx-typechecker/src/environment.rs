use std::path::PathBuf;

use cx_mir::{
    EnvironmentNamespace, MIRUnit,
    mir::data::{MIRFunctionPrototype, MIRType, MIRTypeId},
    symbol::MIRSymbol,
    type_context::MIRTypeContext,
};
use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_tokens::TokenRange;
use cx_tokens::token::Token;
use cx_util::CXResult;
use cx_util::identifier::CXIdent;
use cx_util::namespace::QualifiedName;

pub use crate::environment::functions::control_flow::{
    BindingMoveState, ControlFlowArrow, ControlFlowSnapshot, LoopScopeKind, ScopeArrowSink,
    ScopeExitTarget, ScopeId, TrackedBindingState,
};
use crate::environment::items::ItemRegistry;
use crate::environment::source::SourceContext;
use crate::log::TypeError;
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
            functions: functions,
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

    pub fn get_symbol(
        &mut self,
        namespace: &EnvironmentNamespace,
        name: &QualifiedName,
    ) -> CXResult<Option<MIRSymbol>> {
        if name.namespace.is_root()
            && let Some(local_symbol) = self.symbols.get_local_symbol(name)
        {
            return Ok(Some(local_symbol.clone()));
        }

        if let Some(preresolved_symbol) = self.symbols.get_preresolved_symbol(name) {
            return Ok(Some(preresolved_symbol.clone()));
        }

        let aliased_name = self
            .symbols
            .get_global_registry()
            .resolve_qualified_alias(namespace, name);

        if &aliased_name != name
            && let Some(preresolved_symbol) = self.symbols.get_preresolved_symbol(&aliased_name)
        {
            return Ok(Some(preresolved_symbol.clone()));
        }

        let (resolved_name, untyped_symbol) = match self
            .symbols
            .get_global_registry()
            .resolve(&aliased_name)
            .map(|symbol| (aliased_name.clone(), symbol))
            .or_else(|| {
                name.namespace.is_root().then(|| {
                    self.symbols
                        .get_global_registry()
                        .resolve(name)
                        .map(|symbol| (name.clone(), symbol))
                })?
            }) {
            Some(result) => result,
            None => return Ok(None),
        };

        let symbol = resolve_symbol(
            self,
            namespace,
            &resolved_name.namespace,
            &resolved_name.name,
            &untyped_symbol,
        )?;

        self.symbols.insert_symbol(resolved_name, symbol.clone());
        Ok(Some(symbol))
    }
}

// Under consideration -- functions that may be removed in the refactor
impl TypeEnvironment<'_> {
    pub fn type_error_at_range<T>(
        &self,
        range: &TokenRange,
        message: String,
        notes: Vec<String>,
    ) -> CXResult<T> {
        let (byte_start, byte_end) = crate::log::byte_range_for_tokens(
            self.source.tokens,
            range.start_token,
            range.end_token,
        );
        let compilation_unit = (!range.file_origin.is_empty())
            .then(|| PathBuf::from(range.file_origin.as_ref()))
            .or_else(|| {
                crate::log::file_origin_for_tokens(
                    self.source.tokens,
                    range.start_token,
                    range.end_token,
                )
            })
            .unwrap_or_else(|| self.source.compilation_unit.as_path().to_owned());
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

    pub fn type_eq(&self, type1: &MIRType, type2: &MIRType) -> bool {
        type1.contextual_eq(type2, &self.symbols)
    }

    pub fn get_named_type_definition(&self, id: MIRTypeId) -> Option<&MIRType> {
        self.symbols
            .contains(id)
            .then(|| self.symbols.resolve_type_id(id))
    }
}
