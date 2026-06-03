use std::path::PathBuf;

use cx_mir::program::{MIRFunction, MIRGlobalVariable, MIRUnit};
use cx_mir::{
    mir::data::{MIRFunctionPrototype, MIRType, MIRTypeId},
    registry::MIRSymbolRegistry,
    type_context::MIRTypeContext,
};
use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_tokens::TokenRange;
use cx_tokens::token::Token;
use cx_util::CXResult;
use cx_util::identifier::CXIdent;
use cx_util::namespace::QualifiedName;

use crate::environment::functions::context::FunctionContext;
use crate::environment::functions::context::FunctionModeSnapshot;
pub use crate::environment::functions::control_flow::{
    BindingMoveState, ControlFlowArrow, ControlFlowSnapshot, LoopScopeKind, ScopeArrowSink,
    ScopeExitTarget, ScopeId, TrackedBindingState,
};
use crate::environment::items::ItemRegistry;
use crate::environment::source::SourceContext;
use crate::log::TypeError;

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
            source: SourceContext::new(tokens, compilation_unit, working_directory, module_data),
            symbols: MIRSymbolRegistry::new(&module_data.symbol_registry),
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
            registry: cx_mir::registry::MIRDecomposedRegistry::decompose_registry(self.symbols),

            source_path: self.source.compilation_unit.as_path().to_owned(),
        })
    }

    pub fn push_scope(&mut self, has_break_merge: bool, has_continue_merge: bool) {
        self.symbols.push_scope();
        self.function
            .push_scope(has_break_merge, has_continue_merge);
    }

    pub fn pop_scope(&mut self) -> CXResult<()> {
        self.function
            .pop_scope(self.source.compilation_unit.as_path(), self.source.tokens)?;
        self.symbols.pop_scope();
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

    pub fn request_function_generation(&mut self, request: MIRFunctionGenRequest) {
        self.items.requests.push(request);
    }

    pub fn pop_request(&mut self) -> Option<MIRFunctionGenRequest> {
        self.items.requests.pop()
    }

    pub fn push_generated_function(&mut self, function: MIRFunction) {
        self.items.generated_functions.push(function);
    }

    pub fn push_generated_global(&mut self, name: String, global: MIRGlobalVariable) {
        self.items.realized_globals.insert(name, global);
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

    pub fn set_external_templated_function(&mut self, value: bool) {
        self.items.in_external_templated_function = value;
    }

    pub fn set_external_template_origin(&mut self, origin: Option<String>) {
        self.items.external_template_origin = origin;
    }

    pub fn external_template_origin(&self) -> Option<&String> {
        self.items.external_template_origin.as_ref()
    }
}
