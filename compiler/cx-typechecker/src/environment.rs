use std::path::PathBuf;

use cx_ast::ast::{
    expression::CXExpression,
    function::CXFunctionPrototype,
    types::{CXType, CXTypeKind, PredeclarationType},
};
use cx_mir::mir::data::{MIRFunctionPrototype, MIRType, MIRTypeId};
use cx_mir::mir::program::{EnvironmentNamespace, MIRFunction, MIRGlobalVariable, MIRUnit};
use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_tokens::TokenRange;
use cx_tokens::token::Token;
use cx_util::CXResult;
use cx_util::identifier::CXIdent;
use cx_util::namespace::{NamespacePath, QualifiedName};

use crate::environment::{functions::completion::{complete_prototype_no_insert, complete_type}, symbols::ResolvedValueSymbol};
use crate::environment::functions::context::FunctionContext;
pub use crate::environment::functions::control_flow::{
    BindingMoveState, ControlFlowArrow, ControlFlowSnapshot, LoopScopeKind, ScopeArrowSink,
    ScopeExitTarget, ScopeId, TrackedBindingState,
};
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
    pub symbols: SymbolRegistry<'a>,
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
            symbols: SymbolRegistry::new(&module_data.symbol_registry),
            items: ItemRegistry::new(),
            function: FunctionContext::default(),
        }
    }

    pub fn resolve_compilation_unit(&self, module: &str) -> CompilationUnit {
        self.source.resolve_compilation_unit(module)
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
        new_name: QualifiedName,
        template_info: Option<Box<cx_mir::mir::data::TemplateInfo>>,
    ) {
        self.symbols
            .update_named_type_metadata(id, new_name, template_info);
    }

    pub fn query_symbol(&self, name: &QualifiedName) -> Option<ResolvedValueSymbol

    pub fn get_realized_func(&self, name: &str) -> Option<MIRFunctionPrototype> {
        self.items.get_realized_func(name)
    }

    pub fn get_type(
        &mut self,
        namespace: &EnvironmentNamespace,
        expr: &CXExpression,
        name: &str,
    ) -> CXResult<MIRType> {
        let path = NamespacePath::from_scoped_path(name);
        let name = path
            .parent_and_name()
            .map(|(namespace, name)| QualifiedName::new(namespace, name))
            .unwrap_or_else(|| QualifiedName::new_raw(CXIdent::new(name)));
        let as_cx_type = CXTypeKind::Identifier {
            predeclaration: PredeclarationType::None,
            name,
        }
        .to_type();

        self.complete_type(namespace, expr, &as_cx_type)
    }

    pub fn get_realized_type(&self, name: &str) -> Option<MIRType> {
        self.symbols.get_realized_type(name)
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

    pub fn current_function(&self) -> &MIRFunctionPrototype {
        self.function.current_function()
    }

    pub fn complete_type(
        &mut self,
        namespace: &EnvironmentNamespace,
        expr: &CXExpression,
        _type: &CXType,
    ) -> CXResult<MIRType> {
        complete_type(self, namespace, None, expr, _type)
    }

    pub fn complete_prototype(
        &mut self,
        namespace: &EnvironmentNamespace,
        external_module: Option<&String>,
        prototype: &CXFunctionPrototype,
    ) -> CXResult<MIRFunctionPrototype> {
        complete_prototype_no_insert(self, namespace, external_module, prototype).inspect(
            |prototype| {
                self.items
                    .realized_fns
                    .insert(prototype.name.to_string(), prototype.clone());
                self.symbols
                    .insert_function_symbol(prototype.name.clone(), prototype.clone());
            },
        )
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

    pub fn set_external_template_origin(&mut self, origin: Option<String>) {
        self.items.external_template_origin = origin;
    }

    pub fn external_template_origin(&self) -> Option<&String> {
        self.items.external_template_origin.as_ref()
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

    pub fn realize_global(&mut self, name: String, global: MIRGlobalVariable) {
        self.items.realized_globals.insert(name, global);
    }
}
