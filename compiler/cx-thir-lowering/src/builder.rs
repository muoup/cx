use std::collections::{HashMap, HashSet};

use cx_mir::{
    MIRBasicBlockID, MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunction,
    MIRFunctionID, MIRFunctionMode, MIRGlobalID, MIRInstrKind, MIRPlace, MIRRegister,
    MIRScopeID, MIRTypeRegistryBuilder, MIRTypeID, MIRUnit, MIRValue,
};
use cx_mir_comptime::context::MIRContext;
use cx_thir::{
    THIRUnit,
    registry::THIRDecomposedRegistry,
    thir::{
        data::{THIRComptimeFnPrototype, THIRFnPrototype},
        r#type::THIRTypeID,
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;
use cx_util::linkage::LinkageMode;

mod function;
mod module;

use crate::lowering::types::{lower_type, lower_type_id};
use function::{FunctionBuilder, YieldContext};
use module::{MIRModuleBuilder, ModuleParts};

pub struct MIRBuilder<'thir> {
    types: MIRTypeRegistryBuilder,
    module: MIRModuleBuilder,
    registry: &'thir THIRDecomposedRegistry,

    pub(crate) lowering_types: HashSet<THIRTypeID>,
    function: Option<FunctionBuilder>,
    ambient_prototype: MIRFnPrototype,
    source_range: TokenRange,
}

impl<'thir> MIRBuilder<'thir> {
    pub fn new(thir: &'thir THIRUnit) -> Self {
        let mut builder = Self {
            types: MIRTypeRegistryBuilder::new(*thir.registry.architecture()),
            module: MIRModuleBuilder::new(),
            registry: &thir.registry,
            lowering_types: HashSet::new(),
            function: None,

            ambient_prototype: MIRFnPrototype::new(
                MIRFnSignature::new(
                    CXIdent::from("__cx_ambient".to_string()),
                    None,
                    Vec::new(),
                    MIRTypeID::new(0),
                    MIRFunctionMode::Comptime,
                    false,
                    true,
                ),
                LinkageMode::Static,
            ),
            source_range: TokenRange::internal(),
        };
        builder
            .types
            .reserve_id_space(thir.registry.type_id_bound());

        let unit = thir
            .registry
            .intrinsic_type_id("void")
            .expect("THIR registry is missing the intrinsic void type");

        let void_type = match lower_type_id(&mut builder, unit) {
            Ok(id) => id,
            Err(_) => unreachable!("intrinsic void type must lower"),
        };
        builder.ambient_prototype.signature.return_type = void_type;
        builder
    }

    pub fn registry(&self) -> &THIRDecomposedRegistry {
        self.registry
    }

    pub(crate) fn types(&self) -> &MIRTypeRegistryBuilder {
        &self.types
    }

    pub(crate) fn types_mut(&mut self) -> &mut MIRTypeRegistryBuilder {
        &mut self.types
    }

    pub(crate) fn module(&self) -> &MIRModuleBuilder {
        &self.module
    }

    pub(crate) fn module_mut(&mut self) -> &mut MIRModuleBuilder {
        &mut self.module
    }

    pub(crate) fn current_fn(&self) -> &FunctionBuilder {
        self.function
            .as_ref()
            .expect("no MIR function is currently active")
    }

    pub(crate) fn current_fn_mut(&mut self) -> &mut FunctionBuilder {
        self.function
            .as_mut()
            .expect("no MIR function is currently active")
    }

    pub fn in_function(&self) -> bool {
        self.function.is_some()
    }

    pub(crate) fn set_source_range(&mut self, range: TokenRange) -> TokenRange {
        std::mem::replace(&mut self.source_range, range)
    }

    pub(crate) fn restore_source_range(&mut self, range: TokenRange) {
        self.source_range = range;
    }

    pub fn emit(&mut self, instruction: MIRInstrKind) {
        let range = self.source_range.clone();
        self.current_fn_mut().emit(instruction, range);
    }

    pub fn new_block(&mut self, name: impl Into<CXIdent>) -> MIRBasicBlockID {
        self.current_fn_mut().body_mut().add_block_named(name)
    }

    pub fn current_block(&self) -> MIRBasicBlockID {
        self.current_fn().current_block()
    }

    pub fn set_current_block(&mut self, block: MIRBasicBlockID) {
        self.current_fn_mut().set_current_block(block);
    }

    pub fn current_block_terminated(&self) -> bool {
        self.current_fn().current_block_terminated()
    }

    pub fn label_block(&mut self, name: &CXIdent) -> MIRBasicBlockID {
        self.current_fn_mut().label_block(name)
    }

    pub fn declare_label(&mut self, name: &CXIdent) -> MIRBasicBlockID {
        self.current_fn_mut().declare_label(name)
    }

    pub fn create(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        nodrop: bool,
    ) -> MIRPlace {
        let place = self.place(ty, debug_name, nodrop);
        self.emit(MIRInstrKind::Create { out: place, ty });
        place
    }

    pub fn register(&mut self, ty: MIRTypeID, debug_name: Option<CXIdent>) -> MIRRegister {
        self.current_fn_mut().register(ty, debug_name)
    }

    pub fn register_type(&self, register: MIRRegister) -> Option<MIRTypeID> {
        self.current_fn().register_type(register)
    }

    pub fn block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        self.current_fn_mut().block_param(block, ty, debug_name)
    }

    pub fn place(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        nodrop: bool,
    ) -> MIRPlace {
        self.current_fn_mut().place(ty, debug_name, nodrop)
    }

    pub fn bind_local(&mut self, local: cx_thir::thir::expression::THIRLocalID, place: MIRPlace) {
        self.current_fn_mut().bind_local(local, place);
    }

    pub fn bind_local_value(
        &mut self,
        local: cx_thir::thir::expression::THIRLocalID,
        value: MIRValue,
    ) {
        self.current_fn_mut().bind_local_value(local, value);
    }

    pub fn local(
        &self,
        local: cx_thir::thir::expression::THIRLocalID,
    ) -> Option<MIRPlace> {
        self.current_fn().local(local)
    }

    pub fn local_value(
        &self,
        local: cx_thir::thir::expression::THIRLocalID,
    ) -> Option<MIRValue> {
        self.current_fn().local_value(local)
    }

    pub fn push_named_scope(&mut self) {
        self.current_fn_mut().push_named_scope();
    }

    pub fn pop_named_scope(&mut self) {
        self.current_fn_mut().pop_named_scope();
    }

    pub fn bind_named(&mut self, name: &CXIdent, value: MIRValue) {
        self.current_fn_mut().bind_named(name, value);
    }

    pub fn named(&self, name: &CXIdent) -> Option<MIRValue> {
        self.current_fn().named(name)
    }

    pub fn push_lexical_scope(&mut self, token_range: TokenRange) -> MIRScopeID {
        self.current_fn_mut().push_lexical_scope(token_range)
    }

    pub fn pop_lexical_scope(&mut self) -> (MIRScopeID, Vec<cx_thir::thir::expression::THIRExpression>) {
        self.current_fn_mut().pop_lexical_scope()
    }

    pub fn lexical_scope_exits_to(
        &self,
        depth: usize,
    ) -> Vec<(MIRScopeID, Vec<cx_thir::thir::expression::THIRExpression>)> {
        self.current_fn().lexical_scope_exits_to(depth)
    }

    pub fn register_defer(&mut self, expression: cx_thir::thir::expression::THIRExpression) {
        self.current_fn_mut().register_defer(expression);
    }

    pub fn root_defers(&self) -> Vec<cx_thir::thir::expression::THIRExpression> {
        self.current_fn().root_defers()
    }

    pub fn push_contextual_scope(
        &mut self,
        break_target: MIRBasicBlockID,
        continue_target: Option<MIRBasicBlockID>,
    ) {
        self.current_fn_mut()
            .push_contextual_scope(break_target, continue_target);
    }

    pub fn pop_loop(&mut self) {
        self.current_fn_mut().pop_loop();
    }

    pub fn break_target(&self) -> Option<MIRBasicBlockID> {
        self.current_fn().break_target()
    }

    pub fn continue_target(&self) -> Option<MIRBasicBlockID> {
        self.current_fn().continue_target()
    }

    pub fn break_scope_depth(&self) -> Option<usize> {
        self.current_fn().break_scope_depth()
    }

    pub fn continue_scope_depth(&self) -> Option<usize> {
        self.current_fn().continue_scope_depth()
    }

    pub fn push_yield(&mut self, target: MIRBasicBlockID, result_type: Option<MIRTypeID>) {
        self.current_fn_mut().push_yield(target, result_type);
    }

    pub(crate) fn pop_yield(&mut self) -> YieldContext {
        self.current_fn_mut().pop_yield()
    }

    pub fn yield_target(&self) -> Option<MIRBasicBlockID> {
        self.current_fn().yield_target()
    }

    pub fn yield_scope_depth(&self) -> Option<usize> {
        self.current_fn().yield_scope_depth()
    }

    pub fn yield_result(&self) -> Option<MIRRegister> {
        self.current_fn().yield_result()
    }

    pub fn global_symbol(&mut self, name: &str) -> Option<MIRGlobalID> {
        self.module.global_symbol(name)
    }

    pub fn function_symbol(&mut self, name: &str) -> Option<MIRFunctionID> {
        self.module.function_symbol(name)
    }

    pub fn finish(self) -> MIRUnit {
        let parts: ModuleParts = self.module.into_parts();

        let functions: HashMap<_, _> = parts
            .functions
            .into_iter()
            .filter(|(_, function)| {
                let linkage = function.prototype().linkage;
                let mode = function.mode();
                mode == MIRFunctionMode::Comptime
                    || linkage != LinkageMode::Static
                    || parts.used_functions.contains(&function.id())
            })
            .collect();

        let globals: HashMap<_, _> = parts
            .globals
            .into_iter()
            .filter(|(id, global)| {
                global.linkage != LinkageMode::Static || parts.used_globals.contains(id)
            })
            .collect();

        let global_order: Vec<_> = parts
            .global_order
            .into_iter()
            .filter(|id| globals.contains_key(id))
            .collect();

        MIRUnit::new(self.types, functions, globals, global_order)
    }

    pub(crate) fn convert_prototype(
        &mut self,
        prototype: &THIRFnPrototype,
        mode: MIRFunctionMode,
    ) -> cx_log::CXResult<MIRFnPrototype> {
        let signature = prototype.signature();

        let mut params = Vec::with_capacity(signature.params.len());
        for parameter in &signature.params {
            let ty = lower_type(self, &parameter._type)?;
            let param = match parameter.name.clone() {
                Some(name) => MIRFnParam::named(name, ty),
                None => MIRFnParam::new(ty),
            };
            params.push(param);
        }

        let return_type = lower_type(self, &signature.return_type)?;

        Ok(MIRFnPrototype::new(
            MIRFnSignature::new(
                CXIdent::from(prototype.symbol_name().to_string()),
                prototype.debug_name().cloned(),
                params,
                return_type,
                mode,
                signature.var_args,
                signature.contract.safe,
            ),
            prototype.linkage(),
        ))
    }

    pub(crate) fn convert_comptime_prototype(
        &mut self,
        prototype: &THIRComptimeFnPrototype,
    ) -> cx_log::CXResult<MIRFnPrototype> {
        let mut params = Vec::with_capacity(prototype.params().len());
        for parameter in prototype.params() {
            let ty = lower_type(self, &parameter.value_type._type)?;
            let param = match parameter.name.clone() {
                Some(name) => MIRFnParam::named(name, ty),
                None => MIRFnParam::new(ty),
            };
            params.push(param);
        }

        let return_type = lower_type(self, &prototype.return_type()._type)?;

        Ok(MIRFnPrototype::new(
            MIRFnSignature::new(
                CXIdent::from(prototype.symbol_name().to_string()),
                prototype.debug_name().cloned(),
                params,
                return_type,
                MIRFunctionMode::Comptime,
                false,
                true,
            ),
            LinkageMode::Static,
        ))
    }

    pub(crate) fn start_new_function(&mut self, proto: MIRFnPrototype) -> MIRFunctionID {
        let id = self.module_mut().declare_function(proto);
        self.start_function(id);
        id
    }

    pub(crate) fn start_function(&mut self, id: MIRFunctionID) {
        let function = self
            .module
            .function(id)
            .cloned()
            .expect("function context must be declared in the module before starting");

        self.function = Some(FunctionBuilder::new(function));
    }

    pub(crate) fn finish_function(&mut self) {
        let Some(fn_builder) = self.function.take() else {
            unreachable!("No function context available at finish_function");
        };

        let (id, body) = fn_builder.concise_finish();
        self.module.define_function(id, body);
    }
}

impl cx_mir_comptime::ComptimeResolver for MIRModuleBuilder {
    fn resolve(&self, id: MIRFunctionID) -> Option<&MIRFunction> {
        self.function(id)
    }
}

impl MIRContext for MIRBuilder<'_> {
    fn current_prototype(&self) -> &MIRFnPrototype {
        match self.function.as_ref() {
            Some(function) => function.prototype(),
            None => &self.ambient_prototype,
        }
    }

    fn comptime_resolver(&self) -> &dyn cx_mir_comptime::ComptimeResolver {
        &self.module
    }

    fn lower_thir(
        &mut self,
        expression: &cx_thir::thir::expression::THIRExpression,
    ) -> cx_log::CXResult<MIRValue> {
        crate::lowering::lower_expression(self, expression)
    }

    fn capture_expression(
        &mut self,
        expression: &cx_thir::thir::expression::THIRExpression,
    ) -> cx_log::CXResult<MIRFunction> {
        use cx_mir::MIRInstrKind;
        use cx_tokens::TokenRange;

        let id = self.module_mut().allocate_function_id();
        let prototype = match self.function.as_ref() {
            Some(active) => active.prototype().clone(),
            None => self.ambient_prototype.clone(),
        };

        let saved_function = self.function.take();
        let saved_range =
            std::mem::replace(&mut self.source_range, TokenRange::internal());

        self.function = Some(FunctionBuilder::new(MIRFunction::new(
            id,
            prototype.clone(),
            None,
        )));

        let result = (|| -> cx_log::CXResult<()> {
            let value = crate::lowering::lower_expression(self, expression)?;
            if !self.current_block_terminated() {
                let frame = self.current_fn_mut();
                frame.emit(
                    MIRInstrKind::Return {
                        value: Some(value),
                    },
                    TokenRange::internal(),
                );
            }
            Ok(())
        })();

        let scratch = self.function.take();
        self.function = saved_function;
        self.restore_source_range(saved_range);

        result?;

        let (id, body) = scratch.expect("capture builder is present").concise_finish();
        Ok(MIRFunction::new(id, prototype, Some(body)))
    }
}

pub(crate) fn integer_type(ty: &cx_thir::thir::r#type::THIRType) -> (cx_mir::MIRIntType, bool) {
    use cx_thir::thir::r#type::{THIRIntType, THIRTypeKind};

    match ty.kind {
        THIRTypeKind::Integer { _type, signed } => (
            match _type {
                THIRIntType::I1 => cx_mir::MIRIntType::I1,
                THIRIntType::I8 => cx_mir::MIRIntType::I8,
                THIRIntType::I16 => cx_mir::MIRIntType::I16,
                THIRIntType::I32 => cx_mir::MIRIntType::I32,
                THIRIntType::I64 => cx_mir::MIRIntType::I64,
                THIRIntType::I128 => cx_mir::MIRIntType::I128,
            },
            signed,
        ),
        _ => (cx_mir::MIRIntType::I64, true),
    }
}
