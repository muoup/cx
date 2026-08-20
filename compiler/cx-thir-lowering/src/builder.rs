use std::collections::HashSet;

use cx_mir::global::MIRGlobalKind;
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{
    MIRBasicBlockID, MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunctionID,
    MIRGlobalID, MIRGlobalState, MIRInstrKind, MIRIntType, MIRParameterID, MIRPlace, MIRRegister,
    MIRScopeID, MIRTypeID, MIRTypeRegistryBuilder, MIRUnit, MIRValue,
};
use cx_thir::thir::global::THIRGlobalVariable;
use cx_thir::{
    THIRUnit,
    registry::THIRDecomposedRegistry,
    thir::{
        data::{THIRFnPrototype, THIRFunction},
        expression::{THIRExpression, THIRLocalID},
        r#type::{THIRType, THIRTypeID, THIRTypeKind},
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

mod function;
mod module;

use crate::lowering::types::{lower_int_type, lower_type, lower_type_id};
use function::{FunctionContext, LoopContext, YieldContext};
use module::MIRModuleState;

pub struct MIRBuilder<'thir> {
    types: MIRTypeRegistryBuilder,
    module: MIRModuleState,
    registry: &'thir THIRDecomposedRegistry,

    pub(crate) lowering_types: HashSet<THIRTypeID>,
    function: Option<FunctionContext>,
    source_range: TokenRange,

    next_anonymous_symbol: usize,
}

impl<'thir> MIRBuilder<'thir> {
    pub fn new(thir: &'thir THIRUnit) -> Self {
        let mut builder = Self {
            types: MIRTypeRegistryBuilder::new(*thir.registry.architecture()),
            module: MIRModuleState::new(),
            registry: &thir.registry,
            lowering_types: HashSet::new(),
            function: None,
            source_range: TokenRange::internal(),

            next_anonymous_symbol: 0,
        };
        builder
            .types
            .reserve_id_space(thir.registry.type_id_bound());

        let unit = thir
            .registry
            .intrinsic_type_id("void")
            .expect("THIR registry is missing the intrinsic void type");

        lower_type_id(&mut builder, unit);
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

    pub fn finish(self) -> MIRUnit {
        assert!(
            self.function.is_none(),
            "attempted to finish MIR while a function is active"
        );
        self.module.finish(self.types)
    }

    pub(crate) fn set_global_state(&mut self, id: MIRGlobalID, state: MIRGlobalState) {
        self.module.set_global_state(id, state);
    }

    pub(crate) fn predeclare_function(&mut self, function: &THIRFunction) {
        let prototype = self.convert_prototype(&function.prototype);
        self.module.declare_function(prototype);
    }

    pub(crate) fn predeclare_global(&mut self, global: &THIRGlobalVariable) {
        let ty = lower_type(self, &global._type);

        self.module.declare_global(
            global.name.clone(),
            global.linkage,
            MIRGlobalKind::Variable {
                ty,
                state: MIRGlobalState::External,
                is_mutable: global.is_mutable,
                is_nodrop: global._type.is_nodrop(),
            },
            false
        );
    }

    pub(crate) fn convert_prototype(&mut self, prototype: &THIRFnPrototype) -> MIRFnPrototype {
        let mut lowered = self.prototype_from_signature(
            CXIdent::new(prototype.symbol_name()),
            prototype.signature(),
            prototype.linkage(),
        );
        lowered.signature.debug_name = prototype.debug_name().cloned();
        lowered
    }

    fn prototype_from_signature(
        &mut self,
        name: CXIdent,
        signature: &cx_thir::thir::data::THIRFnSignature,
        linkage: LinkageMode,
    ) -> MIRFnPrototype {
        let params = signature
            .params
            .iter()
            .map(|param| {
                let nodrop = param._type.is_nodrop();
                let ty = lower_type(self, &param._type);
                match &param.name {
                    Some(name) => MIRFnParam::named(name.clone(), ty),
                    None => MIRFnParam::new(ty),
                }
                .with_nodrop(nodrop)
            })
            .collect();
        let return_type = if matches!(signature.return_type.kind, THIRTypeKind::Void) {
            self.types().unit()
        } else {
            lower_type(self, &signature.return_type)
        };
        let mut lowered = MIRFnSignature::new(name, params, return_type);
        lowered.variadic = signature.var_args;
        lowered.safe = signature.contract.safe;
        MIRFnPrototype::new(lowered, linkage)
    }

    pub(crate) fn start_function(
        &mut self,
        index: usize,
        function: &THIRFunction,
        body: &THIRExpression,
    ) {
        assert!(self.function.is_none(), "a MIR function is already active");
        let function_id = *self
            .module
            .function_ids()
            .get(index)
            .expect("THIR function predeclaration is missing");
        let mir = self.module.take_function(function_id);
        let (id, prototype, mut definition) = mir.into_definition();
        let entry = definition.add_block();
        let root_scope = definition.add_scope(body.token_range.clone());

        self.function = Some(FunctionContext::new(
            id, prototype, definition, entry, root_scope,
        ));
        self.context_mut().set_block_name(entry, "entry");

        for (index, parameter) in function.prototype.signature().params.iter().enumerate() {
            let place = MIRPlace::Parameter(MIRParameterID::new(index));
            if let Some(local_id) = parameter.local_id {
                self.bind_local(local_id, place);
            }
            if let Some(name) = &parameter.name {
                self.bind_named(name, MIRValue::Place(place));
            }
        }
    }

    pub fn add_string_literal(&mut self, value: &str) -> MIRGlobalID {
        self.next_anonymous_symbol += 1;
        
        let name_ident = CXIdent::from(format!("__anon_{}", self.next_anonymous_symbol));
        self.module.declare_global(
            name_ident,
            LinkageMode::Static,
            MIRGlobalKind::StringLiteral {
                value: value.to_owned(),
            },
            true
        )
    }

    pub(crate) fn finish_function(&mut self) {
        let context = self
            .function
            .take()
            .expect("attempted to finish without an active MIR function");
        self.module
            .insert_function(context.finish(self.types().unit()));
    }

    pub(crate) fn current_function_id(&self) -> MIRFunctionID {
        self.context().id()
    }

    pub(crate) fn current_block(&self) -> MIRBasicBlockID {
        self.context().current_block()
    }

    pub(crate) fn set_current_block(&mut self, block: MIRBasicBlockID) {
        self.context_mut().set_current_block(block);
    }

    pub(crate) fn new_block(&mut self, debug_name: &str) -> MIRBasicBlockID {
        self.context_mut().new_block(debug_name)
    }

    pub(crate) fn current_block_terminated(&self) -> bool {
        self.context().current_block_terminated()
    }

    pub(crate) fn label_block(&mut self, name: &CXIdent) -> MIRBasicBlockID {
        self.context_mut().label_block(name)
    }

    pub(crate) fn declare_label(&mut self, name: &CXIdent) -> MIRBasicBlockID {
        self.context_mut().declare_label(name)
    }

    pub(crate) fn set_source_range(&mut self, range: TokenRange) -> TokenRange {
        std::mem::replace(&mut self.source_range, range)
    }

    pub(crate) fn restore_source_range(&mut self, range: TokenRange) {
        self.source_range = range;
    }

    pub(crate) fn emit(&mut self, instruction: MIRInstrKind) -> bool {
        let range = self.source_range.clone();
        self.context_mut().emit(instruction, range)
    }

    pub(crate) fn register(&mut self, ty: MIRTypeID, debug_name: Option<CXIdent>) -> MIRRegister {
        self.context_mut().register(ty, debug_name)
    }

    pub(crate) fn register_type(&self, register: MIRRegister) -> Option<MIRTypeID> {
        self.context().register_type(register)
    }

    pub(crate) fn block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        self.context_mut().block_param(block, ty, debug_name)
    }

    pub(crate) fn place(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        nodrop: bool,
    ) -> MIRPlace {
        self.context_mut().place(ty, debug_name, nodrop)
    }

    pub(crate) fn create(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        nodrop: bool,
    ) -> MIRPlace {
        let place = self.place(ty, debug_name, nodrop);
        self.emit(MIRInstrKind::Create { out: place, ty });
        place
    }

    pub(crate) fn bind_local(&mut self, local: THIRLocalID, place: MIRPlace) {
        self.context_mut().bind_local(local, place);
    }

    pub(crate) fn bind_local_value(&mut self, local: THIRLocalID, value: MIRValue) {
        self.context_mut().bind_local_value(local, value);
    }

    pub(crate) fn local(&self, local: THIRLocalID) -> Option<MIRPlace> {
        self.context().local(local)
    }

    pub(crate) fn local_value(&self, local: THIRLocalID) -> Option<MIRValue> {
        self.context().local_value(local)
    }

    pub(crate) fn push_named_scope(&mut self) {
        self.context_mut().push_named_scope();
    }

    pub(crate) fn pop_named_scope(&mut self) {
        self.context_mut().pop_named_scope();
    }

    pub(crate) fn push_lexical_scope(&mut self, token_range: TokenRange) {
        let scope = self.context_mut().push_lexical_scope(token_range);
        self.emit(MIRInstrKind::ScopeEnter { scope });
    }

    pub(crate) fn pop_lexical_scope(&mut self) -> (MIRScopeID, Vec<THIRExpression>) {
        self.context_mut().pop_lexical_scope()
    }

    pub(crate) fn lexical_scope_depth(&self) -> usize {
        self.context().lexical_scope_depth()
    }

    pub(crate) fn register_defer(&mut self, expression: THIRExpression) {
        self.context_mut().register_defer(expression);
    }

    pub(crate) fn lexical_scope_exits_to(
        &self,
        depth: usize,
    ) -> Vec<(MIRScopeID, Vec<THIRExpression>)> {
        self.context().lexical_scope_exits_to(depth)
    }

    pub(crate) fn bind_named(&mut self, name: &CXIdent, value: MIRValue) {
        self.context_mut().bind_named(name, value);
    }

    pub(crate) fn named(&self, name: &CXIdent) -> Option<MIRValue> {
        self.context().named(name)
    }

    pub(crate) fn function_symbol(&mut self, name: &str) -> Option<MIRFunctionID> {
        self.module.function_symbol(name)
    }

    pub(crate) fn global_symbol(&mut self, name: &str) -> Option<MIRGlobalID> {
        self.module.global_symbol(name)
    }

    pub(crate) fn global_id(&self, name: &str) -> Option<MIRGlobalID> {
        self.module.global_id(name)
    }

    pub(crate) fn push_contextual_scope(
        &mut self,
        break_target: MIRBasicBlockID,
        continue_target: Option<MIRBasicBlockID>,
    ) {
        self.context_mut()
            .push_contextual_scope(break_target, continue_target);
    }

    pub(crate) fn pop_loop(&mut self) -> LoopContext {
        self.context_mut().pop_loop()
    }

    pub(crate) fn break_target(&self) -> Option<MIRBasicBlockID> {
        self.context().break_target()
    }

    pub(crate) fn continue_target(&self) -> Option<MIRBasicBlockID> {
        self.context().continue_target()
    }

    pub(crate) fn break_scope_depth(&self) -> Option<usize> {
        self.context().break_scope_depth()
    }

    pub(crate) fn continue_scope_depth(&self) -> Option<usize> {
        self.context().continue_scope_depth()
    }

    pub(crate) fn push_yield(&mut self, target: MIRBasicBlockID, result_type: Option<MIRTypeID>) {
        self.context_mut().push_yield(target, result_type);
    }

    pub(crate) fn yield_target(&self) -> Option<MIRBasicBlockID> {
        self.context().yield_target()
    }

    pub(crate) fn yield_scope_depth(&self) -> Option<usize> {
        self.context().yield_scope_depth()
    }

    pub(crate) fn yield_result(&self) -> Option<MIRRegister> {
        self.context().yield_result()
    }

    pub(crate) fn pop_yield(&mut self) -> YieldContext {
        self.context_mut().pop_yield()
    }

    pub(crate) fn root_defers(&self) -> Vec<THIRExpression> {
        self.context().root_defers()
    }

    fn context(&self) -> &FunctionContext {
        self.function
            .as_ref()
            .expect("no MIR function is currently active")
    }

    fn context_mut(&mut self) -> &mut FunctionContext {
        self.function
            .as_mut()
            .expect("no MIR function is currently active")
    }
}

pub(crate) fn integer_type(ty: &THIRType) -> (MIRIntType, bool) {
    match ty.kind {
        THIRTypeKind::Integer { _type, signed } => (lower_int_type(_type), signed),
        _ => (MIRIntType::I64, true),
    }
}
