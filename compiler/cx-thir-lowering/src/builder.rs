use std::collections::HashSet;

use cx_log::CXResult;
use cx_mir::global::MIRGlobalKind;
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{
    MIRBasicBlockID, MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunction, MIRFunctionID,
    MIRGlobalID, MIRGlobalState, MIRInstrKind, MIRParameterID, MIRPlace, MIRRegister, MIRScopeID,
    MIRTypeID, MIRTypeRegistryBuilder, MIRUnit, MIRValue,
};
use cx_mir_comptime::context::MIRContext;
use cx_thir::thir::global::THIRGlobalVariable;
use cx_thir::{
    THIRUnit,
    registry::THIRDecomposedRegistry,
    thir::{
        data::{THIRFnPrototype, THIRFunction},
        expression::{THIRExpression, THIRLocalID},
        r#type::THIRTypeID,
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

mod function;
mod module;
pub(crate) mod body;

use crate::lowering::types::{lower_type, lower_type_id};
use function::FunctionBuilder;
use module::MIRModuleBuilder;

pub struct MIRBuilder<'thir> {
    types: MIRTypeRegistryBuilder,
    module: MIRModuleBuilder,
    registry: &'thir THIRDecomposedRegistry,

    lowering_types: HashSet<THIRTypeID>,
    function: Option<FunctionBuilder>,

    next_anonymous_symbol: usize,
    next_comptime_function: usize,
}

impl<'thir> MIRBuilder<'thir> {
    pub fn new(thir: &'thir THIRUnit) -> Self {
        let mut builder = Self {
            types: MIRTypeRegistryBuilder::new(*thir.registry.architecture()),
            module: MIRModuleBuilder::new(),
            registry: &thir.registry,
            lowering_types: HashSet::new(),
            function: None,

            next_anonymous_symbol: 0,
            next_comptime_function: 0,
        };
        builder
            .types
            .reserve_id_space(thir.registry.type_id_bound());

        let unit = thir
            .registry
            .intrinsic_type_id("void")
            .expect("THIR registry is missing the intrinsic void type");

        if lower_type_id(&mut builder, unit).is_err() {
            unreachable!("intrinsic void type must lower");
        }
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

    pub fn module(&self) -> &MIRModuleBuilder {
        &self.module
    }

    pub fn module_mut(&self) -> &MIRModuleBuilder {
        &mut self.module
    }

    pub fn current_fn(&self) -> &FunctionBuilder {
        self.function
            .as_ref()
            .expect("no MIR function is currently active")
    }

    pub fn current_fn_mut(&mut self) -> &mut FunctionBuilder {
        self.function
            .as_mut()
            .expect("no MIR function is currently active")
    }

    pub fn finish(self) -> MIRUnit {
        todo!()
    }

    pub(crate) fn convert_prototype(
        &mut self,
        prototype: &THIRFnPrototype,
    ) -> cx_log::CXResult<MIRFnPrototype> {
        let mut lowered = self.prototype_from_signature(
            CXIdent::new(prototype.symbol_name()),
            prototype.signature(),
            prototype.linkage(),
        )?;
        lowered.signature.debug_name = prototype.debug_name().cloned();
        Ok(lowered)
    }

    pub(crate) fn start_new_function(&mut self, proto: MIRFnPrototype) -> MIRFunctionID {
        assert!(
            self.module()
                .function_symbol(proto.signature.symbol_name.as_str())
                .is_none(),
            "Function {} is already declared in the module",
            proto.signature.symbol_name.as_str()
        );

        let id = self.module_mut().declare_function(proto);
        start_function(id);
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
        let Some(context) = self.function.take() else {
            unreachable!("No function context available at finish_function");
        };

        self.module
            .define_function(context.id(), context.definition());
    }

    pub(crate) fn emit(&mut self, instruction: MIRInstrKind) -> bool {
        let range = self.source_range.clone();
        self.current_fn_mut().emit(instruction, range)
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
}

impl MIRContext for MIRBuilder<'_> {
    fn current_function(&self) -> &MIRFunction {
        todo!()
    }
}
