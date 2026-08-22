use std::collections::HashSet;

use cx_mir::{
    MIRFnPrototype, MIRFunction, MIRFunctionID,
    MIRTypeRegistryBuilder, MIRUnit,
};
use cx_mir_comptime::context::MIRContext;
use cx_thir::{
    THIRUnit,
    registry::THIRDecomposedRegistry,
    thir::{
        data::THIRFnPrototype,
        r#type::THIRTypeID,
    },
    type_context::THIRTypeContext,
};

mod function;
mod module;

use crate::lowering::types::lower_type_id;
use function::FunctionBuilder;
use module::MIRModuleBuilder;

pub struct MIRBuilder<'thir> {
    types: MIRTypeRegistryBuilder,
    module: MIRModuleBuilder,
    registry: &'thir THIRDecomposedRegistry,

    lowering_types: HashSet<THIRTypeID>,
    function: Option<FunctionBuilder<'thir>>,

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

    pub fn module_mut(&mut self) -> &mut MIRModuleBuilder {
        &mut self.module
    }

    pub fn current_fn(&self) -> &'thir FunctionBuilder {
        self.function
            .as_ref()
            .expect("no MIR function is currently active")
    }

    pub fn current_fn_mut(&mut self) -> &'thir mut FunctionBuilder {
        self.function
            .as_mut()
            .expect("no MIR function is currently active")
    }

    pub fn finish(self) -> MIRUnit {
        todo!()
    }

    pub(crate) fn convert_prototype(
        &mut self,
        _prototype: &THIRFnPrototype,
    ) -> cx_log::CXResult<MIRFnPrototype> {
        todo!();
    }

    pub(crate) fn start_new_function(&mut self, proto: MIRFnPrototype) -> MIRFunctionID {
        // assert!(
        //     self.module()
        //         .function_symbol(proto.signature.symbol_name.as_str())
        //         .is_none(),
        //     "Function {} is already declared in the module",
        //     proto.signature.symbol_name.as_str()
        // );

        let id = self.module_mut()
            .declare_function(proto);
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
        self.module
            .define_function(id, body);
    }
}

impl MIRContext for MIRBuilder<'_> {
    fn current_function(&self) -> &MIRFunction {
        todo!()
    }
}
