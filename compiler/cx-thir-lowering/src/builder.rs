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

use crate::lowering::types::{lower_type, lower_type_id};
use function::{FunctionContext};
use module::MIRModuleState;

pub struct MIRBuilder<'thir> {
    types: MIRTypeRegistryBuilder,
    module: MIRModuleState,
    registry: &'thir THIRDecomposedRegistry,

    lowering_types: HashSet<THIRTypeID>,
    function: Option<FunctionContext>,

    next_anonymous_symbol: usize,
    next_comptime_function: usize,
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

    pub fn module(&self) -> &MIRModuleState {
        &self.module
    }
    
    pub fn module_mut(&self) -> &MIRModuleState {
        &mut self.module
    }

    pub fn current_fn(&self) -> &FunctionContext {
        self.function
            .as_ref()
            .expect("no MIR function is currently active")
    }

    pub fn current_fn_mut(&mut self) -> &mut FunctionContext {
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

    fn prototype_from_signature(
        &mut self,
        name: CXIdent,
        signature: &cx_thir::thir::data::THIRFnSignature,
        linkage: LinkageMode,
    ) -> cx_log::CXResult<MIRFnPrototype> {
        let params = signature
            .params
            .iter()
            .map(|param| {
                let nodrop = param._type.is_nodrop();
                let ty = lower_type(self, &param._type)?;
                Ok(match &param.name {
                    Some(name) => MIRFnParam::named(name.clone(), ty),
                    None => MIRFnParam::new(ty),
                }
                .with_nodrop(nodrop))
            })
            .collect::<cx_log::CXResult<Vec<_>>>()?;
        let return_type =
            if signature.return_type.is_void() || signature.return_type.is_unreachable() {
                self.types().unit()
            } else {
                lower_type(self, &signature.return_type)?
            };
        let mut lowered = MIRFnSignature::new(name, params, return_type);
        lowered.variadic = signature.var_args;
        lowered.safe = signature.contract.safe;
        Ok(MIRFnPrototype::new(lowered, linkage))
    }

    pub(crate) fn start_function(
        &mut self,
        index: usize,
        function: &THIRFunction,
        body: &THIRExpression,
    ) {
        let Some(function) = self.module.function(
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
            true,
        )
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
    
    pub(crate) fn named(&self, name: &CXIdent) -> Option<MIRValue> {
        self.current_fn().named(name)
    }
}

impl MIRContext for MIRBuilder<'_> {
    fn current_function(&self) -> &MIRFunction {
        todo!()
    }
}
