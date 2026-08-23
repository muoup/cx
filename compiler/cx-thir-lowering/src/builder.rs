use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use cx_log::CXResult;
use cx_mir::{
    MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunction, MIRFunctionID, MIRFunctionMode,
    MIRInstrKind, MIRPlace, MIRRegister, MIRStagedTemplate, MIRTypeID, MIRTypeRegistryBuilder,
    MIRUnit, MIRValue,
};
use cx_mir_comptime::context::MIRContext;
use cx_thir::{
    THIRUnit,
    registry::THIRDecomposedRegistry,
    thir::{
        data::{THIRComptimeFnPrototype, THIRFnPrototype},
        expression::THIRLocalID,
        r#type::THIRTypeID,
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;
use cx_util::linkage::LinkageMode;

mod function;
mod module;

use crate::lowering::{
    self,
    types::{lower_type, lower_type_id},
};
use function::FunctionBuilder;
use module::{MIRModuleBuilder, ModuleParts};

pub struct MIRBuilder<'thir> {
    types: MIRTypeRegistryBuilder,
    module: MIRModuleBuilder,
    registry: &'thir THIRDecomposedRegistry,

    pub(crate) lowering_types: HashSet<THIRTypeID>,
    function: Option<FunctionBuilder>,
    ambient_prototype: MIRFnPrototype,
    source_range: TokenRange,
    capture: Option<CaptureContext>,
}

struct CaptureContext {
    source_locals: HashMap<THIRLocalID, MIRValue>,
    captures: Vec<(MIRRegister, MIRValue)>,
    params: Vec<MIRRegister>,
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
            capture: None,
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

    #[allow(dead_code)]
    pub(crate) fn module(&self) -> &MIRModuleBuilder {
        &self.module
    }

    pub(crate) fn module_mut(&mut self) -> &mut MIRModuleBuilder {
        &mut self.module
    }

    pub(crate) fn resolve_function(
        &mut self,
        name: &str,
    ) -> Option<(MIRFunctionID, MIRFnPrototype)> {
        let id = self.module_mut().function_symbol(name)?;
        let prototype = self.module().function(id)?.prototype().clone();
        Some((id, prototype))
    }

    pub(crate) fn fun(&self) -> &FunctionBuilder {
        self.function
            .as_ref()
            .expect("no MIR function is currently active")
    }

    pub(crate) fn fun_mut(&mut self) -> &mut FunctionBuilder {
        self.function
            .as_mut()
            .expect("no MIR function is currently active")
    }

    pub(crate) fn is_capturing(&self) -> bool {
        self.capture.is_some()
    }

    pub(crate) fn set_source_range(&mut self, range: TokenRange) -> TokenRange {
        std::mem::replace(&mut self.source_range, range)
    }

    pub(crate) fn restore_source_range(&mut self, range: TokenRange) {
        self.source_range = range;
    }

    pub fn emit(&mut self, instr: MIRInstrKind) {
        let range = self.source_range.clone();

        self.fun_mut().emit(instr, range);
    }

    pub fn create(&mut self, ty: MIRTypeID, debug_name: Option<CXIdent>, nodrop: bool) -> MIRPlace {
        let place = self.fun_mut().new_place(ty, debug_name, nodrop);
        self.emit(MIRInstrKind::Create { out: place, ty });
        place
    }

    pub(crate) fn local_value(
        &mut self,
        local: THIRLocalID,
        ty: &cx_thir::thir::r#type::THIRType,
    ) -> CXResult<Option<MIRValue>> {
        if let Some(value) = self.fun().local(local) {
            return Ok(Some(value));
        }

        let Some(capture) = self.capture.as_ref() else {
            return Ok(None);
        };
        let source = capture.source_locals.get(&local).cloned();
        if source.is_none() {
            return Ok(None);
        }

        let ty = lower_type(self, ty)?;
        let input = self.fun_mut().new_register(ty, None);
        let value = MIRValue::Register(input);
        self.fun_mut().bind_local(local, value.clone());
        let capture = self.capture.as_mut().expect("capture context is active");
        capture
            .captures
            .push((input, source.expect("captured local has a source value")));
        Ok(Some(value))
    }

    pub(crate) fn capture_staged(
        &mut self,
        expression: &cx_thir::thir::expression::THIRExpression,
        params: &[(THIRLocalID, &cx_thir::thir::r#type::THIRType)],
        diverges: Option<bool>,
    ) -> CXResult<(Arc<MIRStagedTemplate>, Vec<MIRValue>)> {
        let id = self.module_mut().allocate_function_id();
        let prototype = self.fun().prototype().clone();
        let source_locals = self.fun().locals();
        let saved_function = self.function.take();
        let saved_capture = self.capture.take();
        let saved_range = std::mem::replace(&mut self.source_range, TokenRange::internal());

        self.function = Some(FunctionBuilder::new(MIRFunction::new(id, prototype, None)));
        self.capture = Some(CaptureContext {
            source_locals,
            captures: Vec::new(),
            params: Vec::new(),
        });

        for (local, ty) in params {
            let ty = lower_type(self, ty)?;
            let input = self.fun_mut().new_register(ty, None);
            self.fun_mut().bind_local(*local, MIRValue::Register(input));
            self.capture
                .as_mut()
                .expect("capture context is active")
                .params
                .push(input);
        }

        let lowered = (|| -> CXResult<MIRTypeID> {
            let value = lowering::lower_expression(self, expression)?;
            let result_type = lower_type(self, &expression._type)?;
            if !self.fun().current_block_terminated() {
                self.emit(MIRInstrKind::StagedReturn { value });
            }
            Ok(result_type)
        })();

        let scratch = self.function.take();
        let capture = self.capture.take().expect("capture context is present");
        self.function = saved_function;
        self.capture = saved_capture;
        self.restore_source_range(saved_range);

        let result_type = lowered?;
        let (_, body) = scratch
            .expect("capture builder is present")
            .concise_finish();
        let (inputs, values): (Vec<_>, Vec<_>) = capture.captures.into_iter().unzip();
        Ok((
            Arc::new(MIRStagedTemplate::new(
                body,
                inputs,
                capture.params,
                result_type,
                diverges.unwrap_or_else(|| expression._type.is_unreachable()),
            )),
            values,
        ))
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
            let staged_params = if parameter.value_type.expr {
                Some(
                    parameter
                        .value_type
                        .params
                        .iter()
                        .map(|ty| lower_type(self, ty))
                        .collect::<CXResult<Vec<_>>>()?,
                )
            } else {
                None
            };
            let param = match parameter.name.clone() {
                Some(name) => MIRFnParam::named(name, ty),
                None => MIRFnParam::new(ty),
            }
            .with_staged(
                staged_params,
                parameter.value_type.expr && parameter.value_type._type.is_unreachable(),
            );
            params.push(param);
        }

        let return_type = lower_type(self, &prototype.return_type()._type)?;
        let return_staged_params = if prototype.return_type().expr {
            Some(
                prototype
                    .return_type()
                    .params
                    .iter()
                    .map(|ty| lower_type(self, ty))
                    .collect::<CXResult<Vec<_>>>()?,
            )
        } else {
            None
        };

        Ok(MIRFnPrototype::new(
            MIRFnSignature::new(
                CXIdent::from(prototype.symbol_name().to_string()),
                prototype.debug_name().cloned(),
                params,
                return_type,
                MIRFunctionMode::Comptime,
                false,
                true,
            )
            .with_staged_return(return_staged_params),
            LinkageMode::Static,
        ))
    }

    #[allow(dead_code)]
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
        let saved_range = std::mem::replace(&mut self.source_range, TokenRange::internal());

        self.function = Some(FunctionBuilder::new(MIRFunction::new(
            id,
            prototype.clone(),
            None,
        )));

        let result = (|| -> cx_log::CXResult<()> {
            let value = lowering::lower_expression(self, expression)?;
            if !self.fun_mut().current_block_terminated() {
                let frame = self.fun_mut();
                frame.emit(
                    MIRInstrKind::Return { value: Some(value) },
                    TokenRange::internal(),
                );
            }
            Ok(())
        })();

        let scratch = self.function.take();
        self.function = saved_function;
        self.restore_source_range(saved_range);

        result?;

        let (id, body) = scratch
            .expect("capture builder is present")
            .concise_finish();
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
