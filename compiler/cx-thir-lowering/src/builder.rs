use std::collections::{HashMap, HashSet};

use cx_ast::ast::modifiers::CXLinkageMode;
use cx_mir::{
    MIRBasicBlockID, MIRConstant, MIRField, MIRFnParam, MIRFnPrototype, MIRFnSignature,
    MIRFunctionID, MIRGlobalID, MIRGlobalState, MIRInstrKind, MIRIntType, MIRParameterID, MIRPlace,
    MIRRegister, MIRTypeDefinition, MIRTypeID, MIRTypeKind, MIRUnit, MIRValue,
};
use cx_thir::{
    registry::THIRDecomposedRegistry,
    thir::{
        data::{THIRFnPrototype, THIRFunction},
        expression::THIRLocalID,
        global::{MIRGlobalVarKind, MIRGlobalVariable as THIRGlobalVariable},
        r#type::{THIRIntType, THIRType, THIRTypeID, THIRTypeKind},
    },
    type_context::THIRTypeContext,
    THIRUnit,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

#[derive(Debug, Clone, Copy)]
pub(crate) struct LoopContext {
    pub break_target: MIRBasicBlockID,
    pub continue_target: Option<MIRBasicBlockID>,
}

#[derive(Debug)]
pub(crate) struct YieldContext {
    pub target: MIRBasicBlockID,
    pub result: MIRRegister,
    pub has_incoming: bool,
}

#[derive(Debug)]
struct FunctionContext {
    function: MIRFunctionID,
    current_block: MIRBasicBlockID,
    local_places: HashMap<THIRLocalID, MIRPlace>,
    named_values: Vec<HashMap<String, MIRValue>>,
    loops: Vec<LoopContext>,
    yields: Vec<YieldContext>,
}

/// Stateful constructor for one semantic MIR unit.
///
/// Functions and globals are predeclared before any body is visited, making
/// symbol resolution deterministic and allowing forward references. Places,
/// registers, and blocks are allocated by the dense append-only allocators on
/// `MIRFunction`.
pub struct MIRBuilder<'thir> {
    unit: MIRUnit,
    registry: &'thir THIRDecomposedRegistry,
    lowering_types: HashSet<THIRTypeID>,
    function_symbols: HashMap<String, MIRFunctionID>,
    global_symbols: HashMap<String, MIRGlobalID>,
    definitions: Vec<MIRFunctionID>,
    current: Option<FunctionContext>,
    source_range: TokenRange,
}

impl<'thir> MIRBuilder<'thir> {
    pub fn new(thir: &'thir THIRUnit) -> Self {
        let mut builder = Self {
            unit: MIRUnit::with_architecture(*thir.registry.architecture()),
            registry: &thir.registry,
            lowering_types: HashSet::new(),
            function_symbols: HashMap::new(),
            global_symbols: HashMap::new(),
            definitions: Vec::with_capacity(thir.functions.len()),
            current: None,
            source_range: TokenRange::internal(),
        };
        builder
            .unit
            .types
            .reserve_id_space(thir.registry.type_id_bound());

        let unit = thir
            .registry
            .intrinsic_type_id("void")
            .expect("THIR registry is missing the intrinsic void type");
        builder.lower_type_id(unit);

        for function in &thir.functions {
            builder.predeclare_function(function);
        }
        for global in &thir.global_variables {
            builder.predeclare_global(global);
        }

        builder
    }

    pub fn registry(&self) -> &THIRDecomposedRegistry {
        self.registry
    }

    pub fn unit(&self) -> &MIRUnit {
        &self.unit
    }

    pub fn finish(self) -> MIRUnit {
        assert!(
            self.current.is_none(),
            "attempted to finish MIR while a function is active"
        );
        self.unit
    }

    pub(crate) fn lower_type(&mut self, ty: &THIRType) -> MIRTypeID {
        if let Some(id) = self.registry.type_id(ty) {
            return self.lower_type_id(id);
        }
        let kind = self.lower_type_kind_mut(&ty.kind);
        self.unit.types.intern(MIRTypeDefinition {
            kind,
            minimum_alignment: ty.attributes.minimum_alignment,
        })
    }

    fn lower_type_id(&mut self, id: THIRTypeID) -> MIRTypeID {
        let mir_id = MIRTypeID::from_raw(id.0);
        if self.unit.types.definition(mir_id).is_some() || self.lowering_types.contains(&id) {
            return mir_id;
        }

        self.lowering_types.insert(id);
        let Some(ty) = self.registry.try_resolve_type_id(id).cloned() else {
            assert!(
                id.0 < self.registry.type_id_bound(),
                "THIR type {id} is outside its registry"
            );
            self.unit
                .types
                .define(mir_id, MIRTypeDefinition::new(MIRTypeKind::Undefined))
                .expect("reserved THIR type ID must have one MIR definition");
            self.lowering_types.remove(&id);
            return mir_id;
        };
        let definition = MIRTypeDefinition {
            kind: self.lower_type_kind_mut(&ty.kind),
            minimum_alignment: ty.attributes.minimum_alignment,
        };
        self.unit
            .types
            .define(mir_id, definition)
            .expect("THIR type ID must have one MIR definition");
        self.lowering_types.remove(&id);
        mir_id
    }

    fn lower_type_kind_mut(&mut self, kind: &THIRTypeKind) -> MIRTypeKind {
        match kind {
            THIRTypeKind::Unit => MIRTypeKind::Unit,
            THIRTypeKind::Integer { _type, signed } => MIRTypeKind::Integer {
                ty: lower_int_type(*_type),
                signed: *signed,
            },
            THIRTypeKind::Float { _type } => MIRTypeKind::Float {
                ty: match _type {
                    cx_thir::thir::r#type::THIRFloatType::F32 => cx_mir::MIRFloatType::F32,
                    cx_thir::thir::r#type::THIRFloatType::F64 => cx_mir::MIRFloatType::F64,
                },
            },
            THIRTypeKind::Structured { fields } => MIRTypeKind::Structured {
                fields: fields.iter().map(|field| self.lower_field(field)).collect(),
            },
            THIRTypeKind::Union { variants } => MIRTypeKind::Union {
                variants: variants
                    .iter()
                    .map(|field| self.lower_field(field))
                    .collect(),
            },
            THIRTypeKind::TaggedUnion { variants } => MIRTypeKind::TaggedUnion {
                variants: variants
                    .iter()
                    .map(|field| self.lower_field(field))
                    .collect(),
            },
            THIRTypeKind::PointerTo { inner_type } => MIRTypeKind::PointerTo {
                inner: self.lower_type_id(*inner_type),
            },
            THIRTypeKind::MemoryReference {
                inner_type,
                bitfield,
            } => MIRTypeKind::MemoryReference {
                inner: self.lower_type_id(*inner_type),
                bitfield: bitfield.as_ref().map(|bitfield| cx_mir::MIRBitfieldAccess {
                    storage_type: self.lower_type_id(bitfield.storage_type),
                    bit_offset: bitfield.bit_offset,
                    bit_width: bitfield.bit_width,
                    signed: bitfield.signed,
                }),
            },
            THIRTypeKind::Array { length, inner_type } => MIRTypeKind::Array {
                length: *length,
                inner: self.lower_type_id(*inner_type),
            },
            THIRTypeKind::Function { signature } => MIRTypeKind::Function {
                signature: cx_mir::MIRFunctionType {
                    params: signature
                        .params
                        .iter()
                        .map(|param| self.lower_type(&param._type))
                        .collect(),
                    return_type: self.lower_type(&signature.return_type),
                    variadic: signature.var_args,
                },
            },
            THIRTypeKind::Opaque { size, alignment } => MIRTypeKind::Opaque {
                size: *size,
                alignment: *alignment,
            },
            THIRTypeKind::Undefined => MIRTypeKind::Undefined,
            THIRTypeKind::Str => MIRTypeKind::Str,
        }
    }

    fn lower_field(&mut self, field: &cx_thir::thir::r#type::THIRField) -> MIRField {
        match field {
            cx_thir::thir::r#type::THIRField::Standard { type_id, .. } => MIRField::Standard {
                type_id: self.lower_type_id(*type_id),
            },
            cx_thir::thir::r#type::THIRField::Bitfield {
                integer_type_id,
                width,
                ..
            } => MIRField::Bitfield {
                integer_type_id: self.lower_type_id(*integer_type_id),
                width: *width,
            },
        }
    }

    fn predeclare_function(&mut self, function: &THIRFunction) {
        let name = function.prototype.symbol_name().to_string();
        let prototype = self.convert_prototype(&function.prototype);
        let id = self.unit.add_function(prototype);
        self.function_symbols.entry(name).or_insert(id);
        self.definitions.push(id);
    }

    fn predeclare_global(&mut self, global: &THIRGlobalVariable) {
        let (name, ty, state, nodrop) = match &global.kind {
            MIRGlobalVarKind::StringLiteral { name, value } => (
                name.clone(),
                self.lower_type(&THIRType::from(THIRTypeKind::Str)),
                MIRGlobalState::Initialized(MIRConstant::String(value.clone())),
                true,
            ),
            MIRGlobalVarKind::Variable {
                name,
                _type,
                initializer,
            } => {
                let state = match initializer {
                    Some(value) => match &_type.kind {
                        THIRTypeKind::Integer {
                            _type: integer_type,
                            signed,
                        } => MIRGlobalState::Initialized(MIRConstant::Integer {
                            value: *value as i128,
                            ty: lower_int_type(*integer_type),
                            signed: *signed,
                        }),
                        _ => MIRGlobalState::ZeroInitialized,
                    },
                    None if global.linkage == CXLinkageMode::Extern => MIRGlobalState::External,
                    None => MIRGlobalState::ZeroInitialized,
                };
                (
                    name.clone(),
                    self.lower_type(_type),
                    state,
                    _type.is_nodrop(),
                )
            }
        };

        let id = self.unit.add_global_with_nodrop_and_state(
            name.clone(),
            ty,
            global.linkage,
            global.is_mutable,
            nodrop,
            state,
        );
        self.global_symbols.entry(name.as_string()).or_insert(id);
    }

    pub(crate) fn convert_prototype(&mut self, prototype: &THIRFnPrototype) -> MIRFnPrototype {
        self.prototype_from_signature(
            CXIdent::new(prototype.symbol_name()),
            prototype.signature(),
            prototype.linkage(),
        )
    }

    fn prototype_from_signature(
        &mut self,
        name: CXIdent,
        signature: &cx_thir::thir::data::THIRFnSignature,
        linkage: CXLinkageMode,
    ) -> MIRFnPrototype {
        let params = signature
            .params
            .iter()
            .map(|param| {
                let nodrop = param._type.is_nodrop();
                let ty = self.lower_type(&param._type);
                match &param.name {
                    Some(name) => MIRFnParam::named(name.clone(), ty),
                    None => MIRFnParam::new(ty),
                }
                .with_nodrop(nodrop)
            })
            .collect();
        let return_type = (!matches!(signature.return_type.kind, THIRTypeKind::Unit))
            .then(|| self.lower_type(&signature.return_type));
        let mut lowered = MIRFnSignature::new(name, params, return_type);
        lowered.variadic = signature.var_args;
        MIRFnPrototype::new(lowered, linkage)
    }

    pub(crate) fn start_function(&mut self, index: usize, function: &THIRFunction) {
        assert!(self.current.is_none(), "a MIR function is already active");
        let function_id = *self
            .definitions
            .get(index)
            .expect("THIR function predeclaration is missing");
        let entry = self
            .unit
            .function_mut(function_id)
            .expect("predeclared MIR function is missing")
            .add_block();

        self.current = Some(FunctionContext {
            function: function_id,
            current_block: entry,
            local_places: HashMap::new(),
            named_values: vec![HashMap::new()],
            loops: Vec::new(),
            yields: Vec::new(),
        });
        self.set_block_name(entry, "entry");

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

    pub(crate) fn finish_function(&mut self) {
        let context = self
            .current
            .take()
            .expect("attempted to finish without an active MIR function");
        assert!(context.loops.is_empty(), "loop context stack is unbalanced");
        assert!(
            context.yields.is_empty(),
            "yield context stack is unbalanced"
        );

        let function = self
            .unit
            .function_mut(context.function)
            .expect("active MIR function is missing");
        let returns_value = function.prototype.signature.return_type.is_some();
        for block in &mut function.blocks {
            if block.terminator().is_some() {
                continue;
            }
            let terminator = if block.id == context.current_block && !returns_value {
                MIRInstrKind::Return { value: None }
            } else {
                MIRInstrKind::Unreachable
            };
            block.push(terminator);
        }
    }

    pub(crate) fn current_function_id(&self) -> MIRFunctionID {
        self.context().function
    }

    pub(crate) fn current_block(&self) -> MIRBasicBlockID {
        self.context().current_block
    }

    pub(crate) fn set_current_block(&mut self, block: MIRBasicBlockID) {
        assert!(
            self.function().block(block).is_some(),
            "selected block does not belong to the active function"
        );
        self.context_mut().current_block = block;
    }

    pub(crate) fn new_block(&mut self, debug_name: &str) -> MIRBasicBlockID {
        let id = self.function_mut().add_block();
        self.set_block_name(id, debug_name);
        id
    }

    fn set_block_name(&mut self, block: MIRBasicBlockID, debug_name: &str) {
        self.function_mut()
            .block_mut(block)
            .expect("selected block does not exist")
            .debug_name = Some(CXIdent::new(debug_name));
    }

    pub(crate) fn block_terminated(&self, block: MIRBasicBlockID) -> bool {
        self.function()
            .block(block)
            .expect("selected block does not exist")
            .terminator()
            .is_some()
    }

    pub(crate) fn current_block_terminated(&self) -> bool {
        self.block_terminated(self.current_block())
    }

    pub(crate) fn set_source_range(&mut self, range: TokenRange) -> TokenRange {
        std::mem::replace(&mut self.source_range, range)
    }

    pub(crate) fn restore_source_range(&mut self, range: TokenRange) {
        self.source_range = range;
    }

    pub(crate) fn emit(&mut self, instruction: MIRInstrKind) -> bool {
        if self.current_block_terminated() {
            return false;
        }
        let block = self.current_block();
        let range = self.source_range.clone();
        self.function_mut()
            .push_instr_at(block, instruction, range)
            .expect("active MIR block is missing");
        true
    }

    pub(crate) fn new_register(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        self.function_mut().add_register(ty, debug_name)
    }

    pub(crate) fn new_register_for_type(
        &mut self,
        ty: &THIRType,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        let type_id = self.lower_type(ty);
        self.new_register(type_id, debug_name)
    }

    pub(crate) fn add_block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        self.function_mut()
            .add_block_param(block, ty, debug_name)
            .expect("selected block does not exist")
    }

    pub(crate) fn add_block_param_for_type(
        &mut self,
        block: MIRBasicBlockID,
        ty: &THIRType,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        let type_id = self.lower_type(ty);
        self.add_block_param(block, type_id, debug_name)
    }

    pub(crate) fn declare_place(&mut self, ty: MIRTypeID, debug_name: Option<CXIdent>) -> MIRPlace {
        self.function_mut().add_place(ty, debug_name)
    }

    pub(crate) fn declare_place_for_type(
        &mut self,
        ty: &THIRType,
        debug_name: Option<CXIdent>,
    ) -> MIRPlace {
        let type_id = self.lower_type(ty);
        self.declare_place(type_id, debug_name)
    }

    pub(crate) fn declare_place_with_nodrop(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        nodrop: bool,
    ) -> MIRPlace {
        self.function_mut()
            .add_place_with_nodrop(ty, debug_name, nodrop)
    }

    pub(crate) fn create_place_with_nodrop(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        nodrop: bool,
    ) -> MIRPlace {
        let place = self.declare_place_with_nodrop(ty, debug_name, nodrop);
        self.emit(MIRInstrKind::Create { out: place, ty });
        place
    }

    pub(crate) fn create_place_for_type(
        &mut self,
        ty: &THIRType,
        debug_name: Option<CXIdent>,
        nodrop: bool,
    ) -> MIRPlace {
        let type_id = self.lower_type(ty);
        self.create_place_with_nodrop(type_id, debug_name, nodrop)
    }

    pub(crate) fn bind_local(&mut self, local: THIRLocalID, place: MIRPlace) {
        self.context_mut().local_places.insert(local, place);
    }

    pub(crate) fn local(&self, local: THIRLocalID) -> Option<MIRPlace> {
        self.context().local_places.get(&local).copied()
    }

    pub(crate) fn push_named_scope(&mut self) {
        self.context_mut().named_values.push(HashMap::new());
    }

    pub(crate) fn pop_named_scope(&mut self) {
        let context = self.context_mut();
        assert!(
            context.named_values.len() > 1,
            "attempted to pop the function's base symbol scope"
        );
        context.named_values.pop();
    }

    pub(crate) fn bind_named(&mut self, name: &CXIdent, value: MIRValue) {
        self.context_mut()
            .named_values
            .last_mut()
            .expect("active function has no symbol scope")
            .insert(name.as_string(), value);
    }

    pub(crate) fn named(&self, name: &CXIdent) -> Option<MIRValue> {
        self.context()
            .named_values
            .iter()
            .rev()
            .find_map(|scope| scope.get(name.as_str()).cloned())
    }

    pub(crate) fn function_symbol(&self, name: &str) -> Option<MIRFunctionID> {
        self.function_symbols.get(name).copied()
    }

    pub(crate) fn ensure_function(
        &mut self,
        name: &CXIdent,
        callable_type: &THIRType,
    ) -> MIRFunctionID {
        if let Some(id) = self.function_symbol(name.as_str()) {
            return id;
        }

        let signature = self
            .registry
            .intern_signature(callable_type)
            .cloned()
            .unwrap_or_default();
        let prototype =
            self.prototype_from_signature(name.clone(), &signature, CXLinkageMode::Extern);
        let id = self.unit.add_function(prototype);
        self.function_symbols.insert(name.as_string(), id);
        id
    }

    pub(crate) fn global_symbol(&self, name: &str) -> Option<MIRGlobalID> {
        self.global_symbols.get(name).copied()
    }

    pub(crate) fn ensure_global(&mut self, name: &CXIdent, ty: &THIRType) -> MIRGlobalID {
        if let Some(id) = self.global_symbol(name.as_str()) {
            return id;
        }

        let ty_id = self.lower_type(ty);
        let id = self.unit.add_global_with_nodrop(
            name.clone(),
            ty_id,
            CXLinkageMode::Extern,
            true,
            ty.is_nodrop(),
        );
        self.unit
            .global_mut(id)
            .expect("a just-created global must exist")
            .state = MIRGlobalState::External;
        self.global_symbols.insert(name.as_string(), id);
        id
    }

    pub(crate) fn push_loop(
        &mut self,
        break_target: MIRBasicBlockID,
        continue_target: Option<MIRBasicBlockID>,
    ) {
        self.context_mut().loops.push(LoopContext {
            break_target,
            continue_target,
        });
    }

    pub(crate) fn pop_loop(&mut self) -> LoopContext {
        self.context_mut()
            .loops
            .pop()
            .expect("loop context stack is unbalanced")
    }

    pub(crate) fn break_target(&self) -> Option<MIRBasicBlockID> {
        self.context()
            .loops
            .last()
            .map(|context| context.break_target)
    }

    pub(crate) fn continue_target(&self) -> Option<MIRBasicBlockID> {
        self.context()
            .loops
            .iter()
            .rev()
            .find_map(|context| context.continue_target)
    }

    pub(crate) fn push_yield(&mut self, target: MIRBasicBlockID, result_type: MIRTypeID) {
        let result = self.add_block_param(target, result_type, None);
        self.context_mut().yields.push(YieldContext {
            target,
            result,
            has_incoming: false,
        });
    }

    pub(crate) fn push_yield_for_type(&mut self, target: MIRBasicBlockID, result_type: &THIRType) {
        let type_id = self.lower_type(result_type);
        self.push_yield(target, type_id);
    }

    pub(crate) fn yield_target(&self) -> Option<MIRBasicBlockID> {
        self.context().yields.last().map(|context| context.target)
    }

    pub(crate) fn record_yield(&mut self) {
        self.context_mut()
            .yields
            .last_mut()
            .expect("yield lowered outside an active yield context")
            .has_incoming = true;
    }

    pub(crate) fn pop_yield(&mut self) -> YieldContext {
        self.context_mut()
            .yields
            .pop()
            .expect("yield context stack is unbalanced")
    }

    fn context(&self) -> &FunctionContext {
        self.current
            .as_ref()
            .expect("no MIR function is currently active")
    }

    fn context_mut(&mut self) -> &mut FunctionContext {
        self.current
            .as_mut()
            .expect("no MIR function is currently active")
    }

    fn function(&self) -> &cx_mir::MIRFunction {
        self.unit
            .function(self.current_function_id())
            .expect("active MIR function is missing")
    }

    fn function_mut(&mut self) -> &mut cx_mir::MIRFunction {
        let id = self.current_function_id();
        self.unit
            .function_mut(id)
            .expect("active MIR function is missing")
    }
}

fn lower_int_type(ty: THIRIntType) -> MIRIntType {
    match ty {
        THIRIntType::I1 => MIRIntType::I1,
        THIRIntType::I8 => MIRIntType::I8,
        THIRIntType::I16 => MIRIntType::I16,
        THIRIntType::I32 => MIRIntType::I32,
        THIRIntType::I64 => MIRIntType::I64,
        THIRIntType::I128 => MIRIntType::I128,
    }
}

pub(crate) fn integer_type(ty: &THIRType) -> (MIRIntType, bool) {
    match ty.kind {
        THIRTypeKind::Integer { _type, signed } => (lower_int_type(_type), signed),
        _ => (MIRIntType::I64, true),
    }
}
