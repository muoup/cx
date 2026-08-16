use std::collections::{HashMap, HashSet};

use cx_mir::{
    MIRBasicBlockID, MIRConstant, MIRField, MIRFnParam, MIRFnPrototype, MIRFnSignature,
    MIRFunctionID, MIRGlobalID, MIRGlobalState, MIRInstrKind, MIRIntType, MIRParameterID, MIRPlace,
    MIRRegister, MIRScopeID, MIRTypeDefinition, MIRTypeID, MIRTypeKind, MIRUnit, MIRValue,
};
use cx_thir::{
    THIRUnit,
    registry::THIRDecomposedRegistry,
    thir::{
        data::{THIRFnPrototype, THIRFunction},
        expression::{THIRCoercion, THIRExpression, THIRExpressionKind, THIRLocalID},
        global::{MIRGlobalVarKind, MIRGlobalVariable as THIRGlobalVariable},
        r#type::{THIRFloatType, THIRIntType, THIRType, THIRTypeID, THIRTypeKind},
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

#[derive(Debug, Clone, Copy)]
pub(crate) struct LoopContext {
    pub break_target: MIRBasicBlockID,
    pub continue_target: Option<MIRBasicBlockID>,
    pub lexical_scope_depth: usize,
}

#[derive(Debug)]
pub(crate) struct YieldContext {
    pub target: MIRBasicBlockID,
    pub result: Option<MIRRegister>,
    pub lexical_scope_depth: usize,
}

#[derive(Debug)]
struct FunctionContext {
    function: MIRFunctionID,
    current_block: MIRBasicBlockID,
    local_places: HashMap<THIRLocalID, MIRPlace>,
    local_values: HashMap<THIRLocalID, MIRValue>,
    named_values: Vec<HashMap<String, MIRValue>>,
    loops: Vec<LoopContext>,
    yields: Vec<YieldContext>,
    lexical_scopes: Vec<MIRScopeID>,
    defers: Vec<Vec<THIRExpression>>,
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
            unit: MIRUnit::new(*thir.registry.architecture()),
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
        let debug_name = self.type_debug_name(ty);
        let id = self.unit.types.intern(MIRTypeDefinition {
            kind,
            minimum_alignment: ty.attributes.minimum_alignment,
        });
        if self.unit.types.debug_name(id).is_none()
            && let Some(debug_name) = debug_name
        {
            self.unit.types.set_debug_name(id, debug_name);
        }
        id
    }

    fn lower_type_id(&mut self, id: THIRTypeID) -> MIRTypeID {
        let mir_id = MIRTypeID::new(id.index());
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
        let debug_name = self.type_debug_name(&ty);
        let definition = MIRTypeDefinition {
            kind: self.lower_type_kind_mut(&ty.kind),
            minimum_alignment: ty.attributes.minimum_alignment,
        };
        self.unit
            .types
            .define(mir_id, definition)
            .expect("THIR type ID must have one MIR definition");
        if let Some(debug_name) = debug_name {
            self.unit.types.set_debug_name(mir_id, debug_name);
        }
        self.lowering_types.remove(&id);
        mir_id
    }

    fn type_debug_name(&self, ty: &THIRType) -> Option<String> {
        ty.strong_identifier()
            .map(|_| ty.display_with(self.registry).to_string())
    }

    fn lower_type_kind_mut(&mut self, kind: &THIRTypeKind) -> MIRTypeKind {
        match kind {
            THIRTypeKind::Void => MIRTypeKind::Void,
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
            cx_thir::thir::r#type::THIRField::Standard { name, type_id } => {
                MIRField::named(name.clone(), self.lower_type_id(*type_id))
            }
            cx_thir::thir::r#type::THIRField::Bitfield {
                name,
                integer_type_id,
                width,
            } => MIRField::Bitfield {
                name: name.clone(),
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
                    Some(value) => MIRGlobalState::Initialized(self.lower_global_constant(value)),
                    None if global.linkage == LinkageMode::Extern => MIRGlobalState::External,
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

        let id = self.unit.add_global(
            name.clone(),
            ty,
            global.linkage,
            global.is_mutable,
            nodrop,
            state,
        );
        self.global_symbols.entry(name.as_string()).or_insert(id);
    }

    fn lower_global_constant(&mut self, expression: &THIRExpression) -> MIRConstant {
        match &expression.kind {
            THIRExpressionKind::BoolLiteral(value) => MIRConstant::Bool(*value),
            THIRExpressionKind::IntLiteral(value) => {
                let (ty, signed) = integer_type(&expression._type);
                MIRConstant::Integer {
                    value: *value as i128,
                    ty,
                    signed,
                }
            }
            THIRExpressionKind::FloatLiteral(value) => MIRConstant::Float {
                value: *value,
                ty: match expression._type.kind {
                    THIRTypeKind::Float {
                        _type: THIRFloatType::F32,
                    } => cx_mir::MIRFloatType::F32,
                    THIRTypeKind::Float {
                        _type: THIRFloatType::F64,
                    } => cx_mir::MIRFloatType::F64,
                    _ => cx_mir::MIRFloatType::F64,
                },
            },
            THIRExpressionKind::ArrayInitializer { elements, .. } => MIRConstant::Aggregate {
                ty: self.lower_type(&expression._type),
                fields: elements
                    .iter()
                    .enumerate()
                    .map(|(index, element)| (index, self.lower_global_constant(element)))
                    .collect(),
            },
            THIRExpressionKind::StructInitializer { initializations, .. } => {
                MIRConstant::Aggregate {
                    ty: self.lower_type(&expression._type),
                    fields: initializations
                        .iter()
                        .map(|initialization| {
                            (
                                initialization.field_index,
                                self.lower_global_constant(&initialization.value),
                            )
                        })
                        .collect(),
                }
            }
            THIRExpressionKind::TypeConversion {
                conversion: THIRCoercion::IntToPtr { .. },
                operand,
            } if matches!(operand.kind, THIRExpressionKind::IntLiteral(0)) => {
                MIRConstant::Null {
                    ty: self.lower_type(&expression._type),
                }
            }
            THIRExpressionKind::TypeConversion {
                conversion: THIRCoercion::ReinterpretBits,
                operand,
            } if matches!(operand.kind, THIRExpressionKind::GlobalVariable { .. }) => {
                let THIRExpressionKind::GlobalVariable { symbol } = &operand.kind else {
                    unreachable!()
                };
                self.lower_string_array_constant(&expression._type, symbol)
            }
            _ => panic!("unsupported global initializer: {expression:?}"),
        }
    }

    fn lower_string_array_constant(
        &mut self,
        expression_type: &THIRType,
        symbol: &CXIdent,
    ) -> MIRConstant {
        let ty = self.lower_type(expression_type);
        let length = match self.unit.types.kind(ty) {
            Some(MIRTypeKind::Array { length, .. }) => *length,
            _ => panic!("string literal initializer target is not an array"),
        };
        let global = self
            .global_symbol(symbol.as_str())
            .and_then(|id| self.unit.global(id))
            .unwrap_or_else(|| panic!("string literal global {symbol} is not declared"));
        let MIRGlobalState::Initialized(MIRConstant::String(value)) = &global.state else {
            panic!("global {symbol} is not a string literal");
        };
        let mut bytes = value.bytes().chain(std::iter::once(0));
        let fields = (0..length)
            .filter_map(|index| {
                bytes.next().map(|value| {
                    (
                        index,
                        MIRConstant::Integer {
                            value: value as i128,
                            ty: MIRIntType::I8,
                            signed: false,
                        },
                    )
                })
            })
            .collect();
        MIRConstant::Aggregate { ty, fields }
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
                let ty = self.lower_type(&param._type);
                match &param.name {
                    Some(name) => MIRFnParam::named(name.clone(), ty),
                    None => MIRFnParam::new(ty),
                }
                .with_nodrop(nodrop)
            })
            .collect();
        let return_type = if matches!(signature.return_type.kind, THIRTypeKind::Void) {
            self.unit.types.unit()
        } else {
            self.lower_type(&signature.return_type)
        };
        let mut lowered = MIRFnSignature::new(name, params, return_type);
        lowered.variadic = signature.var_args;
        lowered.safe = signature.contract.safe;
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
        let root_scope = self
            .unit
            .function_mut(function_id)
            .expect("predeclared MIR function is missing")
            .add_scope(function.body.token_range.clone());

        self.current = Some(FunctionContext {
            function: function_id,
            current_block: entry,
            local_places: HashMap::new(),
            local_values: HashMap::new(),
            named_values: vec![HashMap::new()],
            loops: Vec::new(),
            yields: Vec::new(),
            lexical_scopes: vec![root_scope],
            defers: vec![Vec::new()],
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
        assert_eq!(
            context.lexical_scopes.len(),
            1,
            "lexical scope stack is unbalanced"
        );
        assert_eq!(context.defers.len(), 1, "defer stack is unbalanced");

        let unit_type = self.unit.types.unit();
        let returns_value = self
            .unit
            .function(context.function)
            .expect("active MIR function is missing")
            .prototype
            .signature
            .return_type
            != unit_type;
        let function = self
            .unit
            .function_mut(context.function)
            .expect("active MIR function is missing");
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

    pub(crate) fn register(&mut self, ty: MIRTypeID, debug_name: Option<CXIdent>) -> MIRRegister {
        self.function_mut().add_register(ty, debug_name)
    }

    pub(crate) fn register_type(&self, register: MIRRegister) -> Option<MIRTypeID> {
        self.function().register(register).map(|register| register.ty)
    }

    pub(crate) fn block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        self.function_mut()
            .add_block_param(block, ty, debug_name)
            .expect("selected block does not exist")
    }

    pub(crate) fn place(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        nodrop: bool,
    ) -> MIRPlace {
        let scope = self
            .context()
            .lexical_scopes
            .last()
            .copied()
            .expect("active function has no lexical scope");
        self.function_mut().add_place(ty, debug_name, nodrop, scope)
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
        self.context_mut().local_places.insert(local, place);
    }

    pub(crate) fn bind_local_value(&mut self, local: THIRLocalID, value: MIRValue) {
        self.context_mut().local_values.insert(local, value);
    }

    pub(crate) fn local(&self, local: THIRLocalID) -> Option<MIRPlace> {
        self.context().local_places.get(&local).copied()
    }

    pub(crate) fn local_value(&self, local: THIRLocalID) -> Option<MIRValue> {
        self.context().local_values.get(&local).cloned()
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

    pub(crate) fn push_lexical_scope(&mut self, token_range: TokenRange) {
        let scope = self.function_mut().add_scope(token_range);
        let context = self.context_mut();
        context.lexical_scopes.push(scope);
        context.defers.push(Vec::new());
        self.emit(MIRInstrKind::ScopeEnter { scope });
    }

    pub(crate) fn pop_lexical_scope(&mut self) -> (MIRScopeID, Vec<THIRExpression>) {
        let context = self.context_mut();
        assert!(
            context.lexical_scopes.len() > 1,
            "attempted to pop the function's lexical scope"
        );
        assert_eq!(
            context.lexical_scopes.len(),
            context.defers.len(),
            "lexical scope and defer stacks are unbalanced"
        );
        let defers = context
            .defers
            .pop()
            .expect("active lexical scope has a defer list");
        let scope = context
            .lexical_scopes
            .pop()
            .expect("lexical scope stack is non-empty");
        (scope, defers)
    }

    pub(crate) fn lexical_scope_depth(&self) -> usize {
        self.context().lexical_scopes.len()
    }

    pub(crate) fn register_defer(&mut self, expression: THIRExpression) {
        self.context_mut()
            .defers
            .last_mut()
            .expect("active function has no defer scope")
            .push(expression);
    }

    pub(crate) fn lexical_scope_exits_to(
        &self,
        depth: usize,
    ) -> Vec<(MIRScopeID, Vec<THIRExpression>)> {
        assert!(
            depth <= self.lexical_scope_depth(),
            "invalid lexical scope depth"
        );
        let context = self.context();
        context.lexical_scopes[depth..]
            .iter()
            .zip(&context.defers[depth..])
            .rev()
            .map(|(scope, defers)| (*scope, defers.clone()))
            .collect()
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
        debug_name: Option<&CXIdent>,
    ) -> MIRFunctionID {
        if let Some(id) = self.function_symbol(name.as_str()) {
            if let Some(debug_name) = debug_name
                && let Some(function) = self.unit.function_mut(id)
                && function.prototype.signature.debug_name.is_none()
            {
                function.prototype.signature.debug_name = Some(debug_name.clone());
            }
            return id;
        }

        let signature = self
            .registry
            .intern_signature(callable_type)
            .cloned()
            .unwrap_or_default();
        let mut prototype =
            self.prototype_from_signature(name.clone(), &signature, LinkageMode::Extern);
        prototype.signature.debug_name = debug_name.cloned();
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
        let id = self.unit.add_global(
            name.clone(),
            ty_id,
            LinkageMode::Extern,
            true,
            ty.is_nodrop(),
            MIRGlobalState::External,
        );
        self.global_symbols.insert(name.as_string(), id);
        id
    }

    pub(crate) fn push_contextual_scope(
        &mut self,
        break_target: MIRBasicBlockID,
        continue_target: Option<MIRBasicBlockID>,
    ) {
        let lexical_scope_depth = self.lexical_scope_depth();
        self.context_mut().loops.push(LoopContext {
            break_target,
            continue_target,
            lexical_scope_depth,
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

    pub(crate) fn break_scope_depth(&self) -> Option<usize> {
        self.context()
            .loops
            .last()
            .map(|context| context.lexical_scope_depth)
    }

    pub(crate) fn continue_scope_depth(&self) -> Option<usize> {
        self.context()
            .loops
            .iter()
            .rev()
            .find_map(|context| context.continue_target.map(|_| context.lexical_scope_depth))
    }

    pub(crate) fn push_yield(&mut self, target: MIRBasicBlockID, result_type: Option<MIRTypeID>) {
        let result = result_type.map(|ty| self.block_param(target, ty, None));
        let lexical_scope_depth = self.lexical_scope_depth();
        self.context_mut().yields.push(YieldContext {
            target,
            result,
            lexical_scope_depth,
        });
    }

    pub(crate) fn yield_target(&self) -> Option<MIRBasicBlockID> {
        self.context().yields.last().map(|context| context.target)
    }

    pub(crate) fn yield_scope_depth(&self) -> Option<usize> {
        self.context()
            .yields
            .last()
            .map(|context| context.lexical_scope_depth)
    }

    pub(crate) fn yield_result(&self) -> Option<MIRRegister> {
        self.context()
            .yields
            .last()
            .and_then(|context| context.result)
    }

    pub(crate) fn pop_yield(&mut self) -> YieldContext {
        self.context_mut()
            .yields
            .pop()
            .expect("yield context stack is unbalanced")
    }

    pub(crate) fn root_defers(&self) -> Vec<THIRExpression> {
        self.context().defers.first().cloned().unwrap_or_default()
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
