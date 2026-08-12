use std::collections::HashMap;

use cx_ast::ast::modifiers::CXLinkageMode;
use cx_mir::{
    MIRBasicBlockID, MIRConstant, MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunctionID,
    MIRGlobalID, MIRGlobalInitializer, MIRInstrKind, MIRParameterID, MIRPlace, MIRRegister,
    MIRType, MIRUnit, MIRValue,
};
use cx_thir::{
    THIRUnit,
    registry::THIRDecomposedRegistry,
    thir::{
        data::{THIRFnPrototype, THIRFunction},
        expression::THIRLocalID,
        global::{MIRGlobalVarKind, MIRGlobalVariable as THIRGlobalVariable},
        r#type::{THIRIntType, THIRType, THIRTypeKind},
    },
    type_context::THIRTypeContext,
};
use cx_util::identifier::CXIdent;

#[derive(Debug, Clone, Copy)]
pub(crate) struct LoopContext {
    pub break_target: MIRBasicBlockID,
    pub continue_target: Option<MIRBasicBlockID>,
}

#[derive(Debug)]
pub(crate) struct YieldContext {
    pub target: MIRBasicBlockID,
    pub result_type: MIRType,
    pub target_scope: Option<usize>,
    pub incoming: Vec<(MIRBasicBlockID, MIRValue)>,
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
    function_symbols: HashMap<String, MIRFunctionID>,
    global_symbols: HashMap<String, MIRGlobalID>,
    definitions: Vec<MIRFunctionID>,
    current: Option<FunctionContext>,
}

impl<'thir> MIRBuilder<'thir> {
    pub fn new(thir: &'thir THIRUnit) -> Self {
        let mut builder = Self {
            unit: MIRUnit::new(),
            registry: &thir.registry,
            function_symbols: HashMap::new(),
            global_symbols: HashMap::new(),
            definitions: Vec::with_capacity(thir.functions.len()),
            current: None,
        };

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

    fn predeclare_function(&mut self, function: &THIRFunction) {
        let name = function.prototype.symbol_name().to_string();
        let id = self
            .unit
            .add_function(Self::convert_prototype(&function.prototype));
        self.function_symbols.entry(name).or_insert(id);
        self.definitions.push(id);
    }

    fn predeclare_global(&mut self, global: &THIRGlobalVariable) {
        let (name, ty, initializer) = match &global.kind {
            MIRGlobalVarKind::StringLiteral { name, value } => (
                name.clone(),
                MIRType::from_kind(THIRTypeKind::Str),
                Some(MIRGlobalInitializer::Bytes(
                    value.as_bytes().to_vec().into_boxed_slice(),
                )),
            ),
            MIRGlobalVarKind::Variable {
                name,
                _type,
                initializer,
            } => {
                let constant = initializer.and_then(|value| match &_type.kind {
                    THIRTypeKind::Integer {
                        _type: integer_type,
                        signed,
                    } => Some(MIRGlobalInitializer::Scalar(MIRConstant::Integer {
                        value: value as i128,
                        ty: *integer_type,
                        signed: *signed,
                    })),
                    _ => None,
                });
                (name.clone(), MIRType::new(_type.clone()), constant)
            }
        };

        let id = self
            .unit
            .add_global(name.clone(), ty, global.linkage, global.is_mutable);
        let lowered = self
            .unit
            .global_mut(id)
            .expect("a just-created global must exist");
        lowered.initializer = initializer;
        lowered.is_definition = true;
        self.global_symbols.entry(name.as_string()).or_insert(id);
    }

    pub(crate) fn convert_prototype(prototype: &THIRFnPrototype) -> MIRFnPrototype {
        Self::prototype_from_signature(
            CXIdent::new(prototype.symbol_name()),
            prototype.signature(),
            prototype.linkage(),
        )
    }

    fn prototype_from_signature(
        name: CXIdent,
        signature: &cx_thir::thir::data::THIRFnSignature,
        linkage: CXLinkageMode,
    ) -> MIRFnPrototype {
        let params = signature
            .params
            .iter()
            .map(|param| match &param.name {
                Some(name) => MIRFnParam::named(name.clone(), MIRType::new(param._type.clone())),
                None => MIRFnParam::new(MIRType::new(param._type.clone())),
            })
            .collect();
        let return_type = (!matches!(signature.return_type.kind, THIRTypeKind::Unit))
            .then(|| MIRType::new(signature.return_type.clone()));
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

    /// Emits an instruction if the active block is open. Returns whether the
    /// instruction was appended, which lets CFG lowering record real edges.
    pub(crate) fn emit(&mut self, instruction: MIRInstrKind) -> bool {
        if self.current_block_terminated() {
            return false;
        }
        let block = self.current_block();
        self.function_mut()
            .push_instr(block, instruction)
            .expect("active MIR block is missing");
        true
    }

    pub(crate) fn new_register(&mut self, ty: MIRType, debug_name: Option<CXIdent>) -> MIRRegister {
        self.function_mut().add_register(ty, debug_name)
    }

    pub(crate) fn declare_place(&mut self, ty: MIRType, debug_name: Option<CXIdent>) -> MIRPlace {
        self.function_mut().add_place(ty, debug_name)
    }

    pub(crate) fn create_place(&mut self, ty: MIRType, debug_name: Option<CXIdent>) -> MIRPlace {
        let place = self.declare_place(ty.clone(), debug_name);
        self.emit(MIRInstrKind::Create { out: place, ty });
        place
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
            Self::prototype_from_signature(name.clone(), &signature, CXLinkageMode::Extern);
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

        let id = self.unit.add_global(
            name.clone(),
            MIRType::new(ty.clone()),
            CXLinkageMode::Extern,
            true,
        );
        self.unit
            .global_mut(id)
            .expect("a just-created global must exist")
            .is_definition = false;
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

    pub(crate) fn push_yield(&mut self, target: MIRBasicBlockID, result_type: MIRType) {
        self.context_mut().yields.push(YieldContext {
            target,
            result_type,
            target_scope: None,
            incoming: Vec::new(),
        });
    }

    pub(crate) fn yield_target(&self) -> Option<MIRBasicBlockID> {
        self.context().yields.last().map(|context| context.target)
    }

    pub(crate) fn record_yield(&mut self, target_scope: usize, value: MIRValue) {
        let predecessor = self.current_block();
        let context = self
            .context_mut()
            .yields
            .last_mut()
            .expect("yield lowered outside an active yield context");
        match context.target_scope {
            Some(existing) => debug_assert_eq!(existing, target_scope),
            None => context.target_scope = Some(target_scope),
        }
        context.incoming.push((predecessor, value));
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

pub(crate) fn integer_type(ty: &THIRType) -> (THIRIntType, bool) {
    match ty.kind {
        THIRTypeKind::Integer { _type, signed } => (_type, signed),
        _ => (THIRIntType::I64, true),
    }
}
