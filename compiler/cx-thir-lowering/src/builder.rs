use std::collections::{HashMap, HashSet};

use cx_log::CXResult;
use cx_mir::{
    MIRComptimeOp, MIRFunctionID, MIRInstruction, MIRPlaceID, MIRType, MIRTypeID, MIRTypeKind,
    MIRUnit, MIRValue,
    ty::{interface::MTRegistry, registry::MIRTypeRegistry},
};
use cx_target::ArchitectureConfig;
use cx_thir::{
    THIRUnit,
    registry::THIRDecomposedRegistry,
    thir::{expression::THIRLocalID, r#type::THIRTypeID},
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;
use cx_util::linkage::LinkageMode;

pub(crate) mod body;
mod function;
mod module;

use crate::builder::body::MIRBodyKind;
pub(crate) use function::DeferredExpression;
use function::MIRFunctionBuilder;
use module::{MIRUnitBuilder, ModuleParts};

pub struct MIRBuilder<'thir> {
    types: MIRTypeRegistryBuilder,
    module: MIRUnitBuilder<'thir>,
    registry: &'thir THIRDecomposedRegistry,
    function: Option<MIRFunctionBuilder<'thir>>,
}

#[derive(Debug, Clone)]
pub struct MIRTypeRegistryBuilder {
    architecture: ArchitectureConfig,
    definitions: Vec<Option<MIRType>>,

    lowering_types: HashSet<THIRTypeID>,
    interner: Vec<(MIRType, MIRTypeID)>,
    debug_names: HashMap<MIRTypeID, String>,
    next_id: usize,
}

impl<'thir> MIRBuilder<'thir> {
    pub fn new(thir: &'thir THIRUnit) -> Self {
        let mut builder = Self {
            types: MIRTypeRegistryBuilder::new(*thir.registry.architecture()),
            module: MIRUnitBuilder::new(),
            registry: &thir.registry,
            function: None,
        };

        builder
            .types
            .reserve_id_space(thir.registry.type_id_bound());

        builder
    }

    pub fn registry(&self) -> &'thir THIRDecomposedRegistry {
        self.registry
    }

    pub(crate) fn types(&self) -> &MIRTypeRegistryBuilder {
        &self.types
    }

    pub(crate) fn types_mut(&mut self) -> &mut MIRTypeRegistryBuilder {
        &mut self.types
    }

    #[allow(dead_code)]
    pub(crate) fn module(&self) -> &MIRUnitBuilder<'thir> {
        &self.module
    }

    pub(crate) fn module_mut(&mut self) -> &mut MIRUnitBuilder<'thir> {
        &mut self.module
    }

    #[allow(dead_code)]
    pub(crate) fn try_fun(&self) -> Option<&MIRFunctionBuilder<'thir>> {
        self.function.as_ref()
    }

    pub(crate) fn fun(&self) -> &MIRFunctionBuilder<'thir> {
        self.function
            .as_ref()
            .expect("no MIR function is currently active")
    }

    #[allow(dead_code)]
    pub(crate) fn try_fun_mut(&mut self) -> Option<&mut MIRFunctionBuilder<'thir>> {
        self.function.as_mut()
    }

    pub(crate) fn fun_mut(&mut self) -> &mut MIRFunctionBuilder<'thir> {
        self.function
            .as_mut()
            .expect("no MIR function is currently active")
    }

    pub(crate) fn emit(&mut self, instruction: MIRInstruction) {
        self.fun_mut().emit(instruction);
    }

    pub(crate) fn emit_if_open(&mut self, instruction: MIRInstruction) {
        if !self.fun().current_block_terminated() {
            self.emit(instruction);
        }
    }

    #[allow(dead_code)]
    pub(crate) fn emit_comptime(&mut self, op: MIRComptimeOp<'thir>, range: TokenRange) {
        let function = self.fun_mut();
        function.open_unreachable_block();
        function.body_mut().emit_comptime(op, range);
    }

    pub fn new_place(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        nodrop: bool,
    ) -> MIRPlaceID {
        self.fun_mut().new_place(ty, debug_name, nodrop)
    }

    pub(crate) fn local_value(&mut self, local: THIRLocalID) -> Option<MIRValue> {
        self.fun().local(local)
    }

    #[allow(dead_code)]
    pub(crate) fn take_current_function(&mut self) -> Option<MIRFunctionBuilder<'thir>> {
        self.function.take()
    }

    #[allow(dead_code)]
    pub(crate) fn restore_current_function(&mut self, function: MIRFunctionBuilder<'thir>) {
        self.function = Some(function);
    }

    pub fn finish(self) -> MIRUnit<'thir> {
        let parts: ModuleParts<'thir> = self.module.into_parts();

        let functions = parts
            .functions
            .into_iter()
            .filter(|(id, function)| {
                parts.used_functions.contains(id)
                    || (function.body().is_some()
                        && function.prototype().linkage != LinkageMode::Static)
            })
            .collect();

        let comp_functions = parts
            .comptime_functions
            .into_iter()
            .filter(|(id, _)| parts.used_functions.contains(id))
            .collect();

        let globals: HashMap<_, _> = parts
            .globals
            .into_iter()
            .filter(|(id, global)| {
                global.linkage() != LinkageMode::Static || parts.used_globals.contains(id)
            })
            .collect();

        let global_order: Vec<_> = parts
            .global_order
            .into_iter()
            .filter(|id| globals.contains_key(id))
            .collect();

        MIRUnit::new(
            self.types.finish(),
            functions,
            comp_functions,
            parts.staged_expressions,
            globals,
            global_order,
        )
    }

    pub(crate) fn start_function(&mut self, id: MIRFunctionID) {
        let function = self
            .module
            .function(id)
            .cloned()
            .expect("function context must be declared in the module before starting");

        self.function = Some(MIRFunctionBuilder::new_runtime(id, function));
    }

    pub(crate) fn start_comptime_function(
        &mut self,
        id: MIRFunctionID,
        prototype: cx_mir::MIRComptimeFnPrototype,
    ) {
        self.function = Some(MIRFunctionBuilder::new_comptime(id, prototype));
    }

    pub(crate) fn start_comptime_scratch(&mut self, id: MIRFunctionID) {
        self.function = Some(MIRFunctionBuilder::new_comptime_scratch(id));
    }

    pub(crate) fn finish_comptime_scratch(&mut self) -> cx_mir::MIRComptimeBody<'thir> {
        let function = self.function.take().expect("missing comptime scratch body");
        match function.thin_finish().1 {
            MIRBodyKind::ComptimeScratch { body } => body,
            MIRBodyKind::Runtime { .. } | MIRBodyKind::Comptime { .. } => {
                unreachable!("scratch body must not be a function body")
            }
        }
    }

    pub(crate) fn finish_function(&mut self) -> CXResult<()> {
        let Some(fn_builder) = self.function.take() else {
            unreachable!("No function context available at finish_function");
        };

        let (id, body) = fn_builder.thin_finish();

        match body {
            MIRBodyKind::Runtime { body, .. } => {
                self.module_mut().define_function(id, body);
                Ok(())
            }
            MIRBodyKind::Comptime { body, .. } => {
                self.module_mut().define_comptime_function(id, body)
            }
            MIRBodyKind::ComptimeScratch { .. } => {
                unreachable!("scratch body cannot define a function")
            }
        }
    }
}

impl MTRegistry for MIRTypeRegistryBuilder {
    fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    fn definition(&self, id: MIRTypeID) -> Option<&MIRType> {
        self.definitions.get(id.index()).and_then(Option::as_ref)
    }

    fn find(&self, ty: &MIRType) -> Option<MIRTypeID> {
        self.interner
            .iter()
            .find_map(|(candidate, id)| (candidate == ty).then_some(*id))
    }

    fn find_kind(&self, kind: &MIRTypeKind) -> Option<MIRTypeID> {
        self.interner
            .iter()
            .find_map(|(ty, id)| (ty.kind() == kind).then_some(*id))
    }

    fn debug_name(&self, id: MIRTypeID) -> Option<&str> {
        self.debug_names.get(&id).map(|s| s.as_str())
    }
}

impl MIRTypeRegistryBuilder {
    pub fn new(architecture: ArchitectureConfig) -> Self {
        Self {
            architecture,
            definitions: Vec::new(),
            interner: Vec::new(),
            lowering_types: HashSet::new(),
            debug_names: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn intern(&mut self, definition: MIRType) -> MIRTypeID {
        if let Some(id) = self.find(&definition) {
            return id;
        }

        let id = MIRTypeID::new(self.next_id);
        self.next_id += 1;
        self.ensure_capacity(id.index());
        self.definitions[id.index()] = Some(definition.clone());
        self.interner.push((definition, id));
        id
    }

    pub fn set_debug_name(&mut self, id: MIRTypeID, name: String) {
        self.debug_names.insert(id, name);
    }

    pub fn reserve_id_space(&mut self, end: usize) {
        self.next_id = self.next_id.max(end);
        let end = end;

        if self.definitions.len() < end {
            self.definitions.resize_with(end, || None);
        }
    }

    pub fn find(&self, definition: &MIRType) -> Option<MIRTypeID> {
        self.interner
            .iter()
            .find_map(|(candidate, id)| (candidate == definition).then_some(*id))
    }

    pub fn define(&mut self, id: MIRTypeID, definition: MIRType) -> CXResult<()> {
        self.ensure_capacity(id.index());
        self.next_id = self.next_id.max(id.index() + 1);

        self.definitions[id.index()] = Some(definition.clone());
        if self.find(&definition).is_none() {
            self.interner.push((definition, id));
        }
        Ok(())
    }

    fn ensure_capacity(&mut self, index: usize) {
        if self.definitions.len() <= index {
            let len = index + 1;
            self.definitions.resize_with(len, || None);
        }
    }

    pub fn reference_to(&mut self, id: MIRTypeID) -> CXResult<MIRTypeID> {
        let ty = MIRType::new(
            MIRTypeKind::MemoryReference {
                inner: id,
                bitfield: None,
            },
            None,
        );

        Ok(self.intern(ty))
    }

    pub fn insert_lowering_type(&mut self, id: THIRTypeID) {
        self.lowering_types.insert(id);
    }

    pub fn remove_lowering_type(&mut self, id: &THIRTypeID) {
        self.lowering_types.remove(id);
    }

    pub fn is_lowering_type(&self, id: &THIRTypeID) -> bool {
        self.lowering_types.contains(id)
    }

    pub fn finish(self) -> MIRTypeRegistry {
        let definitions = self
            .definitions
            .into_iter()
            .enumerate()
            .filter_map(|(index, ty)| ty.map(|ty| (MIRTypeID::new(index), ty)))
            .collect();

        MIRTypeRegistry::new(self.architecture, definitions, self.debug_names)
    }
}
