use std::collections::{HashMap, HashSet};

use cx_log::{CXResult, catalogue::mir as catalogue};
use cx_mir::{
    MIRFnPrototype, MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalVariable, MIRPlaceID,
    MIRType, MIRTypeID, MIRTypeKind, MIRUnit, MIRValue,
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

use crate::{builder::body::MIRBodyKind};
use function::MIRFunctionBuilder;
use module::{MIRModuleBuilder, ModuleParts};

pub struct MIRBuilder<'thir> {
    types: MIRTypeRegistryBuilder,
    module: MIRModuleBuilder,
    registry: &'thir THIRDecomposedRegistry,
    function: Option<MIRFunctionBuilder>,
}

#[derive(Debug, Clone)]
pub struct MIRTypeRegistryBuilder {
    architecture: ArchitectureConfig,
    definitions: Vec<Option<MIRType>>,

    lowering_types: HashSet<THIRTypeID>,
    interner: HashMap<MIRType, MIRTypeID>,
    debug_names: HashMap<MIRTypeID, String>,
    next_id: usize,
}

impl<'thir> MIRBuilder<'thir> {
    pub fn new(thir: &'thir THIRUnit) -> Self {
        let mut builder = Self {
            types: MIRTypeRegistryBuilder::new(*thir.registry.architecture()),
            module: MIRModuleBuilder::new(),
            registry: &thir.registry,
            function: None,
        };

        builder
            .types
            .reserve_id_space(thir.registry.type_id_bound());

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

    pub(crate) fn try_fun(&self) -> Option<&MIRFunctionBuilder> {
        self.function.as_ref()
    }

    pub(crate) fn fun(&self) -> &MIRFunctionBuilder {
        self.function
            .as_ref()
            .expect("no MIR function is currently active")
    }

    #[allow(dead_code)]
    pub(crate) fn try_fun_mut(&mut self) -> Option<&mut MIRFunctionBuilder> {
        self.function.as_mut()
    }

    pub(crate) fn fun_mut(&mut self) -> &mut MIRFunctionBuilder {
        self.function
            .as_mut()
            .expect("no MIR function is currently active")
    }

    pub(crate) fn is_capturing(&self) -> bool {
        self.try_fun()
            .is_some_and(|function| function.capture.is_some())
    }

    pub(crate) fn set_source_range(&mut self, range: TokenRange) -> TokenRange {
        self.fun_mut().set_source_range(range)
    }

    pub(crate) fn restore_source_range(&mut self, range: TokenRange) {
        self.fun_mut().restore_source_range(range);
    }

    pub(crate) fn source_range(&self) -> &TokenRange {
        self.fun().source_range()
    }

    pub fn create(
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

    pub(crate) fn take_current_function(&mut self) -> Option<MIRFunctionBuilder> {
        self.function.take()
    }

    pub(crate) fn restore_current_function(&mut self, function: MIRFunctionBuilder) {
        self.function = Some(function);
    }

    pub fn finish(self) -> MIRUnit<'thir> {
        let parts: ModuleParts = self.module.into_parts();

        let functions = parts
            .functions
            .into_iter()
            .filter(|(id, _)| parts.used_functions.contains(id))
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

    pub(crate) fn start_custom_function(
        &mut self,
        function: MIRFunction,
        parent: Option<&MIRFunctionBuilder>,
    ) {
        self.function = Some(MIRFunctionBuilder::new_runtime(function, parent));
    }

    pub(crate) fn start_function(&mut self, id: MIRFunctionID) {
        let function = self
            .module
            .function(id)
            .cloned()
            .expect("function context must be declared in the module before starting");

        self.function = Some(MIRFunctionBuilder::new_runtime(function, None));
    }

    pub(crate) fn start_comptime_function(&mut self, id: MIRFunctionID) {
        let function = self
            .module
            .function(id)
            .cloned()
            .expect("function context must be declared in the module before starting");

        self.function = Some(MIRFunctionBuilder::new_comptime(function, None));
    }

    pub(crate) fn finish_function(&mut self) -> CXResult<()> {
        let Some(fn_builder) = self.function.take() else {
            unreachable!("No function context available at finish_function");
        };

        let (id, body) = fn_builder.thin_finish();

        match body {
            MIRBodyKind::Runtime(body) => {
                self.module_mut().finish_function(id, body)
            }
            MIRBodyKind::Comptime(body) => {
                self.module_mut().finish_comptime_function(id, body)
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
        self.interner.get(ty).copied()
    }

    fn find_kind(&self, kind: &MIRTypeKind) -> Option<MIRTypeID> {
        self.interner
            .iter()
            .find_map(|(ty, id)| if &ty.kind() == kind { Some(*id) } else { None })
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
            interner: HashMap::new(),
            lowering_types: HashSet::new(),
            debug_names: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn intern(&mut self, definition: MIRType) -> MIRTypeID {
        if let Some(id) = self.interner.get(&definition).copied() {
            return id;
        }

        let id = MIRTypeID::new(self.next_id);
        self.next_id += 1;
        self.ensure_capacity(id.index());
        self.definitions[id.index()] = Some(definition.clone());
        self.interner.insert(definition, id);
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
        self.interner.get(definition).copied()
    }

    pub fn define(&mut self, id: MIRTypeID, definition: MIRType) -> Result<(), MIRLayoutError> {
        self.ensure_capacity(id.index());
        self.next_id = self.next_id.max(id.index() + 1);
        
        let Some(slot) = self.definitions[id.index()].as_mut() else {
            return todo!();
        };
        
        *slot = definition;
        self.interner.entry(definition).or_insert(id);
        Ok(())
    }

    fn ensure_capacity(&mut self, index: usize) {
        if self.definitions.len() <= index {
            let len = index + 1;
            self.definitions.resize_with(len, || None);
        }
    }

    pub fn reference_to(&mut self, id: MIRTypeID) -> Result<MIRTypeID, MIRLayoutError> {
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

impl ComptimeContext for MIRBuilder<'_> {
    type Registry = MIRTypeRegistryBuilder;

    fn resolve(&self, id: MIRFunctionID) -> Option<&MIRFunction> {
        self.module().function(id)
    }

    fn types(&self) -> &MIRTypeRegistryBuilder {
        self.types()
    }

    fn global(&self, id: MIRGlobalID) -> Option<&MIRGlobalVariable> {
        self.module().global(id)
    }

    fn global_initializer(&self, id: MIRGlobalID) -> Option<MIRFunctionID> {
        self.module().global_initializer(id)
    }
}
