pub(crate) mod function;
pub(crate) mod generation;
pub(crate) mod module;
pub(crate) mod resolver;
pub(crate) mod symbols;

use cx_hmir::{
    HMIRDef, HMIRDefKind, HMIRDefRef, HMIRUnit,
    ty::{HMIRType, HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_mir::MIRUnit;
use cx_pipeline_data::db::ModuleData;
use cx_target::ArchitectureConfig;

use crate::env::{
    generation::GenerationQueue, module::MIRModuleBuilder, resolver::HMIRResolver,
    symbols::HMIRSymbolEnv,
};

#[derive(Debug)]
pub struct HMIREnvironment<'global, 'hmir> {
    global: &'global ModuleData,
    architecture: ArchitectureConfig,

    module: MIRModuleBuilder,
    symbols: HMIRSymbolEnv<'global, 'hmir>,
    resolver: HMIRResolver,
    generation: GenerationQueue,
}

impl<'global, 'hmir> HMIREnvironment<'global, 'hmir> {
    pub fn new(
        global: &'global ModuleData,
        unit: &'hmir HMIRUnit,
        architecture: ArchitectureConfig,
    ) -> Self {
        Self {
            global,
            architecture,

            module: MIRModuleBuilder::new(architecture),
            symbols: HMIRSymbolEnv::new(&global.hmir, unit),
            resolver: HMIRResolver::new(),
            generation: GenerationQueue::new(),
        }
    }

    // Equivalent types are interchangeable views of one value, so they may differ in constness but never in the
    // ownership obligations of their move semantics, at any depth
    pub fn same_type(&self, a: HMIRTypeID, b: HMIRTypeID) -> bool {
        if self.semantics(a).move_semantics != self.semantics(b).move_semantics {
            return false;
        }

        let (a, b) = (self.underlying(a), self.underlying(b));
        if a == b {
            return true;
        }

        match (self.resolve_kind(a), self.resolve_kind(b)) {
            (HMIRTypeKind::Void, HMIRTypeKind::Void)
            | (HMIRTypeKind::Unreachable, HMIRTypeKind::Unreachable)
            | (HMIRTypeKind::Type, HMIRTypeKind::Type)
            | (HMIRTypeKind::Str, HMIRTypeKind::Str) => true,

            (HMIRTypeKind::Int(a), HMIRTypeKind::Int(b)) => a == b,
            (HMIRTypeKind::Float(a), HMIRTypeKind::Float(b)) => a == b,

            (HMIRTypeKind::PointerTo(a), HMIRTypeKind::PointerTo(b))
            | (HMIRTypeKind::ReferenceTo(a), HMIRTypeKind::ReferenceTo(b)) => {
                self.same_type(*a, *b)
            }

            (
                HMIRTypeKind::Array {
                    element: a,
                    length: a_length,
                },
                HMIRTypeKind::Array {
                    element: b,
                    length: b_length,
                },
            ) => a_length == b_length && self.same_type(*a, *b),

            _ => false,
        }
    }

    // A reference to a function or global is what requests its generation
    pub fn resolve_def(&mut self, def: &HMIRDefRef) -> &'hmir HMIRDef {
        let HMIRDefRef::Local(id) = def else {
            todo!("reference to an external definition")
        };
        let def = self
            .symbols
            .local_unit()
            .resolve_def(*id)
            .expect("Local symbol not found");

        if matches!(
            def.kind(),
            HMIRDefKind::Function(_) | HMIRDefKind::Global(_)
        ) {
            self.generation.request(*id);
        }

        def
    }

    pub fn module(&self) -> &MIRModuleBuilder {
        &self.module
    }

    pub fn module_mut(&mut self) -> &mut MIRModuleBuilder {
        &mut self.module
    }

    pub fn symbols(&self) -> &HMIRSymbolEnv<'global, 'hmir> {
        &self.symbols
    }

    pub fn symbols_mut(&mut self) -> &mut HMIRSymbolEnv<'global, 'hmir> {
        &mut self.symbols
    }

    pub fn resolver(&self) -> &HMIRResolver {
        &self.resolver
    }

    pub fn resolver_mut(&mut self) -> &mut HMIRResolver {
        &mut self.resolver
    }

    pub fn generation(&self) -> &GenerationQueue {
        &self.generation
    }

    pub fn generation_mut(&mut self) -> &mut GenerationQueue {
        &mut self.generation
    }

    pub fn finish(self) -> MIRUnit {
        self.module.finish()
    }
}

impl HMIRTypeContext for HMIREnvironment<'_, '_> {
    fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    fn resolve_type_id(&self, id: HMIRTypeID) -> &HMIRType {
        match self.resolver.realized_type(id) {
            Some(ty) => ty,
            None => self.symbols.local_unit().resolve_type(id),
        }
    }
}
