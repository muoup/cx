pub(crate) mod facts;
pub(crate) mod function;
pub(crate) mod generation;
pub(crate) mod module;
pub(crate) mod resolver;
pub mod settings;
pub mod symbols;

use cx_hmir::{
    HMIRUnit,
    ty::{HMIRType, HMIRTypeID, HMIRTypeKind, HMIRUndefined, context::HMIRTypeContext},
};
use cx_mir::MIRUnit;
use cx_pipeline_data::db::ModuleData;
use cx_target::ArchitectureConfig;

use crate::env::{
    facts::TypeRecorder, generation::GenerationQueue, module::MIRModuleBuilder,
    resolver::HMIRResolver, settings::HMIRSettings, symbols::HMIRSymbolEnv,
};

#[derive(Debug)]
pub struct HMIREnvironment<'global, 'hmir> {
    settings: HMIRSettings,
    module: MIRModuleBuilder,
    symbols: HMIRSymbolEnv<'global, 'hmir>,
    resolver: HMIRResolver,
    generation: GenerationQueue,
    recorder: Option<TypeRecorder>,
}

impl<'global, 'hmir> HMIREnvironment<'global, 'hmir> {
    pub fn new(
        global: &'global ModuleData,
        unit: &'hmir HMIRUnit,
        settings: HMIRSettings,
    ) -> Self {
        Self {
            settings,

            module: MIRModuleBuilder::new(*settings.architecture()),
            symbols: HMIRSymbolEnv::new(&global.hmir, unit),
            resolver: HMIRResolver::new(),
            generation: GenerationQueue::new(),
            recorder: settings.record_types().then(TypeRecorder::default),
        }
    }

    pub fn settings(&self) -> &HMIRSettings {
        &self.settings
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
            (HMIRTypeKind::PointerTo(a), HMIRTypeKind::PointerTo(b))
            | (HMIRTypeKind::Staged(a), HMIRTypeKind::Staged(b))
            | (
                HMIRTypeKind::Undefined(HMIRUndefined::Array { element: a }),
                HMIRTypeKind::Undefined(HMIRUndefined::Array { element: b }),
            ) => self.same_type(*a, *b),
            (
                HMIRTypeKind::ReferenceTo {
                    inner: a,
                    bitfield: a_bitfield,
                },
                HMIRTypeKind::ReferenceTo {
                    inner: b,
                    bitfield: b_bitfield,
                },
            ) => a_bitfield == b_bitfield && self.same_type(*a, *b),

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

            (HMIRTypeKind::Function { signature: a, .. }, HMIRTypeKind::Function { signature: b, .. }) => {
                a.is_variadic() == b.is_variadic()
                    && a.params().len() == b.params().len()
                    && a.params()
                        .iter()
                        .zip(b.params())
                        .all(|(a, b)| self.same_type(*a.ty(), *b.ty()))
                    && self.same_type(*a.return_type(), *b.return_type())
            }

            _ => false,
        }
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

    #[allow(private_interfaces)]
    pub fn resolver(&self) -> &HMIRResolver {
        &self.resolver
    }

    #[allow(private_interfaces)]
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
        self.settings.architecture()
    }

    fn resolve_type_id(&self, id: HMIRTypeID) -> &HMIRType {
        self.resolver.realized_type(id)
    }
}
