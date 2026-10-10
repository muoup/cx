pub(crate) mod function;
pub(crate) mod lowering;
pub(crate) mod module;
pub(crate) mod resolver;
pub(crate) mod symbols;

use cx_hmir::{
    HMIRBody, HMIRUnit,
    expr::HMIRExprID,
    ty::{HMIRType, HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::CXResult;
use cx_mir::{MIRTypeID, MIRUnit};
use cx_pipeline_data::db::ModuleData;
use cx_target::ArchitectureConfig;
use cx_tokens::TokenRange;

use crate::{
    env::{module::MIRModuleBuilder, resolver::HMIRResolver, symbols::HMIRSymbolEnv},
    lower::types,
};

#[derive(Debug)]
pub struct HMIREnvironment<'global, 'hmir> {
    global: &'global ModuleData,
    architecture: ArchitectureConfig,

    module: MIRModuleBuilder,
    symbols: HMIRSymbolEnv<'global, 'hmir>,
    resolver: HMIRResolver,
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
        }
    }

    pub fn same_type(&self, a: HMIRTypeID, b: HMIRTypeID) -> bool {
        if a == b {
            return true;
        }

        match (
            self.resolve_type_id(a).kind(),
            self.resolve_type_id(b).kind(),
        ) {
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

    pub fn eval_type(&mut self, body: &HMIRBody, expr: HMIRExprID) -> CXResult<HMIRTypeID> {
        todo!("Point to new API defined in eval/ module")
    }

    pub fn lower_type(&mut self, ty: HMIRTypeID, span: &TokenRange) -> CXResult<MIRTypeID> {
        types::lower_type(self, ty, span)
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
