pub(crate) mod module;
pub(crate) mod symbols;
pub(crate) mod lowering;
pub(crate) mod resolver;

use cx_hmir::{
    HMIRBody, HMIRUnit,
    expr::{HMIRExprID, constant::HMIRConstant},
    ty::{HMIRType, HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::CXResult;
use cx_mir::{MIRConstant, MIRTypeID, MIRUnit};
use cx_pipeline_data::db::ModuleData;
use cx_target::ArchitectureConfig;
use cx_tokens::TokenRange;

use crate::env::{module::MIRModuleBuilder, symbols::HMIRSymbolEnv, resolver::HMIRResolver};

#[derive(Debug)]
pub struct HMIREnvironment<'global, 'hmir> {
    global: &'global ModuleData,

    module: MIRModuleBuilder,
    symbols: HMIRSymbolEnv<'global, 'hmir>,
    types: HMIRResolver,
}

impl<'global, 'hmir> HMIREnvironment<'global, 'hmir> {
    pub fn new(
        global: &'global ModuleData,
        unit: &'hmir HMIRUnit,
        architecture: ArchitectureConfig,
    ) -> Self {
        todo!()
    }

    pub fn realize_type(&mut self, kind: HMIRTypeKind, span: &TokenRange) -> HMIRTypeID {
        todo!()
    }

    pub fn same_type(&self, a: HMIRTypeID, b: HMIRTypeID) -> bool {
        todo!()
    }

    pub fn eval_type(&mut self, body: &HMIRBody, expr: HMIRExprID) -> CXResult<HMIRTypeID> {
        todo!()
    }

    pub fn lower_type(&mut self, ty: HMIRTypeID, span: &TokenRange) -> CXResult<MIRTypeID> {
        todo!()
    }

    pub fn lower_constant(
        &mut self,
        constant: &HMIRConstant,
        span: &TokenRange,
    ) -> CXResult<MIRConstant> {
        todo!()
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
    
    pub fn finish(self) -> MIRUnit {
        self.module.finish()
    }
}

impl HMIRTypeContext for HMIREnvironment<'_, '_> {
    fn architecture(&self) -> &ArchitectureConfig {
        todo!()
    }

    fn resolve_type_id(&self, id: HMIRTypeID) -> &HMIRType {
        todo!()
    }
}
