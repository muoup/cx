pub(crate) mod module;
pub(crate) mod symbols;
pub(crate) mod lowering;

use cx_mir::MIRUnit;
use cx_pipeline_data::db::ModuleData;

use crate::env::{module::MIRModuleBuilder, symbols::HMIRSymbolEnv};

pub struct HMIREnvironment<'global, 'hmir> {
    global: &'global ModuleData,

    module: MIRModuleBuilder,
    symbols: HMIRSymbolEnv<'global, 'hmir>,
}

impl<'global, 'hmir> HMIREnvironment<'global, 'hmir> {
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
