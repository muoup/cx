mod module;
mod symbols;

use cx_hmir::{HMIRDefID, HMIRUnit};
use cx_pipeline_data::db::ModuleData;

pub struct HMIREnvironment<'global, 'hmir> {
    global: &'global ModuleData,
    hmir: &'hmir HMIRUnit,
}

impl<'_, 'hmir> HMIREnvironment<'hmir> {
    pub fn finish(self) -> MIRUnit {
        todo!()
    }
}
