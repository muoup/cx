use cx_lmir::LMIRUnit;
use cx_mir::mir::program::MIRUnit;
use cx_util::CXResult;

use crate::{builder::LMIRBuilder, mir_lowering::lower_mir};

pub mod builder;
pub mod mir_lowering;

pub type LMIRResult<T> = Option<T>;

pub fn generate_lmir(mir: &MIRUnit) -> CXResult<LMIRUnit> {
    let mut builder = LMIRBuilder::new(mir);
    
    lower_mir(&mut builder, mir)?;
    
    Ok(builder.finish())
}