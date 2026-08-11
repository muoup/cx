use cx_lmir::LMIRUnit;
use cx_log::CXResult;
use cx_thir::THIRUnit;

use crate::{builder::LMIRBuilder, mir_lowering::lower_mir};

pub mod builder;
pub mod mir_lowering;

pub(crate) mod log;

pub type LMIRResult<T> = Option<T>;

pub fn generate_lmir(mir: &THIRUnit) -> CXResult<LMIRUnit> {
    let mut builder = LMIRBuilder::new(mir);

    lower_mir(&mut builder, mir)?;

    Ok(builder.finish())
}
