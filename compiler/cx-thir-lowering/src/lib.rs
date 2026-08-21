use cx_log::CXResult;
use cx_mir::MIRUnit;
use cx_thir::THIRUnit;

pub mod builder;
pub(crate) mod lowering;

pub use builder::MIRBuilder;
pub use cx_mir_comptime::{MIRComptimeEngine, MIRComptimeError};

pub fn generate_mir(thir: &THIRUnit) -> CXResult<MIRUnit> {
    let mut builder = MIRBuilder::new(thir);
    lowering::lower_unit(&mut builder, thir)?;
    Ok(builder.finish())
}
