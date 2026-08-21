use cx_log::CXResult;
use cx_mir::MIRUnit;
use cx_thir::THIRUnit;

pub mod builder;
pub mod const_eval;
pub(crate) mod lowering;

pub use builder::MIRBuilder;
pub use const_eval::MIRConstEvalError;

pub fn generate_mir(thir: &THIRUnit) -> CXResult<MIRUnit> {
    let mut builder = MIRBuilder::new(thir);
    lowering::lower_unit(&mut builder, thir)?;
    Ok(builder.finish())
}
