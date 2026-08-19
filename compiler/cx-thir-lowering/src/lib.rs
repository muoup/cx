use cx_log::CXResult;
use cx_mir::MIRUnit;
use cx_thir::THIRUnit;

pub mod builder;
pub(crate) mod lowering;

pub use builder::MIRBuilder;

pub fn generate_mir(thir: &THIRUnit) -> CXResult<MIRUnit> {
    let mut builder = MIRBuilder::new(thir);
    lowering::lower_unit(&mut builder, thir)?;
    
    let mut mir = builder.finish();
    mir.compute_layouts().map_err(|error| {
        cx_log::error::CXErr::new(
            cx_log::error::message::CXStdErrMessage::error("MIRLayoutError", error.to_string()),
            cx_log::error::context::CXInternalContext::error(
                "MIR layout calculation failed during MIR generation",
            ),
        )
    })?;

    Ok(mir)
}
