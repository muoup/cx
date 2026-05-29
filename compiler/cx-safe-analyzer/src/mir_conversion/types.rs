use cx_mir::mir::{data::MIRFunctionPrototype, expression::MIRExpression};
use cx_util::CXResult;

use crate::{log_analysis_error, mir_conversion::environment::FMIREnvironment};

pub(crate) fn validate_safe_function_signature(
    env: &mut FMIREnvironment,
    prototype: &MIRFunctionPrototype,
    body: &MIRExpression,
) -> CXResult<()> {
    if !prototype.signature.contract.safe {
        return Ok(());
    }

    if prototype.signature.var_args {
        return log_analysis_error!(
            env,
            body,
            "Safe function '{}' may not use varargs",
            prototype.name
        );
    }

    Ok(())
}
