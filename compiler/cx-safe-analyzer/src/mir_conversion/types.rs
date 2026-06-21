use cx_log::CXResult;
use cx_mir::mir::{data::MIRFunctionPrototype, expression::MIRExpression};

use crate::{log::AnalysisDiagnosticSource, mir_conversion::environment::FMIREnvironment};

pub(crate) fn validate_safe_function_signature(
    env: &mut FMIREnvironment,
    prototype: &MIRFunctionPrototype,
    body: &MIRExpression,
) -> CXResult<()> {
    if !prototype.signature().contract.safe {
        return Ok(());
    }

    if prototype.signature().var_args {
        return env.log_error(
            body,
            format!("Safe function '{}' may not use varargs", prototype.name()),
        );
    }

    Ok(())
}
