use cx_log::CXResult;
use cx_thir::thir::{data::THIRFnPrototype, expression::THIRExpression};

use crate::{log::AnalysisDiagnosticSource, thir_conversion::environment::FMIREnvironment};

pub(crate) fn validate_safe_function_signature(
    env: &mut FMIREnvironment,
    prototype: &THIRFnPrototype,
    body: &THIRExpression,
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
