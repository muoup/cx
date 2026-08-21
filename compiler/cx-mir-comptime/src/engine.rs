use cx_mir::{MIRConstant, MIRDiagnosticLocation, MIRFunctionID, MIRFunctionMode, MIRUnit};

use crate::{error::MIRComptimeError, execution};

pub struct MIRComptimeEngine<'unit> {
    unit: &'unit MIRUnit,
}

impl<'unit> MIRComptimeEngine<'unit> {
    pub fn new(unit: &'unit MIRUnit) -> Self {
        Self { unit }
    }

    pub fn unit(&self) -> &'unit MIRUnit {
        self.unit
    }

    pub fn evaluate(
        &self,
        function: MIRFunctionID,
        arguments: &[MIRConstant],
    ) -> Result<MIRConstant, MIRComptimeError> {
        let mir_function = self.unit.function(function).ok_or_else(|| {
            self.internal(
                format!("function {function} is not present in the MIR unit"),
                format!("missing function {function}"),
            )
        })?;
        if mir_function.mode() == MIRFunctionMode::Runtime {
            return Err(self.internal(
                format!("runtime function {function} cannot be evaluated in comptime"),
                format!("runtime function {function}"),
            ));
        }
        let expected_arguments = mir_function.prototype().signature.params.len();
        if expected_arguments != arguments.len() {
            return Err(self.internal(
                format!(
                    "comptime function {function} expects {expected_arguments} arguments, found {}",
                    arguments.len()
                ),
                format!("invalid arguments for {function}"),
            ));
        }
        let definition = mir_function.definition().ok_or_else(|| {
            self.internal(
                format!("comptime function {function} has no definition"),
                format!("declaration {function}"),
            )
        })?;
        let entry = definition.entry().ok_or_else(|| {
            self.internal(
                format!("comptime function {function} has no entry block"),
                format!("definition {function}"),
            )
        })?;
        execution::run(self, function, definition, entry, arguments)
    }

    pub(crate) fn internal(
        &self,
        message: impl Into<String>,
        context: impl Into<String>,
    ) -> MIRComptimeError {
        MIRComptimeError::new(message, MIRDiagnosticLocation::Internal(context.into()))
    }

    pub(crate) fn error(
        &self,
        message: impl Into<String>,
        location: MIRDiagnosticLocation,
    ) -> MIRComptimeError {
        MIRComptimeError::new(message, location)
    }
}
