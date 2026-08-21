use std::fmt::{self, Display, Formatter};

use cx_mir::{
    MIRConstant, MIRDiagnostic, MIRDiagnosticLocation, MIRFunctionID, MIRFunctionMode,
    MIRInstrKind, MIRUnit, MIRValue,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRComptimeError {
    diagnostic: MIRDiagnostic,
    trace: Vec<MIRDiagnosticLocation>,
}

impl MIRComptimeError {
    pub fn new(message: impl Into<String>, location: MIRDiagnosticLocation) -> Self {
        Self {
            diagnostic: MIRDiagnostic::new("COMPTIME ERROR", message, location),
            trace: Vec::new(),
        }
    }

    pub fn diagnostic(&self) -> MIRDiagnostic {
        let mut diagnostic = self.diagnostic.clone();
        for (index, location) in self.trace.iter().enumerate() {
            diagnostic.add_note(format!("comptime frame {index}: {location:?}"));
        }
        diagnostic
    }

    pub fn trace(&self) -> &[MIRDiagnosticLocation] {
        &self.trace
    }

    pub fn push_frame(&mut self, location: MIRDiagnosticLocation) {
        self.trace.push(location);
    }

    pub fn with_frame(mut self, location: MIRDiagnosticLocation) -> Self {
        self.push_frame(location);
        self
    }
}

impl Display for MIRComptimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.diagnostic.message(), f)
    }
}

impl std::error::Error for MIRComptimeError {}

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
            MIRComptimeError::new(
                format!("function {function} is not present in the MIR unit"),
                MIRDiagnosticLocation::Internal(format!("missing function {function}")),
            )
        })?;
        if mir_function.mode() == MIRFunctionMode::Runtime {
            return Err(MIRComptimeError::new(
                format!("runtime function {function} cannot be evaluated in comptime"),
                MIRDiagnosticLocation::Internal(format!("runtime function {function}")),
            ));
        }
        let expected_arguments = mir_function.prototype().signature.params.len();
        if expected_arguments != arguments.len() {
            return Err(MIRComptimeError::new(
                format!(
                    "comptime function {function} expects {expected_arguments} arguments, found {}",
                    arguments.len()
                ),
                MIRDiagnosticLocation::Internal(format!("invalid arguments for {function}")),
            ));
        }
        if !arguments.is_empty() {
            return Err(MIRComptimeError::new(
                "comptime parameters are not supported by the initial MIR engine",
                MIRDiagnosticLocation::Internal(format!("parameters for {function}")),
            ));
        }

        let definition = mir_function.definition().ok_or_else(|| {
            MIRComptimeError::new(
                format!("comptime function {function} has no definition"),
                MIRDiagnosticLocation::Internal(format!("declaration {function}")),
            )
        })?;
        let entry = definition.entry().ok_or_else(|| {
            MIRComptimeError::new(
                format!("comptime function {function} has no entry block"),
                MIRDiagnosticLocation::Internal(format!("definition {function}")),
            )
        })?;
        let block = definition.block(entry).ok_or_else(|| {
            MIRComptimeError::new(
                format!("comptime function {function} has an invalid entry block"),
                MIRDiagnosticLocation::Internal(format!("entry block {entry}")),
            )
        })?;

        for (instruction, instruction_data) in block.instrs.iter().enumerate() {
            match &instruction_data.kind {
                MIRInstrKind::ScopeEnter { .. } | MIRInstrKind::ScopeExit { .. } => {}
                MIRInstrKind::Return { value: None } => return Ok(MIRConstant::Unit),
                MIRInstrKind::Return {
                    value: Some(MIRValue::Constant(value)),
                } => return Ok(value.clone()),
                _ => {
                    return Err(MIRComptimeError::new(
                        format!(
                            "MIR instruction is not supported by the initial comptime engine: {:?}",
                            instruction_data.kind
                        ),
                        MIRDiagnosticLocation::Instruction {
                            function,
                            block: entry,
                            instruction,
                        },
                    ));
                }
            }
        }

        Err(MIRComptimeError::new(
            format!("comptime function {function} has no return instruction"),
            MIRDiagnosticLocation::Internal(format!("block {entry}")),
        ))
    }
}
