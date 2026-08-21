use cx_mir::{
    MIRConstant, MIRDiagnosticLocation, MIRGlobalKind, MIRGlobalState, MIRPlace, MIRTypeID,
    MIRTypeKind, MIRValue,
};

use crate::{
    engine::MIRComptimeEngine,
    error::MIRComptimeError,
    frame::ExecutionFrame,
    value::{self, RuntimeValue},
};

impl MIRComptimeEngine<'_> {
    pub(crate) fn eval_value(
        &self,
        frame: &mut ExecutionFrame,
        value: &MIRValue,
        _expected: Option<MIRTypeID>,
        location: &MIRDiagnosticLocation,
    ) -> Result<RuntimeValue, MIRComptimeError> {
        match value {
            MIRValue::Register(register) => frame.register(*register).ok_or_else(|| {
                self.error(
                    format!("register {register:?} has no value"),
                    location.clone(),
                )
            }),
            MIRValue::Place(place) => Ok(RuntimeValue::Place(*place)),
            MIRValue::Copy(place) => {
                let value = self.read_place(frame, *place, location)?;
                let value = self.constant(frame, value, None, location)?;
                Ok(RuntimeValue::Constant(value))
            }
            MIRValue::Move(place) => {
                let value = self.read_place(frame, *place, location)?;
                let value = self.constant(frame, value, None, location)?;
                frame.remove_place(*place);
                Ok(RuntimeValue::Constant(value))
            }
            MIRValue::Constant(value) => Ok(RuntimeValue::Constant(value.clone())),
        }
    }

    fn read_place(
        &self,
        frame: &ExecutionFrame,
        place: MIRPlace,
        location: &MIRDiagnosticLocation,
    ) -> Result<RuntimeValue, MIRComptimeError> {
        match place {
            MIRPlace::FunctionLocal(_) | MIRPlace::Parameter(_) => {
                frame.place(place).ok_or_else(|| {
                    self.error(format!("place {place:?} has no value"), location.clone())
                })
            }
            MIRPlace::Global(global) => {
                let global_data = self.unit().global(global).ok_or_else(|| {
                    self.error(
                        format!("global {global} is not present in the MIR unit"),
                        location.clone(),
                    )
                })?;
                let MIRGlobalKind::Variable { ty, state, .. } = &global_data.kind else {
                    return Err(self.error(
                        "string literal cannot be read as a global value",
                        location.clone(),
                    ));
                };
                match state {
                    MIRGlobalState::External => Err(self.error(
                        format!("external global {global} cannot be read during comptime"),
                        location.clone(),
                    )),
                    MIRGlobalState::ZeroInitialized => value::zero(self.unit(), *ty)
                        .map(RuntimeValue::Constant)
                        .map_err(|message| self.error(message, location.clone())),
                    MIRGlobalState::Initializer(_) => Err(self.error(
                        format!("global {global} is not initialized yet"),
                        location.clone(),
                    )),
                    MIRGlobalState::Initialized(value) => Ok(RuntimeValue::Constant(value.clone())),
                }
            }
        }
    }

    pub(crate) fn constant(
        &self,
        frame: &ExecutionFrame,
        value: RuntimeValue,
        expected: Option<MIRTypeID>,
        location: &MIRDiagnosticLocation,
    ) -> Result<MIRConstant, MIRComptimeError> {
        match value {
            RuntimeValue::Constant(value) => Ok(value),
            RuntimeValue::Place(MIRPlace::Global(global)) => {
                let ty = expected.or_else(|| self.global_type(global));
                let ty = ty.ok_or_else(|| {
                    self.error(
                        format!("cannot infer address type for global {global}"),
                        location.clone(),
                    )
                })?;
                Ok(MIRConstant::Global { global, ty })
            }
            RuntimeValue::Place(place) => frame
                .place(place)
                .ok_or_else(|| {
                    self.error(format!("place {place:?} has no value"), location.clone())
                })
                .and_then(|value| self.constant(frame, value, expected, location)),
        }
    }

    pub(crate) fn address(
        &self,
        place: MIRPlace,
        ty: MIRTypeID,
        location: &MIRDiagnosticLocation,
    ) -> Result<RuntimeValue, MIRComptimeError> {
        match place {
            MIRPlace::Global(global) => Ok(RuntimeValue::Constant(MIRConstant::Global { global, ty })),
            MIRPlace::FunctionLocal(_) | MIRPlace::Parameter(_) => Err(self.error(
                "addresses of local comptime storage cannot be materialized as a global initializer",
                location.clone(),
            )),
        }
    }

    fn global_type(&self, global: cx_mir::MIRGlobalID) -> Option<MIRTypeID> {
        match &self.unit().global(global)?.kind {
            MIRGlobalKind::Variable { ty, .. } => Some(*ty),
            MIRGlobalKind::StringLiteral { .. } => self.unit().types().find_kind(&MIRTypeKind::Str),
        }
    }

    pub(crate) fn condition(&self, value: &MIRConstant) -> Result<bool, String> {
        match value {
            MIRConstant::Bool(value) => Ok(*value),
            MIRConstant::Integer { value, .. } => Ok(*value != 0),
            MIRConstant::Float { value, .. } => Ok(f64::from(value) != 0.0),
            MIRConstant::Null { .. } => Ok(false),
            MIRConstant::Global { .. }
            | MIRConstant::GlobalOffset { .. }
            | MIRConstant::Function(_) => Ok(true),
            _ => Err(format!("value {value:?} is not a condition")),
        }
    }
}
