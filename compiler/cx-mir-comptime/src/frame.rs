use std::collections::HashMap;

use cx_mir::{MIRParameterID, MIRPlace, MIRPlaceID, MIRRegister};

use crate::value::RuntimeValue;

pub(crate) struct ExecutionFrame {
    registers: HashMap<MIRRegister, RuntimeValue>,
    locals: HashMap<MIRPlaceID, RuntimeValue>,
    parameters: HashMap<MIRParameterID, RuntimeValue>,
}

impl ExecutionFrame {
    pub(crate) fn new(arguments: &[cx_mir::MIRConstant]) -> Self {
        let parameters = arguments
            .iter()
            .cloned()
            .enumerate()
            .map(|(index, value)| (MIRParameterID::new(index), RuntimeValue::Constant(value)))
            .collect();
        Self {
            registers: HashMap::new(),
            locals: HashMap::new(),
            parameters,
        }
    }

    pub(crate) fn register(&self, register: MIRRegister) -> Option<RuntimeValue> {
        self.registers.get(&register).cloned()
    }

    pub(crate) fn set_register(&mut self, register: MIRRegister, value: RuntimeValue) {
        self.registers.insert(register, value);
    }

    pub(crate) fn place(&self, place: MIRPlace) -> Option<RuntimeValue> {
        match place {
            MIRPlace::FunctionLocal(place) => self.locals.get(&place).cloned(),
            MIRPlace::Parameter(parameter) => self.parameters.get(&parameter).cloned(),
            MIRPlace::Global(_) => None,
        }
    }

    pub(crate) fn set_place(&mut self, place: MIRPlace, value: RuntimeValue) -> Result<(), String> {
        match place {
            MIRPlace::FunctionLocal(place) => {
                self.locals.insert(place, value);
                Ok(())
            }
            MIRPlace::Parameter(_) => Err("cannot assign to a function parameter".to_owned()),
            MIRPlace::Global(global) => Err(format!(
                "cannot assign to global {global} during comptime evaluation"
            )),
        }
    }

    pub(crate) fn remove_place(&mut self, place: MIRPlace) {
        if let MIRPlace::FunctionLocal(place) = place {
            self.locals.remove(&place);
        }
    }
}
