use crate::{
    expr::{MIRBasicBlockID, MIRPlace},
    global::MIRFunction,
    unit::MIRUnit,
};

use super::error::MIRValidationError;

impl MIRUnit {
    pub(super) fn validate_instruction(
        &self,
        function: &MIRFunction,
        block: MIRBasicBlockID,
        instruction_index: usize,
        instruction: &crate::expr::MIRInstr,
    ) -> Result<(), MIRValidationError> {
        let function_id = function.id;
        let mut bad_id = None;
        let check_place = |place| match place {
            MIRPlace::FunctionLocal(id) if id.index() >= function.places.len() => {
                Some(("place", id.index(), function.places.len()))
            }
            MIRPlace::Parameter(id) if id.index() >= function.prototype.signature.params.len() => {
                Some((
                    "parameter",
                    id.index(),
                    function.prototype.signature.params.len(),
                ))
            }
            MIRPlace::Global(id) if id.index() >= self.globals.len() => {
                Some(("global", id.index(), self.globals.len()))
            }
            _ => None,
        };
        instruction.visit_operands(|operand| {
            if bad_id.is_none()
                && let Some(place) = operand.place()
            {
                bad_id = check_place(place);
            }
            if let Some(register) = operand.register()
                && bad_id.is_none()
                && register.index() >= function.registers.len()
            {
                bad_id = Some(("register", register.index(), function.registers.len()));
            }
            if let Some(referenced) = operand.function()
                && bad_id.is_none()
                && referenced.index() >= self.functions.len()
            {
                bad_id = Some(("function", referenced.index(), self.functions.len()));
            }
        });
        for place in instruction.defined_places() {
            if bad_id.is_none() {
                bad_id = check_place(place);
            }
        }
        for register in instruction.defined_registers() {
            if bad_id.is_none() && register.index() >= function.registers.len() {
                bad_id = Some(("register", register.index(), function.registers.len()));
            }
        }
        for successor in instruction.successors() {
            if bad_id.is_none() && successor.index() >= function.blocks.len() {
                bad_id = Some(("block target", successor.index(), function.blocks.len()));
            }
        }

        if let Some((entity, id, upper_bound)) = bad_id {
            self.check_id(
                function_id,
                Some(block),
                Some(instruction_index),
                entity,
                id,
                upper_bound,
            )?;
        }

        self.validate_targets(function, block, instruction_index, instruction)?;
        self.validate_instruction_types(function, block, instruction_index, &instruction.kind)
    }
}
