use std::collections::BTreeSet;

use crate::{
    expr::MIRBasicBlockID,
    global::{MIRFunction, MIRFunctionID},
    unit::MIRUnit,
};

use super::error::MIRValidationError;

impl MIRUnit {
    pub(super) fn validate_internal(&self) -> Result<(), MIRValidationError> {
        for (index, global) in self.globals.iter().enumerate() {
            if global.id.index() != index {
                return Err(MIRValidationError::NonDenseId {
                    entity: "global",
                    function: None,
                    position: index,
                    actual: global.id.index(),
                });
            }
        }

        for (function_index, function) in self.functions.iter().enumerate() {
            if function.id.index() != function_index {
                return Err(MIRValidationError::NonDenseId {
                    entity: "function",
                    function: None,
                    position: function_index,
                    actual: function.id.index(),
                });
            }
            self.validate_function(function)?;
        }

        Ok(())
    }

    pub(super) fn validate_function(
        &self,
        function: &MIRFunction,
    ) -> Result<(), MIRValidationError> {
        let function_id = function.id;

        for (position, place) in function.places.iter().enumerate() {
            if place.id.index() != position {
                return Err(MIRValidationError::NonDenseId {
                    entity: "place",
                    function: Some(function_id),
                    position,
                    actual: place.id.index(),
                });
            }
        }
        for (position, register) in function.registers.iter().enumerate() {
            if register.id.index() != position {
                return Err(MIRValidationError::NonDenseId {
                    entity: "register",
                    function: Some(function_id),
                    position,
                    actual: register.id.index(),
                });
            }
        }
        for (position, block) in function.blocks.iter().enumerate() {
            if block.id.index() != position {
                return Err(MIRValidationError::NonDenseId {
                    entity: "basic block",
                    function: Some(function_id),
                    position,
                    actual: block.id.index(),
                });
            }
        }

        if function.blocks.is_empty() {
            if let Some(entry) = function.entry {
                return Err(MIRValidationError::EntryOnDeclaration {
                    function: function_id,
                    entry,
                });
            }
            return Ok(());
        }

        let entry = function.entry.ok_or(MIRValidationError::MissingEntry {
            function: function_id,
        })?;
        self.check_id(
            function_id,
            None,
            None,
            "entry block",
            entry.index(),
            function.blocks.len(),
        )?;

        if !function
            .block(entry)
            .expect("validated entry block is missing")
            .params
            .is_empty()
        {
            return Err(MIRValidationError::EntryBlockParameters {
                function: function_id,
                entry,
            });
        }

        let mut block_params = BTreeSet::new();
        for block in &function.blocks {
            for param in &block.params {
                self.check_id(
                    function_id,
                    Some(block.id),
                    None,
                    "block parameter register",
                    param.index(),
                    function.registers.len(),
                )?;
                if !block_params.insert(*param) {
                    return Err(MIRValidationError::DuplicateBlockParameter {
                        function: function_id,
                        block: block.id,
                        register: *param,
                    });
                }
            }
        }
        let mut register_definitions = block_params;

        for block in &function.blocks {
            if block.instrs.is_empty() {
                return Err(MIRValidationError::EmptyBlock {
                    function: function_id,
                    block: block.id,
                });
            }

            let mut terminated_at = None;
            for (instruction_index, instruction) in block.instrs.iter().enumerate() {
                if let Some(terminator) = terminated_at {
                    return Err(MIRValidationError::InstructionAfterTerminator {
                        function: function_id,
                        block: block.id,
                        terminator,
                        instruction: instruction_index,
                    });
                }
                if instruction.is_terminator() {
                    terminated_at = Some(instruction_index);
                }

                self.validate_instruction(function, block.id, instruction_index, instruction)?;
                let mut duplicate_register = None;
                for register in instruction.defined_registers() {
                    if !register_definitions.insert(register) && duplicate_register.is_none() {
                        duplicate_register = Some(register);
                    }
                }
                if let Some(register) = duplicate_register {
                    return Err(MIRValidationError::DuplicateRegisterDefinition {
                        function: function_id,
                        block: block.id,
                        instruction: instruction_index,
                        register,
                    });
                }
            }

            if terminated_at.is_none() {
                return Err(MIRValidationError::UnterminatedBlock {
                    function: function_id,
                    block: block.id,
                });
            }
        }

        for register in &function.registers {
            if !register_definitions.contains(&register.id) {
                return Err(MIRValidationError::UndefinedRegister {
                    function: function_id,
                    register: register.id,
                });
            }
        }

        Ok(())
    }

    pub(super) fn check_id(
        &self,
        function: MIRFunctionID,
        block: Option<MIRBasicBlockID>,
        instruction: Option<usize>,
        entity: &'static str,
        id: usize,
        upper_bound: usize,
    ) -> Result<(), MIRValidationError> {
        if id < upper_bound {
            Ok(())
        } else {
            Err(MIRValidationError::IdOutOfRange {
                function,
                block,
                instruction,
                entity,
                id,
                upper_bound,
            })
        }
    }
}
