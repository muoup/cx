use crate::{
    expr::{MIRBasicBlockID, MIRBlockTarget, MIRInstrKind},
    global::MIRFunction,
    unit::MIRUnit,
};

use super::error::MIRValidationError;

impl MIRUnit {
    pub(super) fn validate_targets(
        &self,
        function: &MIRFunction,
        block: MIRBasicBlockID,
        instruction_index: usize,
        instruction: &crate::expr::MIRInstr,
    ) -> Result<(), MIRValidationError> {
        let mut targets = Vec::new();
        match &instruction.kind {
            MIRInstrKind::Jump { target } => targets.push(target),
            MIRInstrKind::Branch {
                true_target,
                false_target,
                ..
            } => {
                targets.push(true_target);
                targets.push(false_target);
            }
            MIRInstrKind::IntSwitch { cases, default, .. } => {
                targets.extend(cases.iter().map(|(_, target)| target));
                targets.extend(default.iter());
            }
            MIRInstrKind::VariantSwitch { cases, default, .. } => {
                targets.extend(cases.iter().map(|(_, target)| target));
                targets.extend(default.iter());
            }
            _ => {}
        }

        for target in targets {
            self.validate_target(function, block, instruction_index, target)?;
        }
        Ok(())
    }

    pub(super) fn validate_target(
        &self,
        function: &MIRFunction,
        source: MIRBasicBlockID,
        instruction: usize,
        target: &MIRBlockTarget,
    ) -> Result<(), MIRValidationError> {
        let Some(block) = function.block(target.block) else {
            return Ok(());
        };
        if target.args.len() != block.params.len() {
            return Err(MIRValidationError::BlockArgumentCount {
                function: function.id,
                source,
                instruction,
                target: target.block,
                expected: block.params.len(),
                actual: target.args.len(),
            });
        }
        for (index, (argument, parameter)) in target.args.iter().zip(&block.params).enumerate() {
            let expected = function
                .register(*parameter)
                .expect("validated block parameter is missing")
                .ty;
            if let Some(actual) = self.value_type_for_expected(function, argument, expected)
                && !self.types.same_type(actual, expected)
            {
                return Err(MIRValidationError::BlockArgumentType {
                    function: function.id,
                    source,
                    instruction,
                    target: target.block,
                    argument: index,
                    expected: expected.to_string(),
                    actual: actual.to_string(),
                });
            }
        }
        Ok(())
    }
}
