use std::fmt::{self, Display, Formatter};

use crate::{
    expr::{MIRBasicBlockID, MIRRegister},
    format::TypePrinter,
    global::MIRFunctionID,
    ty::MIRTypeRegistryBuilder,
    unit::MIRUnit,
};

use crate::validator::error::MIRValidationError;

pub struct MIRValidationErrorDisplay<'a> {
    error: &'a MIRValidationError,
    unit: &'a MIRUnit,
}

impl MIRValidationError {
    pub fn display_with<'a>(&'a self, unit: &'a MIRUnit) -> MIRValidationErrorDisplay<'a> {
        MIRValidationErrorDisplay { error: self, unit }
    }
}

impl Display for MIRValidationErrorDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut context = ValidationFormat {
            unit: self.unit,
            types: TypePrinter::new(self.unit.types()),
        };
        context.write_error(f, self.error)
    }
}

struct ValidationFormat<'a> {
    unit: &'a MIRUnit,
    types: TypePrinter<'a, MIRTypeRegistryBuilder>,
}

impl ValidationFormat<'_> {
    fn write_error(&mut self, f: &mut Formatter<'_>, error: &MIRValidationError) -> fmt::Result {
        match error {
            MIRValidationError::NonDenseId {
                entity,
                function,
                position,
                ..
            } => {
                write!(f, "non-dense {entity} numbering")?;
                if let Some(function) = function {
                    write!(f, " in function ")?;
                    self.write_function(f, *function)?;
                }
                write!(f, ": slot {position} is inconsistent")
            }
            MIRValidationError::MissingEntry { function } => {
                write!(f, "function ")?;
                self.write_function(f, *function)?;
                f.write_str(" has blocks but no entry block")
            }
            MIRValidationError::EntryOnDeclaration { function, .. } => {
                write!(f, "function ")?;
                self.write_function(f, *function)?;
                f.write_str(" is a declaration but has an entry block")
            }
            MIRValidationError::IdOutOfRange {
                function,
                block,
                instruction,
                entity,
                ..
            } => {
                write!(f, "{entity} reference is out of range in function ")?;
                self.write_function(f, *function)?;
                self.write_block_location(f, *function, *block, *instruction)
            }
            MIRValidationError::EmptyBlock { function, block } => {
                write!(f, "function ")?;
                self.write_function(f, *function)?;
                f.write_str(" contains empty block ")?;
                self.write_block(f, *function, *block)
            }
            MIRValidationError::UnterminatedBlock { function, block } => {
                write!(f, "function ")?;
                self.write_function(f, *function)?;
                f.write_str(" block ")?;
                self.write_block(f, *function, *block)?;
                f.write_str(" is not terminated")
            }
            MIRValidationError::InstructionAfterTerminator {
                function,
                block,
                terminator,
                instruction,
            } => {
                self.write_location(f, *function, *block, *instruction)?;
                write!(f, " appears after terminator {terminator}")
            }
            MIRValidationError::EntryBlockParameters { function, entry } => {
                write!(f, "function ")?;
                self.write_function(f, *function)?;
                f.write_str(" entry block ")?;
                self.write_block(f, *function, *entry)?;
                f.write_str(" cannot declare CFG parameters")
            }
            MIRValidationError::DuplicateBlockParameter {
                function,
                block,
                register,
            } => {
                write!(f, "function ")?;
                self.write_function(f, *function)?;
                f.write_str(" block ")?;
                self.write_block(f, *function, *block)?;
                f.write_str(" reuses block parameter ")?;
                self.write_register(f, *function, *register)
            }
            MIRValidationError::DuplicateRegisterDefinition {
                function,
                block,
                instruction,
                register,
            } => {
                self.write_location(f, *function, *block, *instruction)?;
                f.write_str(" redefines ")?;
                self.write_register(f, *function, *register)
            }
            MIRValidationError::UndefinedRegister { function, register } => {
                write!(f, "function ")?;
                self.write_function(f, *function)?;
                f.write_str(" never defines ")?;
                self.write_register(f, *function, *register)
            }
            MIRValidationError::BlockArgumentCount {
                function,
                source,
                instruction,
                target,
                expected,
                actual,
            } => {
                self.write_location(f, *function, *source, *instruction)?;
                write!(f, " passes {actual} arguments to block ")?;
                self.write_block(f, *function, *target)?;
                write!(f, ", expected {expected}")
            }
            MIRValidationError::BlockArgumentType {
                function,
                source,
                instruction,
                target,
                argument,
                expected,
                actual,
            } => {
                self.write_location(f, *function, *source, *instruction)?;
                write!(f, " passes argument {argument} to block ")?;
                self.write_block(f, *function, *target)?;
                f.write_str(" with type ")?;
                self.write_type(f, *actual)?;
                f.write_str(", expected ")?;
                self.write_type(f, *expected)
            }
            MIRValidationError::VariantSwitchCaseOutOfRange {
                function,
                block,
                instruction,
                variant,
                variant_count,
            } => {
                self.write_location(f, *function, *block, *instruction)?;
                write!(
                    f,
                    " switches on variant {variant}, but the sum has {variant_count} variants"
                )
            }
            MIRValidationError::DuplicateVariantSwitchCase {
                function,
                block,
                instruction,
                variant,
            } => {
                self.write_location(f, *function, *block, *instruction)?;
                write!(f, " repeats variant case {variant}")
            }
            MIRValidationError::TypeMismatch {
                function,
                block,
                instruction,
                entity,
                expected,
                actual,
            } => {
                self.write_location(f, *function, *block, *instruction)?;
                f.write_str(" has ")?;
                f.write_str(entity)?;
                f.write_str(" type ")?;
                self.write_type(f, *actual)?;
                f.write_str(", expected ")?;
                self.write_type(f, *expected)
            }
        }
    }

    fn write_location(
        &self,
        f: &mut Formatter<'_>,
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        instruction: usize,
    ) -> fmt::Result {
        f.write_str("function ")?;
        self.write_function(f, function)?;
        f.write_str(" block ")?;
        self.write_block(f, function, block)?;
        write!(f, " instruction {instruction}")
    }

    fn write_block_location(
        &self,
        f: &mut Formatter<'_>,
        function: MIRFunctionID,
        block: Option<MIRBasicBlockID>,
        instruction: Option<usize>,
    ) -> fmt::Result {
        if let Some(block) = block {
            f.write_str(" in block ")?;
            self.write_block(f, function, block)?;
        }
        if let Some(instruction) = instruction {
            write!(f, " at instruction {instruction}")?;
        }
        Ok(())
    }

    fn write_function(&self, f: &mut Formatter<'_>, function: MIRFunctionID) -> fmt::Result {
        if let Some(function) = self.unit.function(function) {
            write!(f, "'{}'", function.prototype().signature.display_name())
        } else {
            f.write_str("'<unknown function>'")
        }
    }

    fn write_block(
        &self,
        f: &mut Formatter<'_>,
        function: MIRFunctionID,
        block: MIRBasicBlockID,
    ) -> fmt::Result {
        if let Some(name) = self
            .unit
            .function(function)
            .and_then(|function| function.definition())
            .and_then(|definition| definition.block(block))
            .and_then(|block| block.debug_name.as_ref())
        {
            write!(f, "'{}'", name)
        } else {
            f.write_str("'<unknown block>'")
        }
    }

    fn write_register(
        &self,
        f: &mut Formatter<'_>,
        function: MIRFunctionID,
        register: MIRRegister,
    ) -> fmt::Result {
        if let Some(name) = self
            .unit
            .function(function)
            .and_then(|function| function.definition())
            .and_then(|definition| definition.register(register))
            .and_then(|register| register.debug_name.as_ref())
        {
            write!(f, "'{}'", name)
        } else {
            f.write_str("'<unnamed register>'")
        }
    }

    fn write_type(&mut self, f: &mut Formatter<'_>, ty: crate::MIRTypeID) -> fmt::Result {
        self.types.write(f, ty)
    }
}
