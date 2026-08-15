use std::{error::Error, fmt};

use crate::{
    expr::{MIRBasicBlockID, MIRRegister},
    global::MIRFunctionID,
    ty::MIRTypeID,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRValidationError {
    NonDenseId {
        entity: &'static str,
        function: Option<MIRFunctionID>,
        position: usize,
        actual: usize,
    },
    MissingEntry {
        function: MIRFunctionID,
    },
    EntryOnDeclaration {
        function: MIRFunctionID,
        entry: MIRBasicBlockID,
    },
    IdOutOfRange {
        function: MIRFunctionID,
        block: Option<MIRBasicBlockID>,
        instruction: Option<usize>,
        entity: &'static str,
        id: usize,
        upper_bound: usize,
    },
    EmptyBlock {
        function: MIRFunctionID,
        block: MIRBasicBlockID,
    },
    UnterminatedBlock {
        function: MIRFunctionID,
        block: MIRBasicBlockID,
    },
    InstructionAfterTerminator {
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        terminator: usize,
        instruction: usize,
    },
    EntryBlockParameters {
        function: MIRFunctionID,
        entry: MIRBasicBlockID,
    },
    DuplicateBlockParameter {
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        register: MIRRegister,
    },
    DuplicateRegisterDefinition {
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        instruction: usize,
        register: MIRRegister,
    },
    UndefinedRegister {
        function: MIRFunctionID,
        register: MIRRegister,
    },
    BlockArgumentCount {
        function: MIRFunctionID,
        source: MIRBasicBlockID,
        instruction: usize,
        target: MIRBasicBlockID,
        expected: usize,
        actual: usize,
    },
    BlockArgumentType {
        function: MIRFunctionID,
        source: MIRBasicBlockID,
        instruction: usize,
        target: MIRBasicBlockID,
        argument: usize,
        expected: MIRTypeID,
        actual: MIRTypeID,
    },
    VariantSwitchCaseOutOfRange {
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        instruction: usize,
        variant: usize,
        variant_count: usize,
    },
    DuplicateVariantSwitchCase {
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        instruction: usize,
        variant: usize,
    },
    TypeMismatch {
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        instruction: usize,
        entity: &'static str,
        expected: MIRTypeID,
        actual: MIRTypeID,
    },
}

impl fmt::Display for MIRValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NonDenseId {
                entity,
                function,
                position,
                actual,
            } => {
                write!(f, "non-dense {entity} ID")?;
                if let Some(function) = function {
                    write!(f, " in function {function}")?;
                }
                write!(f, ": slot {position} contains ID {actual}")
            }
            Self::MissingEntry { function } => {
                write!(f, "function {function} has blocks but no entry block")
            }
            Self::EntryOnDeclaration { function, entry } => write!(
                f,
                "function declaration {function} has entry block {entry} but no blocks"
            ),
            Self::IdOutOfRange {
                function,
                block,
                instruction,
                entity,
                id,
                upper_bound,
            } => {
                write!(
                    f,
                    "{entity} ID {id} is out of range 0..{upper_bound} in function {function}"
                )?;
                if let Some(block) = block {
                    write!(f, ", block {block}")?;
                }
                if let Some(instruction) = instruction {
                    write!(f, ", instruction {instruction}")?;
                }
                Ok(())
            }
            Self::EmptyBlock { function, block } => {
                write!(f, "function {function} contains empty block {block}")
            }
            Self::UnterminatedBlock { function, block } => {
                write!(f, "function {function} block {block} is not terminated")
            }
            Self::InstructionAfterTerminator {
                function,
                block,
                terminator,
                instruction,
            } => write!(
                f,
                "function {function} block {block} has instruction {instruction} after terminator {terminator}"
            ),
            Self::EntryBlockParameters { function, entry } => write!(
                f,
                "function {function} entry block {entry} cannot declare CFG parameters"
            ),
            Self::DuplicateBlockParameter {
                function,
                block,
                register,
            } => write!(
                f,
                "function {function} block {block} reuses block parameter register {register}"
            ),
            Self::DuplicateRegisterDefinition {
                function,
                block,
                instruction,
                register,
            } => write!(
                f,
                "function {function} block {block} instruction {instruction} redefines register {register}"
            ),
            Self::UndefinedRegister { function, register } => {
                write!(f, "function {function} never defines register {register}")
            }
            Self::BlockArgumentCount {
                function,
                source,
                instruction,
                target,
                expected,
                actual,
            } => write!(
                f,
                "function {function} block {source} instruction {instruction} passes {actual} arguments to {target}, expected {expected}"
            ),
            Self::BlockArgumentType {
                function,
                source,
                instruction,
                target,
                argument,
                expected,
                actual,
            } => write!(
                f,
                "function {function} block {source} instruction {instruction} passes {actual} as argument {argument} to {target}, expected {expected}"
            ),
            Self::VariantSwitchCaseOutOfRange {
                function,
                block,
                instruction,
                variant,
                variant_count,
            } => write!(
                f,
                "function {function} block {block} instruction {instruction} switches on variant {variant}, but the sum has {variant_count} variants"
            ),
            Self::DuplicateVariantSwitchCase {
                function,
                block,
                instruction,
                variant,
            } => write!(
                f,
                "function {function} block {block} instruction {instruction} repeats variant case {variant}"
            ),
            Self::TypeMismatch {
                function,
                block,
                instruction,
                entity,
                expected,
                actual,
            } => write!(
                f,
                "function {function} block {block} instruction {instruction} has {entity} type {actual}, expected {expected}"
            ),
        }
    }
}

impl MIRValidationError {
    pub fn instruction_location(&self) -> Option<(MIRFunctionID, MIRBasicBlockID, usize)> {
        match self {
            Self::IdOutOfRange {
                function,
                block: Some(block),
                instruction: Some(instruction),
                ..
            } => Some((*function, *block, *instruction)),
            Self::InstructionAfterTerminator {
                function,
                block,
                instruction,
                ..
            }
            | Self::DuplicateRegisterDefinition {
                function,
                block,
                instruction,
                ..
            }
            | Self::BlockArgumentCount {
                function,
                source: block,
                instruction,
                ..
            }
            | Self::BlockArgumentType {
                function,
                source: block,
                instruction,
                ..
            }
            | Self::VariantSwitchCaseOutOfRange {
                function,
                block,
                instruction,
                ..
            }
            | Self::DuplicateVariantSwitchCase {
                function,
                block,
                instruction,
                ..
            }
            | Self::TypeMismatch {
                function,
                block,
                instruction,
                ..
            } => Some((*function, *block, *instruction)),
            _ => None,
        }
    }
}

impl Error for MIRValidationError {}
