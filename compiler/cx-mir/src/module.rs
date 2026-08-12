use std::{collections::BTreeSet, error::Error, fmt};

use cx_ast::ast::modifiers::CXLinkageMode;
use cx_util::identifier::CXIdent;

use crate::{
    expr::{MIRBasicBlockID, MIRPlace},
    global::{MIRFnPrototype, MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalVariable},
    ty::MIRType,
};

#[derive(Debug, Clone, Default)]
pub struct MIRUnit {
    pub functions: Vec<MIRFunction>,
    pub globals: Vec<MIRGlobalVariable>,
}

impl MIRUnit {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_function(&mut self, prototype: MIRFnPrototype) -> MIRFunctionID {
        let id = MIRFunctionID::new(self.functions.len());
        self.functions.push(MIRFunction::new(id, prototype));
        id
    }

    /// Inserts an already-built function and assigns its canonical dense ID.
    pub fn push_function(&mut self, mut function: MIRFunction) -> MIRFunctionID {
        let id = MIRFunctionID::new(self.functions.len());
        function.id = id;
        self.functions.push(function);
        id
    }

    pub fn add_global(
        &mut self,
        name: CXIdent,
        ty: MIRType,
        linkage: CXLinkageMode,
        is_mutable: bool,
    ) -> MIRGlobalID {
        let id = MIRGlobalID::new(self.globals.len());
        self.globals
            .push(MIRGlobalVariable::new(id, name, ty, linkage, is_mutable));
        id
    }

    /// Inserts an already-built global and assigns its canonical dense ID.
    pub fn push_global(&mut self, mut global: MIRGlobalVariable) -> MIRGlobalID {
        let id = MIRGlobalID::new(self.globals.len());
        global.id = id;
        self.globals.push(global);
        id
    }

    pub fn function(&self, id: MIRFunctionID) -> Option<&MIRFunction> {
        self.functions.get(id.index())
    }

    pub fn function_mut(&mut self, id: MIRFunctionID) -> Option<&mut MIRFunction> {
        self.functions.get_mut(id.index())
    }

    pub fn global(&self, id: MIRGlobalID) -> Option<&MIRGlobalVariable> {
        self.globals.get(id.index())
    }

    pub fn global_mut(&mut self, id: MIRGlobalID) -> Option<&mut MIRGlobalVariable> {
        self.globals.get_mut(id.index())
    }

    pub fn validate(&self) -> Result<(), MIRValidationError> {
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

    fn validate_function(&self, function: &MIRFunction) -> Result<(), MIRValidationError> {
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
            }

            if terminated_at.is_none() {
                return Err(MIRValidationError::UnterminatedBlock {
                    function: function_id,
                    block: block.id,
                });
            }
        }

        let mut predecessors = vec![BTreeSet::new(); function.blocks.len()];
        for block in &function.blocks {
            if let Some(terminator) = block.terminator() {
                for successor in terminator.successors() {
                    predecessors[successor.index()].insert(block.id);
                }
            }
        }
        for block in &function.blocks {
            for instruction in &block.instrs {
                if let crate::expr::MIRInstrKind::Phi { incoming, .. } = &instruction.kind {
                    let actual = incoming
                        .iter()
                        .map(|(predecessor, _)| *predecessor)
                        .collect::<BTreeSet<_>>();
                    if actual.len() != incoming.len() || actual != predecessors[block.id.index()] {
                        return Err(MIRValidationError::PhiPredecessorMismatch {
                            function: function_id,
                            block: block.id,
                            expected: predecessors[block.id.index()].iter().copied().collect(),
                            actual: incoming
                                .iter()
                                .map(|(predecessor, _)| *predecessor)
                                .collect(),
                        });
                    }
                }
            }
        }

        Ok(())
    }

    fn validate_instruction(
        &self,
        function: &MIRFunction,
        block: MIRBasicBlockID,
        instruction_index: usize,
        instruction: &crate::expr::MIRInstr,
    ) -> Result<(), MIRValidationError> {
        let function_id = function.id;
        let mut bad_id = None;
        let mut check_place = |place| {
            if bad_id.is_some() {
                return;
            }
            match place {
                MIRPlace::FunctionLocal(id) if id.index() >= function.places.len() => {
                    bad_id = Some(("place", id.index(), function.places.len()));
                }
                MIRPlace::Parameter(id)
                    if id.index() >= function.prototype.signature.params.len() =>
                {
                    bad_id = Some((
                        "parameter",
                        id.index(),
                        function.prototype.signature.params.len(),
                    ));
                }
                MIRPlace::Global(id) if id.index() >= self.globals.len() => {
                    bad_id = Some(("global", id.index(), self.globals.len()));
                }
                _ => {}
            }
        };
        instruction.for_each_referenced_place(&mut check_place);
        instruction.for_each_defined_place(&mut check_place);
        instruction.for_each_referenced_register(|register| {
            if bad_id.is_none() && register.index() >= function.registers.len() {
                bad_id = Some(("register", register.index(), function.registers.len()));
            }
        });
        instruction.for_each_defined_register(|register| {
            if bad_id.is_none() && register.index() >= function.registers.len() {
                bad_id = Some(("register", register.index(), function.registers.len()));
            }
        });
        instruction.kind.for_each_referenced_function(|referenced| {
            if bad_id.is_none() && referenced.index() >= self.functions.len() {
                bad_id = Some(("function", referenced.index(), self.functions.len()));
            }
        });
        instruction.kind.for_each_phi_predecessor(|predecessor| {
            if bad_id.is_none() && predecessor.index() >= function.blocks.len() {
                bad_id = Some((
                    "phi predecessor",
                    predecessor.index(),
                    function.blocks.len(),
                ));
            }
        });
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
        Ok(())
    }

    fn check_id(
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
    PhiPredecessorMismatch {
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        expected: Vec<MIRBasicBlockID>,
        actual: Vec<MIRBasicBlockID>,
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
            Self::PhiPredecessorMismatch {
                function,
                block,
                expected,
                actual,
            } => write!(
                f,
                "function {function} block {block} phi predecessors are {actual:?}, expected {expected:?}"
            ),
        }
    }
}

impl Error for MIRValidationError {}
