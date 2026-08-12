use std::{collections::BTreeSet, error::Error, fmt};

use cx_ast::ast::modifiers::CXLinkageMode;
use cx_util::identifier::CXIdent;

use crate::{
    expr::{
        MIRAggregateOp, MIRBasicBlockID, MIRBlockTarget, MIRConstant, MIRInstrKind, MIRPlace,
        MIRPlaceAggregateOp, MIRRegister, MIRValue, MIRValueAggregateOp,
    },
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
                instruction.for_each_defined_register(|register| {
                    if !register_definitions.insert(register) && duplicate_register.is_none() {
                        duplicate_register = Some(register);
                    }
                });
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

        let mut target_error = None;
        instruction.for_each_target(|target| {
            if target_error.is_none() {
                target_error = self
                    .validate_target(function, block, instruction_index, target)
                    .err();
            }
        });
        if let Some(error) = target_error {
            return Err(error);
        }
        self.validate_instruction_types(function, block, instruction_index, &instruction.kind)
    }

    fn validate_instruction_types(
        &self,
        function: &MIRFunction,
        block: MIRBasicBlockID,
        instruction: usize,
        kind: &MIRInstrKind,
    ) -> Result<(), MIRValidationError> {
        match kind {
            MIRInstrKind::Create { out, ty } => {
                self.expect_place_type(function, block, instruction, "created place", *out, ty)?;
            }
            MIRInstrKind::Assign { dest, value, ty } => {
                self.expect_place_type(
                    function,
                    block,
                    instruction,
                    "assignment destination",
                    *dest,
                    ty,
                )?;
                if let MIRValue::Move(source) = value {
                    self.expect_place_type(
                        function,
                        block,
                        instruction,
                        "moved source",
                        *source,
                        ty,
                    )?;
                } else {
                    self.expect_value_type(
                        function,
                        block,
                        instruction,
                        "assignment value",
                        value,
                        ty,
                    )?;
                }
            }
            MIRInstrKind::AggregateOp(MIRAggregateOp::Place {
                out,
                op: MIRPlaceAggregateOp::Dereference { pointee_type, .. },
            }) => {
                self.expect_place_type(
                    function,
                    block,
                    instruction,
                    "dereference result",
                    *out,
                    pointee_type,
                )?;
            }
            MIRInstrKind::AggregateOp(MIRAggregateOp::Place {
                out,
                op: MIRPlaceAggregateOp::Index { element_type, .. },
            }) => {
                self.expect_place_type(
                    function,
                    block,
                    instruction,
                    "index result",
                    *out,
                    element_type,
                )?;
            }
            MIRInstrKind::AggregateOp(MIRAggregateOp::Value {
                out,
                op: MIRValueAggregateOp::Construct { ty, .. },
            }) => {
                self.expect_register_type(
                    function,
                    block,
                    instruction,
                    "aggregate result",
                    *out,
                    ty,
                )?;
            }
            MIRInstrKind::AggregateOp(MIRAggregateOp::Value {
                out,
                op: MIRValueAggregateOp::Variant { sum_type, .. },
            }) => {
                self.expect_register_type(
                    function,
                    block,
                    instruction,
                    "variant result",
                    *out,
                    sum_type,
                )?;
            }
            MIRInstrKind::Coerce { out, to_type, .. } => {
                self.expect_register_type(
                    function,
                    block,
                    instruction,
                    "coercion result",
                    *out,
                    to_type,
                )?;
            }
            MIRInstrKind::VariantSwitch {
                subject,
                sum_type,
                cases,
                ..
            } => {
                self.expect_place_type(
                    function,
                    block,
                    instruction,
                    "variant switch subject",
                    *subject,
                    sum_type,
                )?;
                if let cx_thir::thir::r#type::THIRTypeKind::TaggedUnion { variants } =
                    &sum_type.0.kind
                {
                    let mut seen = BTreeSet::new();
                    for (variant, _) in cases {
                        if *variant >= variants.len() {
                            return Err(MIRValidationError::VariantSwitchCaseOutOfRange {
                                function: function.id,
                                block,
                                instruction,
                                variant: *variant,
                                variant_count: variants.len(),
                            });
                        }
                        if !seen.insert(*variant) {
                            return Err(MIRValidationError::DuplicateVariantSwitchCase {
                                function: function.id,
                                block,
                                instruction,
                                variant: *variant,
                            });
                        }
                    }
                }
            }
            MIRInstrKind::Return { value: Some(value) } => {
                if let Some(return_type) = &function.prototype.signature.return_type {
                    self.expect_value_type(
                        function,
                        block,
                        instruction,
                        "return value",
                        value,
                        return_type,
                    )?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn expect_place_type(
        &self,
        function: &MIRFunction,
        block: MIRBasicBlockID,
        instruction: usize,
        entity: &'static str,
        place: MIRPlace,
        expected: &MIRType,
    ) -> Result<(), MIRValidationError> {
        let actual = self
            .place_type(function, place)
            .expect("validated place is missing");
        self.expect_type(function.id, block, instruction, entity, actual, expected)
    }

    fn expect_register_type(
        &self,
        function: &MIRFunction,
        block: MIRBasicBlockID,
        instruction: usize,
        entity: &'static str,
        register: MIRRegister,
        expected: &MIRType,
    ) -> Result<(), MIRValidationError> {
        let actual = &function
            .register(register)
            .expect("validated register is missing")
            .ty;
        self.expect_type(function.id, block, instruction, entity, actual, expected)
    }

    fn expect_value_type(
        &self,
        function: &MIRFunction,
        block: MIRBasicBlockID,
        instruction: usize,
        entity: &'static str,
        value: &MIRValue,
        expected: &MIRType,
    ) -> Result<(), MIRValidationError> {
        let Some(actual) = self.value_type(function, value) else {
            return Ok(());
        };
        self.expect_type(function.id, block, instruction, entity, &actual, expected)
    }

    fn expect_type(
        &self,
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        instruction: usize,
        entity: &'static str,
        actual: &MIRType,
        expected: &MIRType,
    ) -> Result<(), MIRValidationError> {
        if actual.same_as(expected) {
            Ok(())
        } else {
            Err(MIRValidationError::TypeMismatch {
                function,
                block,
                instruction,
                entity,
                expected: expected.to_string(),
                actual: actual.to_string(),
            })
        }
    }

    fn validate_target(
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
            let expected = &function
                .register(*parameter)
                .expect("validated block parameter is missing")
                .ty;
            if let Some(actual) = self.value_type(function, argument)
                && !actual.same_as(expected)
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

    fn value_type(&self, function: &MIRFunction, value: &MIRValue) -> Option<MIRType> {
        match value {
            MIRValue::Register(register) => function
                .register(*register)
                .map(|register| register.ty.clone()),
            // A plain place operand performs the MIR-level implicit read.
            // Projection places can retain a reference-shaped storage type, so
            // their loaded value type is not recoverable without the THIR registry.
            MIRValue::Place(_) => None,
            // A move consumes the place itself, whose declared storage type is
            // therefore the type transferred to the consumer.
            MIRValue::Move(place) => self.place_type(function, *place).cloned(),
            MIRValue::Constant(MIRConstant::Unit) => Some(MIRType::from_kind(
                cx_thir::thir::r#type::THIRTypeKind::Unit,
            )),
            MIRValue::Constant(MIRConstant::Bool(_)) => Some(MIRType::from_kind(
                cx_thir::thir::r#type::THIRTypeKind::Integer {
                    _type: cx_thir::thir::r#type::THIRIntType::I1,
                    signed: false,
                },
            )),
            MIRValue::Constant(MIRConstant::Integer { ty, signed, .. }) => Some(
                MIRType::from_kind(cx_thir::thir::r#type::THIRTypeKind::Integer {
                    _type: *ty,
                    signed: *signed,
                }),
            ),
            MIRValue::Constant(MIRConstant::Float { ty, .. }) => Some(MIRType::from_kind(
                cx_thir::thir::r#type::THIRTypeKind::Float { _type: *ty },
            )),
            MIRValue::Constant(
                MIRConstant::Null | MIRConstant::Function(_) | MIRConstant::Undefined,
            ) => None,
        }
    }

    fn place_type<'a>(&'a self, function: &'a MIRFunction, place: MIRPlace) -> Option<&'a MIRType> {
        match place {
            MIRPlace::FunctionLocal(id) => function.place(id).map(|place| &place.ty),
            MIRPlace::Parameter(id) => function
                .prototype
                .signature
                .params
                .get(id.index())
                .map(|parameter| &parameter.ty),
            MIRPlace::Global(id) => self.global(id).map(|global| &global.ty),
        }
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
        expected: String,
        actual: String,
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
        expected: String,
        actual: String,
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

impl Error for MIRValidationError {}

#[cfg(test)]
mod tests {
    use cx_ast::ast::modifiers::CXLinkageMode;
    use cx_thir::thir::r#type::{THIRIntType, THIRTypeKind};
    use cx_util::identifier::CXIdent;

    use crate::{
        MIRBlockTarget, MIRConstant, MIRFnPrototype, MIRFnSignature, MIRInstrKind, MIRType,
        MIRValidationError, MIRValue,
    };

    use super::MIRUnit;

    fn integer_type(signed: bool) -> MIRType {
        MIRType::from_kind(THIRTypeKind::Integer {
            _type: THIRIntType::I8,
            signed,
        })
    }

    fn unit_with_join(parameter_type: MIRType, argument: Option<MIRValue>) -> MIRUnit {
        let prototype = MIRFnPrototype::new(
            MIRFnSignature::new(
                CXIdent::from("join"),
                Vec::new(),
                Some(parameter_type.clone()),
            ),
            CXLinkageMode::Standard,
        );
        let mut unit = MIRUnit::new();
        let function_id = unit.add_function(prototype);
        let function = unit.function_mut(function_id).expect("function exists");
        let entry = function.add_block();
        let join = function.add_block();
        let result = function
            .add_block_param(join, parameter_type, None)
            .expect("join block exists");
        function.push_instr(
            entry,
            MIRInstrKind::Jump {
                target: MIRBlockTarget::with_args(join, argument.into_iter().collect()),
            },
        );
        function.push_instr(
            join,
            MIRInstrKind::Return {
                value: Some(MIRValue::Register(result)),
            },
        );
        unit
    }

    #[test]
    fn block_argument_matching_parameter_is_valid() {
        let ty = integer_type(true);
        let unit = unit_with_join(
            ty,
            Some(MIRValue::Constant(MIRConstant::Integer {
                value: 7,
                ty: THIRIntType::I8,
                signed: true,
            })),
        );
        assert_eq!(unit.validate(), Ok(()));
    }

    #[test]
    fn block_argument_count_must_match_parameters() {
        let unit = unit_with_join(integer_type(true), None);
        assert!(matches!(
            unit.validate(),
            Err(MIRValidationError::BlockArgumentCount {
                expected: 1,
                actual: 0,
                ..
            })
        ));
    }

    #[test]
    fn block_argument_type_must_match_parameter() {
        let unit = unit_with_join(
            integer_type(true),
            Some(MIRValue::Constant(MIRConstant::Integer {
                value: 7,
                ty: THIRIntType::I8,
                signed: false,
            })),
        );
        assert!(matches!(
            unit.validate(),
            Err(MIRValidationError::BlockArgumentType { .. })
        ));
    }

    #[test]
    fn moved_assignment_source_must_match_destination_type() {
        let prototype = MIRFnPrototype::new(
            MIRFnSignature::new(CXIdent::from("move_type"), Vec::new(), None),
            CXLinkageMode::Standard,
        );
        let mut unit = MIRUnit::new();
        let function_id = unit.add_function(prototype);
        let function = unit.function_mut(function_id).expect("function exists");
        let source = function.add_place(integer_type(false), None);
        let destination = function.add_place(integer_type(true), None);
        let entry = function.add_block();
        function.push_instr(
            entry,
            MIRInstrKind::Assign {
                dest: destination,
                value: MIRValue::Move(source),
                ty: integer_type(true),
            },
        );
        function.push_instr(entry, MIRInstrKind::Return { value: None });

        assert!(matches!(
            unit.validate(),
            Err(MIRValidationError::TypeMismatch {
                entity: "moved source",
                ..
            })
        ));
        assert_eq!(
            MIRInstrKind::Assign {
                dest: source,
                value: MIRValue::Move(destination),
                ty: integer_type(true),
            }
            .to_string(),
            "%p0 = move %p1: i8"
        );
    }

    #[test]
    fn moved_return_value_uses_the_source_place_type() {
        let prototype = MIRFnPrototype::new(
            MIRFnSignature::new(
                CXIdent::from("move_return_type"),
                Vec::new(),
                Some(integer_type(true)),
            ),
            CXLinkageMode::Standard,
        );
        let mut unit = MIRUnit::new();
        let function_id = unit.add_function(prototype);
        let function = unit.function_mut(function_id).expect("function exists");
        let source = function.add_place(integer_type(false), None);
        let entry = function.add_block();
        function.push_instr(
            entry,
            MIRInstrKind::Return {
                value: Some(MIRValue::Move(source)),
            },
        );

        assert!(matches!(
            unit.validate(),
            Err(MIRValidationError::TypeMismatch {
                entity: "return value",
                ..
            })
        ));
    }
}
