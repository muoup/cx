use std::collections::BTreeSet;

use crate::{
    expr::{
        MIRAggregateOp, MIRAssignTarget, MIRBasicBlockID, MIRConstant, MIRInstrKind, MIRPlace,
        MIRPlaceAggregateOp, MIRRegister, MIRValue, MIRValueAggregateOp,
    },
    global::{MIRFunction, MIRFunctionID},
    ty::{MIRField, MIRType, MIRTypeID, MIRTypeKind},
    unit::MIRUnit,
};

use super::error::MIRValidationError;

impl MIRUnit {
    pub(super) fn validate_instruction_types(
        &self,
        function: &MIRFunction,
        block: MIRBasicBlockID,
        instruction: usize,
        kind: &MIRInstrKind,
    ) -> Result<(), MIRValidationError> {
        match kind {
            MIRInstrKind::Create { out, ty } => {
                self.expect_place_type(function, block, instruction, "created place", *out, *ty)?;
            }
            MIRInstrKind::Assign { target, value, ty } => {
                match target {
                    MIRAssignTarget::Place(dest) => self.expect_place_value_type(
                        function,
                        block,
                        instruction,
                        "assignment destination",
                        *dest,
                        *ty,
                    )?,
                    MIRAssignTarget::Register(out) => self.expect_register_type(
                        function,
                        block,
                        instruction,
                        "assignment result",
                        *out,
                        *ty,
                    )?,
                }
                if let MIRValue::Move(source) = value {
                    self.expect_place_value_type(
                        function,
                        block,
                        instruction,
                        "moved source",
                        *source,
                        *ty,
                    )?;
                } else {
                    self.expect_value_type(
                        function,
                        block,
                        instruction,
                        "assignment value",
                        value,
                        *ty,
                    )?;
                }
            }
            MIRInstrKind::Dereference {
                out, pointee_type, ..
            } => {
                self.expect_place_value_type(
                    function,
                    block,
                    instruction,
                    "dereference result",
                    *out,
                    *pointee_type,
                )?;
            }
            MIRInstrKind::AggregateOp(MIRAggregateOp::Place {
                out,
                op: MIRPlaceAggregateOp::Index { element_type, .. },
            }) => {
                self.expect_place_value_type(
                    function,
                    block,
                    instruction,
                    "index result",
                    *out,
                    *element_type,
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
                    *ty,
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
                    *sum_type,
                )?;
            }
            MIRInstrKind::AggregateOp(MIRAggregateOp::Value {
                out,
                op:
                    MIRValueAggregateOp::ProjectVariant {
                        variant, sum_type, ..
                    },
            }) => {
                let expected = match self.types().kind(*sum_type) {
                    Some(MIRTypeKind::TaggedUnion { variants }) => variants
                        .get(*variant)
                        .map(MIRField::ty)
                        .ok_or(MIRValidationError::VariantSwitchCaseOutOfRange {
                            function: function.id,
                            block,
                            instruction,
                            variant: *variant,
                            variant_count: variants.len(),
                        })?,
                    _ => *sum_type,
                };
                self.expect_register_type(
                    function,
                    block,
                    instruction,
                    "variant projection result",
                    *out,
                    expected,
                )?;
            }
            MIRInstrKind::Coerce { out, to_type, .. } => {
                self.expect_register_type(
                    function,
                    block,
                    instruction,
                    "coercion result",
                    *out,
                    *to_type,
                )?;
            }
            MIRInstrKind::VariantSwitch {
                subject,
                sum_type,
                cases,
                ..
            } => {
                self.expect_value_type(
                    function,
                    block,
                    instruction,
                    "variant switch subject",
                    subject,
                    *sum_type,
                )?;
                if let Some(MIRTypeKind::TaggedUnion { variants }) = self.types().kind(*sum_type) {
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
                let return_type = function.prototype.signature.return_type;
                if !matches!(self.types().kind(return_type), Some(MIRTypeKind::Void)) {
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

    pub(super) fn expect_place_type(
        &self,
        function: &MIRFunction,
        block: MIRBasicBlockID,
        instruction: usize,
        entity: &'static str,
        place: MIRPlace,
        expected: MIRTypeID,
    ) -> Result<(), MIRValidationError> {
        let actual = self
            .place_type(function, place)
            .expect("validated place is missing");
        self.expect_type(function.id, block, instruction, entity, actual, expected)
    }

    pub(super) fn expect_place_value_type(
        &self,
        function: &MIRFunction,
        block: MIRBasicBlockID,
        instruction: usize,
        entity: &'static str,
        place: MIRPlace,
        expected: MIRTypeID,
    ) -> Result<(), MIRValidationError> {
        let actual = self
            .place_type_for_expected(function, place, expected)
            .expect("validated place is missing");
        self.expect_type(function.id, block, instruction, entity, actual, expected)
    }

    pub(super) fn expect_register_type(
        &self,
        function: &MIRFunction,
        block: MIRBasicBlockID,
        instruction: usize,
        entity: &'static str,
        register: MIRRegister,
        expected: MIRTypeID,
    ) -> Result<(), MIRValidationError> {
        let actual = function
            .register(register)
            .expect("validated register is missing")
            .ty;
        self.expect_type(function.id, block, instruction, entity, actual, expected)
    }

    pub(super) fn expect_value_type(
        &self,
        function: &MIRFunction,
        block: MIRBasicBlockID,
        instruction: usize,
        entity: &'static str,
        value: &MIRValue,
        expected: MIRTypeID,
    ) -> Result<(), MIRValidationError> {
        let Some(actual) = self.value_type_for_expected(function, value, expected) else {
            return Ok(());
        };
        self.expect_type(function.id, block, instruction, entity, actual, expected)
    }

    pub(super) fn expect_type(
        &self,
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        instruction: usize,
        entity: &'static str,
        actual: MIRTypeID,
        expected: MIRTypeID,
    ) -> Result<(), MIRValidationError> {
        if self.types().same_type(actual, expected) {
            Ok(())
        } else {
            Err(MIRValidationError::TypeMismatch {
                function,
                block,
                instruction,
                entity,
                expected,
                actual,
            })
        }
    }

    pub(super) fn value_type_for_expected(
        &self,
        function: &MIRFunction,
        value: &MIRValue,
        expected: MIRTypeID,
    ) -> Option<MIRTypeID> {
        match value {
            MIRValue::Place(place) => {
                let raw = self.place_type(function, *place)?;
                if let Some(MIRTypeKind::MemoryReference { inner, .. }) = self.types().kind(expected)
                    && self.types().same_type(raw, *inner)
                {
                    return Some(expected);
                }
                self.place_type_for_expected(function, *place, expected)
            }
            MIRValue::Copy(place) | MIRValue::Move(place) => {
                self.place_type_for_expected(function, *place, expected)
            }
            _ => self.value_type(function, value),
        }
    }

    pub(super) fn value_type(&self, function: &MIRFunction, value: &MIRValue) -> Option<MIRTypeID> {
        match value {
            MIRValue::Register(register) => {
                function.register(*register).map(|register| register.ty)
            }
            MIRValue::Place(place) | MIRValue::Copy(place) | MIRValue::Move(place) => {
                self.place_value_type(function, *place)
            }
            MIRValue::Constant(MIRConstant::Unit) => Some(self.types().unit()),
            MIRValue::Constant(MIRConstant::Bool(_)) => {
                self.types()
                    .find(&MIRType::new(MIRTypeKind::Integer {
                        ty: crate::MIRIntType::I1,
                        signed: false,
                    }))
            }
            MIRValue::Constant(MIRConstant::Integer { ty, signed, .. }) => {
                self.types()
                    .find(&MIRType::new(MIRTypeKind::Integer {
                        ty: *ty,
                        signed: *signed,
                    }))
            }
            MIRValue::Constant(MIRConstant::Float { ty, .. }) => self
                .types()
                .find(&MIRType::new(MIRTypeKind::Float { ty: *ty })),
            MIRValue::Constant(MIRConstant::String(_)) => {
                self.types().find(&MIRType::new(MIRTypeKind::Str))
            }
            MIRValue::Constant(MIRConstant::Null { ty }) => Some(*ty),
            MIRValue::Constant(MIRConstant::Aggregate { ty, .. }) => Some(*ty),
            MIRValue::Constant(MIRConstant::Global { ty, .. }) => Some(*ty),
            MIRValue::Constant(MIRConstant::GlobalOffset { ty, .. }) => Some(*ty),
            MIRValue::Constant(MIRConstant::Function(_) | MIRConstant::Undefined) => None,
        }
    }

    pub(super) fn place_value_type(
        &self,
        function: &MIRFunction,
        place: MIRPlace,
    ) -> Option<MIRTypeID> {
        let raw = self.place_type(function, place)?;
        match self.types().kind(raw) {
            Some(MIRTypeKind::MemoryReference { inner, .. }) => Some(*inner),
            _ => Some(raw),
        }
    }

    pub(super) fn place_type_for_expected(
        &self,
        function: &MIRFunction,
        place: MIRPlace,
        expected: MIRTypeID,
    ) -> Option<MIRTypeID> {
        let raw = self.place_type(function, place)?;
        let expected_is_reference = matches!(
            self.types().kind(expected),
            Some(MIRTypeKind::MemoryReference { .. })
        );
        if expected_is_reference {
            if let Some(MIRTypeKind::MemoryReference { inner, .. }) = self.types().kind(raw)
                && self.types().same_type(*inner, expected)
            {
                return Some(*inner);
            }
            return Some(raw);
        }
        if self.types().same_type(raw, expected) {
            Some(raw)
        } else {
            self.place_value_type(function, place)
        }
    }

    pub(super) fn place_type(&self, function: &MIRFunction, place: MIRPlace) -> Option<MIRTypeID> {
        match place {
            MIRPlace::FunctionLocal(id) => function.place(id).map(|place| place.ty),
            MIRPlace::Parameter(id) => function
                .prototype
                .signature
                .params
                .get(id.index())
                .map(|parameter| parameter.ty),
            MIRPlace::Global(id) => self.global(id).map(|global| global.ty),
        }
    }
}
