use crate::log::mir_error;
use cx_log::CXResult;
use cx_mir::{
    MIRAggregateOp, MIRAssignTarget, MIRBasicBlockID, MIRBlockTarget, MIRInstrKind, MIRPlace,
    MIRPlaceAggregateOp, MIRPlaceID, MIRRegister, MIRScopeID, MIRValue, MIRValueAggregateOp,
};
use cx_tokens::TokenRange;
use std::collections::{HashMap, HashSet};

pub(super) struct Remap<'a> {
    pub registers: &'a HashMap<MIRRegister, MIRValue>,
    pub places: &'a HashMap<MIRPlaceID, MIRPlace>,
    pub omitted_places: &'a HashSet<MIRPlaceID>,
    pub blocks: &'a HashMap<MIRBasicBlockID, MIRBasicBlockID>,
    pub block_params: &'a HashMap<MIRBasicBlockID, Vec<bool>>,
    pub scopes: &'a HashMap<MIRScopeID, MIRScopeID>,
    pub range: &'a TokenRange,
}

impl Remap<'_> {
    pub(super) fn value(&self, value: &MIRValue) -> CXResult<MIRValue> {
        let registers = self.registers;
        let omitted_places = self.omitted_places;
        Ok(match value {
            MIRValue::Register(register) => registers.get(register).cloned().ok_or_else(|| {
                mir_error(
                    self.range,
                    format!("template register {register:?} has no rewrite"),
                )
            })?,
            MIRValue::PlaceRef(MIRPlace::FunctionLocal(id))
            | MIRValue::Copy(MIRPlace::FunctionLocal(id))
            | MIRValue::Move(MIRPlace::FunctionLocal(id))
                if omitted_places.contains(id) =>
            {
                MIRValue::Constant(cx_mir::MIRConstant::Unit)
            }
            MIRValue::PlaceRef(place) => MIRValue::PlaceRef(self.place(*place)?),
            MIRValue::Copy(place) => MIRValue::Copy(self.place(*place)?),
            MIRValue::Move(place) => MIRValue::Move(self.place(*place)?),
            MIRValue::Constant(value) => MIRValue::Constant(value.clone()),
        })
    }

    fn place(&self, place: MIRPlace) -> CXResult<MIRPlace> {
        let places = self.places;
        match place {
            MIRPlace::FunctionLocal(id) => places
                .get(&id)
                .copied()
                .ok_or_else(|| mir_error(self.range, "template place has no rewrite")),
            MIRPlace::Parameter(_) => Err(mir_error(
                self.range,
                "staged template retained a comptime function parameter",
            )),
            MIRPlace::Global(id) => Ok(MIRPlace::Global(id)),
        }
    }

    fn target(&self, target: &MIRBlockTarget) -> CXResult<MIRBlockTarget> {
        let blocks = self.blocks;
        let block_params = self.block_params;
        Ok(MIRBlockTarget::with_args(
            *blocks
                .get(&target.block)
                .ok_or_else(|| mir_error(self.range, "template block target has no rewrite"))?,
            target
                .args
                .iter()
                .enumerate()
                .filter(|(index, _)| {
                    block_params
                        .get(&target.block)
                        .and_then(|params| params.get(*index))
                        .copied()
                        .unwrap_or(false)
                })
                .map(|(_, value)| self.value(value))
                .collect::<CXResult<Vec<_>>>()?,
        ))
    }

    fn register(&self, register: MIRRegister) -> CXResult<MIRRegister> {
        let registers = self.registers;
        match registers.get(&register) {
            Some(MIRValue::Register(register)) => Ok(*register),
            _ => Err(mir_error(
                self.range,
                "instruction output register has no concrete rewrite",
            )),
        }
    }

    fn optional_register(&self, register: MIRRegister) -> CXResult<Option<MIRRegister>> {
        let registers = self.registers;
        match registers.get(&register) {
            Some(MIRValue::Register(register)) => Ok(Some(*register)),
            Some(MIRValue::Constant(cx_mir::MIRConstant::Unit)) => Ok(None),
            _ => Err(mir_error(
                self.range,
                "instruction output register has no concrete rewrite",
            )),
        }
    }

    fn omitted_place(&self, place: MIRPlace) -> bool {
        let omitted_places = self.omitted_places;
        matches!(place, MIRPlace::FunctionLocal(id) if omitted_places.contains(&id))
    }

    fn omitted_register(&self, register: MIRRegister) -> bool {
        let registers = self.registers;
        matches!(
            registers.get(&register),
            Some(MIRValue::Constant(cx_mir::MIRConstant::Unit))
        )
    }

    pub(super) fn omitted(&self, kind: &MIRInstrKind) -> bool {
        match kind {
            MIRInstrKind::Initialize { place }
            | MIRInstrKind::Leak { place }
            | MIRInstrKind::Create { out: place, .. }
            | MIRInstrKind::Dereference { out: place, .. } => self.omitted_place(*place),
            MIRInstrKind::Assign { target, .. } => match target {
                MIRAssignTarget::Place(place) => self.omitted_place(*place),
                MIRAssignTarget::Register(register) => self.omitted_register(*register),
            },
            MIRInstrKind::AddressOf { out, .. }
            | MIRInstrKind::VaArg { out, .. }
            | MIRInstrKind::BinOp { out, .. }
            | MIRInstrKind::UnOp { out, .. }
            | MIRInstrKind::Coerce { out, .. } => self.omitted_register(*out),
            MIRInstrKind::AggregateOp(MIRAggregateOp::Place { out, .. }) => {
                self.omitted_place(*out)
            }
            MIRInstrKind::AggregateOp(MIRAggregateOp::Value { out, .. }) => {
                self.omitted_register(*out)
            }
            _ => false,
        }
    }

    pub(super) fn instruction(&self, kind: &MIRInstrKind) -> CXResult<MIRInstrKind> {
        let value = |value: &MIRValue| self.value(value);
        let place = |place| self.place(place);
        let register = |register| self.register(register);
        let target = |target: &MIRBlockTarget| self.target(target);
        let scopes = self.scopes;
        Ok(match kind {
            MIRInstrKind::ScopeEnter { scope } => MIRInstrKind::ScopeEnter {
                scope: scopes[scope],
            },
            MIRInstrKind::ScopeExit { scope } => MIRInstrKind::ScopeExit {
                scope: scopes[scope],
            },
            MIRInstrKind::Initialize { place: output } => MIRInstrKind::Initialize {
                place: place(*output)?,
            },
            MIRInstrKind::Leak { place: output } => MIRInstrKind::Leak {
                place: place(*output)?,
            },
            MIRInstrKind::Create { out, ty } => MIRInstrKind::Create {
                out: place(*out)?,
                ty: *ty,
            },
            MIRInstrKind::Assign {
                target: output,
                value: input,
                ty,
            } => MIRInstrKind::Assign {
                target: match output {
                    MIRAssignTarget::Place(output) => MIRAssignTarget::Place(place(*output)?),
                    MIRAssignTarget::Register(output) => {
                        MIRAssignTarget::Register(register(*output)?)
                    }
                },
                value: value(input)?,
                ty: *ty,
            },
            MIRInstrKind::AddressOf { out, place: input } => MIRInstrKind::AddressOf {
                out: register(*out)?,
                place: place(*input)?,
            },
            MIRInstrKind::Dereference {
                out,
                pointer,
                pointee_type,
            } => MIRInstrKind::Dereference {
                out: place(*out)?,
                pointer: value(pointer)?,
                pointee_type: *pointee_type,
            },
            MIRInstrKind::AggregateOp(operation) => MIRInstrKind::AggregateOp(match operation {
                MIRAggregateOp::Place { out, op } => MIRAggregateOp::Place {
                    out: place(*out)?,
                    op: match op {
                        MIRPlaceAggregateOp::Field {
                            base,
                            field,
                            aggregate_type,
                        } => MIRPlaceAggregateOp::Field {
                            base: place(*base)?,
                            field: *field,
                            aggregate_type: *aggregate_type,
                        },
                        MIRPlaceAggregateOp::Index {
                            base,
                            index,
                            element_type,
                        } => MIRPlaceAggregateOp::Index {
                            base: place(*base)?,
                            index: value(index)?,
                            element_type: *element_type,
                        },
                        MIRPlaceAggregateOp::Variant {
                            base,
                            variant,
                            sum_type,
                        } => MIRPlaceAggregateOp::Variant {
                            base: place(*base)?,
                            variant: *variant,
                            sum_type: *sum_type,
                        },
                    },
                },
                MIRAggregateOp::Value { out, op } => MIRAggregateOp::Value {
                    out: register(*out)?,
                    op: match op {
                        MIRValueAggregateOp::Discriminant {
                            value: input,
                            sum_type,
                        } => MIRValueAggregateOp::Discriminant {
                            value: value(input)?,
                            sum_type: *sum_type,
                        },
                        MIRValueAggregateOp::Construct { ty, fields } => {
                            MIRValueAggregateOp::Construct {
                                ty: *ty,
                                fields: fields
                                    .iter()
                                    .map(|(index, field)| Ok((*index, value(field)?)))
                                    .collect::<CXResult<Vec<_>>>()?,
                            }
                        }
                        MIRValueAggregateOp::Variant {
                            variant,
                            value: input,
                            sum_type,
                        } => MIRValueAggregateOp::Variant {
                            variant: *variant,
                            value: value(input)?,
                            sum_type: *sum_type,
                        },
                        MIRValueAggregateOp::ProjectVariant {
                            variant,
                            value: input,
                            sum_type,
                        } => MIRValueAggregateOp::ProjectVariant {
                            variant: *variant,
                            value: value(input)?,
                            sum_type: *sum_type,
                        },
                    },
                },
            }),
            MIRInstrKind::Call {
                out,
                kind,
                callee,
                args,
            } => MIRInstrKind::Call {
                out: out
                    .map(|out| self.optional_register(out))
                    .transpose()?
                    .flatten(),
                kind: *kind,
                callee: value(callee)?,
                args: args.iter().map(value).collect::<CXResult<Vec<_>>>()?,
            },
            MIRInstrKind::VaStart { list, last } => MIRInstrKind::VaStart {
                list: value(list)?,
                last: value(last)?,
            },
            MIRInstrKind::VaEnd { list } => MIRInstrKind::VaEnd { list: value(list)? },
            MIRInstrKind::VaArg { out, list, ty } => MIRInstrKind::VaArg {
                out: register(*out)?,
                list: value(list)?,
                ty: *ty,
            },
            MIRInstrKind::BinOp { out, op, lhs, rhs } => MIRInstrKind::BinOp {
                out: register(*out)?,
                op: op.clone(),
                lhs: value(lhs)?,
                rhs: value(rhs)?,
            },
            MIRInstrKind::UnOp { out, op, operand } => MIRInstrKind::UnOp {
                out: register(*out)?,
                op: op.clone(),
                operand: value(operand)?,
            },
            MIRInstrKind::Coerce {
                out,
                operand,
                coercion,
                to_type,
            } => MIRInstrKind::Coerce {
                out: register(*out)?,
                operand: value(operand)?,
                coercion: coercion.clone(),
                to_type: *to_type,
            },
            MIRInstrKind::Assert { condition, message } => MIRInstrKind::Assert {
                condition: value(condition)?,
                message: message.clone(),
            },
            MIRInstrKind::Assume { condition } => MIRInstrKind::Assume {
                condition: value(condition)?,
            },
            MIRInstrKind::Return { value: returned } => MIRInstrKind::Return {
                value: returned.as_ref().map(value).transpose()?,
            },
            MIRInstrKind::StagedYield { value: yielded, ty } => MIRInstrKind::StagedYield {
                value: yielded.as_ref().map(value).transpose()?,
                ty: *ty,
            },
            MIRInstrKind::Jump {
                target: destination,
            } => MIRInstrKind::Jump {
                target: target(destination)?,
            },
            MIRInstrKind::Branch {
                cond,
                true_target,
                false_target,
            } => MIRInstrKind::Branch {
                cond: value(cond)?,
                true_target: target(true_target)?,
                false_target: target(false_target)?,
            },
            MIRInstrKind::IntSwitch {
                value: subject,
                cases,
                default,
            } => MIRInstrKind::IntSwitch {
                value: value(subject)?,
                cases: cases
                    .iter()
                    .map(|(case, destination)| Ok((case.clone(), target(destination)?)))
                    .collect::<CXResult<Vec<_>>>()?,
                default: default.as_ref().map(target).transpose()?,
            },
            MIRInstrKind::VariantSwitch {
                subject,
                sum_type,
                cases,
                default,
            } => MIRInstrKind::VariantSwitch {
                subject: value(subject)?,
                sum_type: *sum_type,
                cases: cases
                    .iter()
                    .map(|(case, destination)| Ok((*case, target(destination)?)))
                    .collect::<CXResult<Vec<_>>>()?,
                default: default.as_ref().map(target).transpose()?,
            },
            MIRInstrKind::Unreachable => MIRInstrKind::Unreachable,
            MIRInstrKind::MakeStaged { .. }
            | MIRInstrKind::ApplyStaged { .. }
            | MIRInstrKind::StagedReturn { .. }
            | MIRInstrKind::StagedExit { .. }
            | MIRInstrKind::StagedMove { .. }
            | MIRInstrKind::StagedUse { .. } => {
                return Err(mir_error(
                    self.range,
                    "nested staged instruction was not expanded",
                ));
            }
        })
    }
}
