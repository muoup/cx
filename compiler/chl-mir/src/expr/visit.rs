use crate::{
    MIRInstruction,
    expr::{instruction::MIRInstructionKind, intrinsic::*},
    value::{MIRBindable, MIRBlockTarget, MIRTarget, MIRValue},
};

pub fn successors(kind: &MIRInstruction) -> Vec<&MIRBlockTarget> {
    match &kind.kind {
        MIRInstructionKind::Jump { target } => vec![target],
        MIRInstructionKind::Branch {
            true_target,
            false_target,
            ..
        } => vec![true_target, false_target],
        MIRInstructionKind::CaseBranch { cases, default, .. } => {
            let default_iter = default.iter();
            let cases_iter = cases.iter().map(|(_, target)| target);

            cases_iter.chain(default_iter).collect()
        }
        MIRInstructionKind::IndirectJump { targets, .. } => targets.iter().collect(),
        _ => vec![],
    }
}

pub fn visit_bindable_uses(kind: &MIRInstructionKind, mut visit: impl FnMut(MIRBindable)) {
    fn visit_value(value: &MIRValue, visit: &mut impl FnMut(MIRBindable)) {
        match value {
            MIRValue::Register(register) => visit(MIRBindable::Register(*register)),
            MIRValue::PlaceRef { place, .. } => visit(MIRBindable::Place(*place)),
            MIRValue::Indirect { value, .. } => visit_value(value.as_ref(), visit),
            MIRValue::GlobalRef(_) | MIRValue::Immediate(_) => {}
        }
    }

    fn visit_target(target: &MIRTarget, visit: &mut impl FnMut(MIRBindable)) {
        if let MIRTarget::Indirect { value, .. } = target {
            visit_value(value, visit)
        }
    }

    fn intrinsic(op: &MIRIntrinsic, visit: &mut impl FnMut(MIRBindable)) {
        if let Some(output) = op.output_target() {
            visit_target(output, visit);
        }
        match op {
            MIRIntrinsic::Int(op) => match op {
                MIRIntIntrinsic::Neg { value: input, .. }
                | MIRIntIntrinsic::LNot { value: input, .. }
                | MIRIntIntrinsic::BNot { value: input, .. }
                | MIRIntIntrinsic::ToFloat { value: input, .. }
                | MIRIntIntrinsic::IntCast { value: input, .. }
                | MIRIntIntrinsic::ToPtr { value: input, .. } => visit_value(input, visit),
                MIRIntIntrinsic::Add { lhs, rhs, .. }
                | MIRIntIntrinsic::Sub { lhs, rhs, .. }
                | MIRIntIntrinsic::UMul { lhs, rhs, .. }
                | MIRIntIntrinsic::SMul { lhs, rhs, .. }
                | MIRIntIntrinsic::UDiv { lhs, rhs, .. }
                | MIRIntIntrinsic::SDiv { lhs, rhs, .. }
                | MIRIntIntrinsic::UMod { lhs, rhs, .. }
                | MIRIntIntrinsic::SMod { lhs, rhs, .. }
                | MIRIntIntrinsic::Eq { lhs, rhs, .. }
                | MIRIntIntrinsic::Neq { lhs, rhs, .. }
                | MIRIntIntrinsic::ULt { lhs, rhs, .. }
                | MIRIntIntrinsic::SLt { lhs, rhs, .. }
                | MIRIntIntrinsic::ULe { lhs, rhs, .. }
                | MIRIntIntrinsic::SLe { lhs, rhs, .. }
                | MIRIntIntrinsic::UGt { lhs, rhs, .. }
                | MIRIntIntrinsic::SGt { lhs, rhs, .. }
                | MIRIntIntrinsic::UGe { lhs, rhs, .. }
                | MIRIntIntrinsic::SGe { lhs, rhs, .. }
                | MIRIntIntrinsic::LAnd { lhs, rhs, .. }
                | MIRIntIntrinsic::LOr { lhs, rhs, .. }
                | MIRIntIntrinsic::BAnd { lhs, rhs, .. }
                | MIRIntIntrinsic::BOr { lhs, rhs, .. }
                | MIRIntIntrinsic::BXor { lhs, rhs, .. }
                | MIRIntIntrinsic::LShift { lhs, rhs, .. }
                | MIRIntIntrinsic::ARShift { lhs, rhs, .. }
                | MIRIntIntrinsic::LRShift { lhs, rhs, .. } => {
                    visit_value(lhs, visit);
                    visit_value(rhs, visit);
                }
            },
            MIRIntrinsic::Float(op) => match op {
                MIRFloatIntrinsic::Neg { value: input, .. }
                | MIRFloatIntrinsic::ToInt { value: input, .. }
                | MIRFloatIntrinsic::FloatCast { value: input, .. } => visit_value(input, visit),
                MIRFloatIntrinsic::Add { lhs, rhs, .. }
                | MIRFloatIntrinsic::Sub { lhs, rhs, .. }
                | MIRFloatIntrinsic::Mul { lhs, rhs, .. }
                | MIRFloatIntrinsic::Div { lhs, rhs, .. }
                | MIRFloatIntrinsic::Eq { lhs, rhs, .. }
                | MIRFloatIntrinsic::Neq { lhs, rhs, .. }
                | MIRFloatIntrinsic::Lt { lhs, rhs, .. }
                | MIRFloatIntrinsic::Le { lhs, rhs, .. }
                | MIRFloatIntrinsic::Gt { lhs, rhs, .. }
                | MIRFloatIntrinsic::Geq { lhs, rhs, .. } => {
                    visit_value(lhs, visit);
                    visit_value(rhs, visit);
                }
            },
            MIRIntrinsic::Pointer(op) => match op {
                MIRPtrIntrinsic::ToInt { ptr, .. } => visit_value(ptr, visit),
                MIRPtrIntrinsic::Add { ptr, offset, .. }
                | MIRPtrIntrinsic::Sub { ptr, offset, .. } => {
                    visit_value(ptr, visit);
                    visit_value(offset, visit);
                }
                MIRPtrIntrinsic::Diff { lhs, rhs, .. }
                | MIRPtrIntrinsic::Eq { lhs, rhs, .. }
                | MIRPtrIntrinsic::Neq { lhs, rhs, .. }
                | MIRPtrIntrinsic::Lt { lhs, rhs, .. }
                | MIRPtrIntrinsic::Leq { lhs, rhs, .. }
                | MIRPtrIntrinsic::Gt { lhs, rhs, .. }
                | MIRPtrIntrinsic::Geq { lhs, rhs, .. } => {
                    visit_value(lhs, visit);
                    visit_value(rhs, visit);
                }
            },
            MIRIntrinsic::Aggregate(op) => match op {
                MIRAggregateIntrinsic::SumIndex { value: input, .. } => visit_value(input, visit),
                MIRAggregateIntrinsic::SumVariant { base, .. }
                | MIRAggregateIntrinsic::StructField { base, .. } => visit_value(base, visit),
                MIRAggregateIntrinsic::AggregateInit { fields, .. } => {
                    for (_, field) in fields {
                        visit_value(field, visit);
                    }
                }
                MIRAggregateIntrinsic::ArrayIndex { base, index, .. } => {
                    visit_value(base, visit);
                    visit_value(index, visit);
                }
            },
            MIRIntrinsic::Internal(op) => match op {
                MIRInternalIntrinsic::AdoptPlace { address, .. } => visit_value(address, visit),
                MIRInternalIntrinsic::PlaceAddress { place, .. } => {
                    visit(MIRBindable::Place(*place))
                }
                MIRInternalIntrinsic::ReferenceAddress { reference, .. } => {
                    visit_value(reference, visit)
                }
                MIRInternalIntrinsic::ArrayAddress { array, .. } => visit_value(array, visit),
                MIRInternalIntrinsic::StringAddress { .. }
                | MIRInternalIntrinsic::GlobalAddress { .. } => {}
                MIRInternalIntrinsic::Bitcast { value: input, .. } => visit_value(input, visit),
                MIRInternalIntrinsic::Assert { condition, .. }
                | MIRInternalIntrinsic::Assume { condition } => visit_value(condition, visit),
            },
            MIRIntrinsic::VA(op) => match op {
                MIRVAIntrinsic::VaStart { list, last } => {
                    visit_value(list, visit);
                    visit_value(last, visit);
                }
                MIRVAIntrinsic::VaEnd { list } | MIRVAIntrinsic::VaArg { list, .. } => {
                    visit_value(list, visit)
                }
            },
        }
    }

    match kind {
        MIRInstructionKind::Initialize { .. } | MIRInstructionKind::Unreachable => {}
        MIRInstructionKind::Lift { source, .. } => visit_target(source, &mut visit),
        MIRInstructionKind::BindLifetime {
            bind: MIRBindable::Register(register),
            ..
        } => visit(MIRBindable::Register(*register)),
        MIRInstructionKind::Invalidate { .. } | MIRInstructionKind::BindLifetime { .. } => {}
        MIRInstructionKind::Store {
            target: destination,
            value: input,
            ..
        } => {
            visit_target(destination, &mut visit);
            visit_value(input, &mut visit);
        }
        MIRInstructionKind::Call { callee, args, .. } => {
            visit_value(callee, &mut visit);
            for arg in args {
                visit_value(arg, &mut visit);
            }
        }
        MIRInstructionKind::IntrinsicOp(op) => intrinsic(op, &mut visit),
        MIRInstructionKind::Return { value: returned } => {
            if let Some(returned) = returned {
                visit_value(returned, &mut visit);
            }
        }
        MIRInstructionKind::Jump { .. } => {}
        MIRInstructionKind::Branch { cond, .. } => visit_value(cond, &mut visit),
        MIRInstructionKind::CaseBranch { value, .. } => visit_value(value, &mut visit),
        MIRInstructionKind::IndirectJump { address, .. } => visit_value(address, &mut visit),
    }
}
