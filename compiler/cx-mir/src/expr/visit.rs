use crate::{
    MIRInstruction,
    constant::MIRConstant,
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
        _ => vec![],
    }
}

pub fn visit_bindable_uses(kind: &MIRInstructionKind, mut visit: impl FnMut(MIRBindable)) {
    fn value(value: &MIRValue, visit: &mut impl FnMut(MIRBindable)) {
        match value {
            MIRValue::Register(register) => visit(MIRBindable::Register(*register)),
            MIRValue::PlaceRef(place) => visit(MIRBindable::Place(*place)),
            MIRValue::Constant(constant) => constant_value(constant, visit)
        }
    }

    fn constant_value(constant: &MIRConstant, visit: &mut impl FnMut(MIRBindable)) {
        match constant {
            MIRConstant::Aggregate { fields, .. } => {
                for (_, field) in fields {
                    constant_value(field, visit);
                }
            }
            _ => {}
        }
    }

    fn target(target: MIRTarget, visit: &mut impl FnMut(MIRBindable)) {
        if let MIRTarget::Indirect(register) = target {
            visit(MIRBindable::Register(register));
        }
    }

    fn block(target: &MIRBlockTarget, visit: &mut impl FnMut(MIRBindable)) {
        for arg in &target.args {
            value(arg, visit);
        }
    }

    fn intrinsic(op: &MIRIntrinsic, visit: &mut impl FnMut(MIRBindable)) {
        if let Some(output) = op.output_target() {
            target(output, visit);
        }
        match op {
            MIRIntrinsic::Int(op) => match op {
                MIRIntIntrinsic::Neg { value: input, .. }
                | MIRIntIntrinsic::LNot { value: input, .. }
                | MIRIntIntrinsic::BNot { value: input, .. }
                | MIRIntIntrinsic::ToFloat { value: input, .. }
                | MIRIntIntrinsic::IntCast { value: input, .. }
                | MIRIntIntrinsic::ToPtr { value: input, .. } => value(input, visit),
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
                    value(lhs, visit);
                    value(rhs, visit);
                }
            },
            MIRIntrinsic::Float(op) => match op {
                MIRFloatIntrinsic::Neg { value: input, .. }
                | MIRFloatIntrinsic::ToInt { value: input, .. }
                | MIRFloatIntrinsic::FloatCast { value: input, .. } => value(input, visit),
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
                    value(lhs, visit);
                    value(rhs, visit);
                }
            },
            MIRIntrinsic::Pointer(op) => match op {
                MIRPtrIntrinsic::ToInt { ptr, .. } => value(ptr, visit),
                MIRPtrIntrinsic::Add { ptr, offset, .. }
                | MIRPtrIntrinsic::Sub { ptr, offset, .. } => {
                    value(ptr, visit);
                    value(offset, visit);
                }
                MIRPtrIntrinsic::Diff { lhs, rhs, .. }
                | MIRPtrIntrinsic::Eq { lhs, rhs, .. }
                | MIRPtrIntrinsic::Neq { lhs, rhs, .. }
                | MIRPtrIntrinsic::Lt { lhs, rhs, .. }
                | MIRPtrIntrinsic::Leq { lhs, rhs, .. }
                | MIRPtrIntrinsic::Gt { lhs, rhs, .. }
                | MIRPtrIntrinsic::Geq { lhs, rhs, .. } => {
                    value(lhs, visit);
                    value(rhs, visit);
                }
            },
            MIRIntrinsic::Aggregate(op) => match op {
                MIRAggregateIntrinsic::SumIndex { value: input, .. } => value(input, visit),
                MIRAggregateIntrinsic::SumVariant { base, .. } => visit(MIRBindable::Place(*base)),
                MIRAggregateIntrinsic::SumVariantL { base, .. }
                | MIRAggregateIntrinsic::StructField { base, .. } => value(base, visit),
                MIRAggregateIntrinsic::AggregateInit { fields, .. } => {
                    for (_, field) in fields {
                        value(field, visit);
                    }
                }
                MIRAggregateIntrinsic::ArrayIndex { base, index, .. } => {
                    value(base, visit);
                    value(index, visit);
                }
            },
            MIRIntrinsic::Internal(op) => match op {
                MIRInternalIntrinsic::PlaceAddress { place, .. } => {
                    visit(MIRBindable::Place(*place))
                }
                MIRInternalIntrinsic::ReferenceAddress { reference, .. } => value(reference, visit),
                MIRInternalIntrinsic::ArrayAddress { array, .. } => value(array, visit),
                MIRInternalIntrinsic::StringAddress { .. }
                | MIRInternalIntrinsic::GlobalAddress { .. }
                | MIRInternalIntrinsic::GetFnPtr { .. } => {}
                MIRInternalIntrinsic::Bitcast { value: input, .. } => value(input, visit),
                MIRInternalIntrinsic::Assert { condition, .. }
                | MIRInternalIntrinsic::Assume { condition } => value(condition, visit),
            },
            MIRIntrinsic::VA(op) => match op {
                MIRVAIntrinsic::VaStart { list, last } => {
                    value(list, visit);
                    value(last, visit);
                }
                MIRVAIntrinsic::VaEnd { list } | MIRVAIntrinsic::VaArg { list, .. } => {
                    value(list, visit)
                }
            },
        }
    }

    match kind {
        MIRInstructionKind::Initialize { .. }
        | MIRInstructionKind::LiftPlace { .. }
        | MIRInstructionKind::Unreachable => {}
        MIRInstructionKind::BindLifetime {
            bind: MIRBindable::Register(register),
            ..
        } => visit(MIRBindable::Register(*register)),
        MIRInstructionKind::Invalidate { .. } | MIRInstructionKind::BindLifetime { .. } => {}
        MIRInstructionKind::Store { target: destination, value: input, .. } => {
            target(*destination, &mut visit);
            value(input, &mut visit);
        }
        MIRInstructionKind::Call { callee, args, .. } => {
            value(callee, &mut visit);
            for arg in args {
                value(arg, &mut visit);
            }
        }
        MIRInstructionKind::IntrinsicOp(op) => intrinsic(op, &mut visit),
        MIRInstructionKind::Return { value: returned } => {
            if let Some(returned) = returned {
                value(returned, &mut visit);
            }
        }
        MIRInstructionKind::Jump { target } => block(target, &mut visit),
        MIRInstructionKind::Branch {
            cond,
            true_target,
            false_target,
        } => {
            value(cond, &mut visit);
            block(true_target, &mut visit);
            block(false_target, &mut visit);
        }
        MIRInstructionKind::CaseBranch {
            value: subject,
            cases,
            default,
        } => {
            value(subject, &mut visit);
            for (_, target) in cases {
                block(target, &mut visit);
            }
            if let Some(target) = default {
                block(target, &mut visit);
            }
        }
    }
}
