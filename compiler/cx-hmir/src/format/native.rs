use cx_intrinsics::IntrinsicArg;
use cx_util::identifier::CXIdent;

use crate::expr::{
    HMIRExprID, HMIRIntrinsic,
    aggregate::HMIRAggregateOp,
    op::{HMIRControlOp, HMIRMemberStep, HMIROp, HMIROwnershipOp, HMIRTypeOp, HMIRUnaryOp},
};

use super::body::BodyPrinter;

pub(super) enum Arg {
    Value(HMIRExprID),
    Place(HMIRExprID),
    Named(CXIdent, HMIRExprID),
    Text(String),
}

pub(super) fn render_args(
    args: &[Arg],
    render: &mut dyn FnMut(HMIRExprID, bool) -> String,
) -> Vec<String> {
    args.iter()
        .map(|arg| match arg {
            Arg::Value(id) => render(*id, false),
            Arg::Place(id) => render(*id, true),
            Arg::Named(name, id) => format!(".{name} = {}", render(*id, false)),
            Arg::Text(text) => text.clone(),
        })
        .collect()
}

fn values(ids: &[HMIRExprID]) -> Vec<Arg> {
    ids.iter().map(|id| Arg::Value(*id)).collect()
}

fn fields(ty: HMIRExprID, fields: &[(Option<CXIdent>, HMIRExprID)]) -> Vec<Arg> {
    let fields = fields.iter().map(|(name, value)| match name {
        Some(name) => Arg::Named(name.clone(), *value),
        None => Arg::Value(*value),
    });
    std::iter::once(Arg::Value(ty)).chain(fields).collect()
}

impl BodyPrinter<'_> {
    pub(super) fn native_parts(&self, op: &HMIROp) -> (String, Vec<Arg>) {
        match op {
            HMIROp::BinOp { op, lhs, rhs } => (op.path().to_string(), values(&[*lhs, *rhs])),
            HMIROp::UnOp { op, operand } => {
                let operand = match op {
                    HMIRUnaryOp::PreIncrement
                    | HMIRUnaryOp::PreDecrement
                    | HMIRUnaryOp::PostIncrement
                    | HMIRUnaryOp::PostDecrement => Arg::Place(*operand),
                    _ => Arg::Value(*operand),
                };
                (op.path().to_string(), vec![operand])
            }
            HMIROp::Coerce {
                mode,
                value,
                target,
            } => (
                format!("{}<{}>", mode.path(), self.inline(*target)),
                values(&[*value]),
            ),
            HMIROp::Assign { target, op, value } => {
                let head = match op {
                    Some(op) => format!("op.assign<{}>", op.path()),
                    None => "op.assign".to_string(),
                };
                (head, vec![Arg::Place(*target), Arg::Value(*value)])
            }
            HMIROp::AddressOf(operand) => ("op.address_of".to_string(), vec![Arg::Place(*operand)]),
            HMIROp::Dereference(operand) => ("op.dereference".to_string(), values(&[*operand])),
            HMIROp::Type(op) => self.type_parts(op),
            HMIROp::Control(op) => control_parts(op),
            HMIROp::OwnershipOp(op) => {
                let (HMIROwnershipOp::Adopt(operand)
                | HMIROwnershipOp::Leak(operand)
                | HMIROwnershipOp::Move(operand)
                | HMIROwnershipOp::Copy(operand)) = op;
                (op.path().to_string(), values(&[*operand]))
            }
            HMIROp::AggregateOp(op) => self.aggregate_parts(op),
        }
    }

    fn aggregate_parts(&self, op: &HMIRAggregateOp) -> (String, Vec<Arg>) {
        let args = match op {
            HMIRAggregateOp::Member { base, name } => {
                vec![Arg::Value(*base), Arg::Text(name.to_string())]
            }
            HMIRAggregateOp::Index { base, index } => values(&[*base, *index]),
            HMIRAggregateOp::Initialize { ty, fields: items } => fields(*ty, items),
            HMIRAggregateOp::Is { value, pattern } => {
                vec![Arg::Value(*value), Arg::Text(self.pattern(pattern))]
            }
            HMIRAggregateOp::Unpack { value, bindings } => {
                let bindings = bindings.iter().map(|(field, local)| {
                    Arg::Text(format!(".{field} = {}", self.local_decl(*local)))
                });
                std::iter::once(Arg::Value(*value))
                    .chain(bindings)
                    .collect()
            }
        };
        (op.path().to_string(), args)
    }

    fn type_parts(&self, op: &HMIRTypeOp) -> (String, Vec<Arg>) {
        let args = match op {
            HMIRTypeOp::Member { ty, name } => {
                vec![Arg::Value(*ty), Arg::Text(name.to_string())]
            }
            HMIRTypeOp::Aggregate { ty, pairs } => fields(*ty, pairs),
            HMIRTypeOp::PointerInner(ty)
            | HMIRTypeOp::ReferenceInner(ty)
            | HMIRTypeOp::TypeOf(ty)
            | HMIRTypeOp::Decay(ty)
            | HMIRTypeOp::SizeOf(ty)
            | HMIRTypeOp::AlignOf(ty)
            | HMIRTypeOp::IsInt(ty)
            | HMIRTypeOp::IsFloat(ty)
            | HMIRTypeOp::IsPointer(ty)
            | HMIRTypeOp::IsSigned(ty) => values(&[*ty]),
            HMIRTypeOp::Equal(lhs, rhs) => values(&[*lhs, *rhs]),
            HMIRTypeOp::OffsetOf { ty, member } => {
                let steps = member
                    .iter()
                    .map(|step| match step {
                        HMIRMemberStep::Field(name) => format!(".{name}"),
                        HMIRMemberStep::Index(index) => format!("[{}]", self.inline(*index)),
                    })
                    .collect::<String>();
                vec![Arg::Value(*ty), Arg::Text(steps)]
            }
        };
        (op.path().to_string(), args)
    }

    pub(super) fn intrinsic_args(&self, intrinsic: &HMIRIntrinsic) -> Vec<Arg> {
        intrinsic
            .args()
            .into_iter()
            .map(|arg| match arg {
                IntrinsicArg::Value(value) | IntrinsicArg::Type(value) => Arg::Value(*value),
                IntrinsicArg::Index(value) => Arg::Text(value.to_string()),
                IntrinsicArg::Bool(value) => Arg::Text(value.to_string()),
                IntrinsicArg::String(value) => Arg::Text(format!("{value:?}")),
            })
            .collect()
    }
}

fn control_parts(op: &HMIRControlOp) -> (String, Vec<Arg>) {
    let args = match op {
        HMIRControlOp::Return(Some(operand))
        | HMIRControlOp::Yield(Some(operand))
        | HMIRControlOp::Defer(operand)
        | HMIRControlOp::Unsafe(operand)
        | HMIRControlOp::IndirectGoto(operand) => values(&[*operand]),
        HMIRControlOp::Goto(label) | HMIRControlOp::LabelAddress { name: label, .. } => {
            vec![Arg::Text(label.to_string())]
        }
        HMIRControlOp::Return(None)
        | HMIRControlOp::Yield(None)
        | HMIRControlOp::Break
        | HMIRControlOp::Continue
        | HMIRControlOp::Unreachable => Vec::new(),
    };
    (op.path().to_string(), args)
}
