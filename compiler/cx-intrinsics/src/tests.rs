use std::vec::IntoIter;

use crate::{
    FloatBinaryOp, FloatIntrinsic, IntBinaryOp, IntIntrinsic, IntUnaryOp, Intrinsic, IntrinsicArg,
    IntrinsicSource, InternalIntrinsic, PointerCompareOp, PointerIntrinsic, PointerOffsetOp,
};

#[derive(Debug, Clone, PartialEq)]
enum Owned {
    Value(u32),
    Type(u32),
    Bool(bool),
    String(String),
}

struct Replay(IntoIter<Owned>);

impl IntrinsicSource<u32, u32> for Replay {
    type Error = String;

    fn value(&mut self) -> Result<u32, String> {
        match self.0.next() {
            Some(Owned::Value(value)) => Ok(value),
            other => Err(format!("expected a value, found {other:?}")),
        }
    }

    fn ty(&mut self) -> Result<u32, String> {
        match self.0.next() {
            Some(Owned::Type(ty)) => Ok(ty),
            other => Err(format!("expected a type, found {other:?}")),
        }
    }

    fn bool(&mut self) -> Result<bool, String> {
        match self.0.next() {
            Some(Owned::Bool(value)) => Ok(value),
            other => Err(format!("expected a bool, found {other:?}")),
        }
    }

    fn string(&mut self) -> Result<Option<String>, String> {
        match self.0.next() {
            Some(Owned::String(value)) => Ok(Some(value)),
            None => Ok(None),
            other => Err(format!("expected a string, found {other:?}")),
        }
    }
}

fn samples() -> Vec<Intrinsic<u32, u32>> {
    let mut samples = Vec::new();
    for op in IntUnaryOp::ALL {
        samples.push(IntIntrinsic::Unary { op, value: 1 }.into());
    }
    for op in IntBinaryOp::ALL {
        samples.push(IntIntrinsic::Binary { op, lhs: 1, rhs: 2 }.into());
    }
    for flag in [true, false] {
        samples.push(IntIntrinsic::ToFloat { value: 1, target: 2, signed: flag }.into());
        samples.push(IntIntrinsic::Cast { value: 1, target: 2, sign_extend: flag }.into());
        samples.push(IntIntrinsic::ToPtr { value: 1, sign_extend: flag }.into());
        samples.push(FloatIntrinsic::ToInt { value: 1, target: 2, signed: flag }.into());
    }
    for op in FloatBinaryOp::ALL {
        samples.push(FloatIntrinsic::Binary { op, lhs: 1, rhs: 2 }.into());
    }
    samples.push(FloatIntrinsic::Neg { value: 1 }.into());
    samples.push(FloatIntrinsic::Cast { value: 1, target: 2 }.into());
    for op in PointerOffsetOp::ALL {
        samples.push(PointerIntrinsic::Offset { op, ptr: 1, offset: 2 }.into());
    }
    for op in PointerCompareOp::ALL {
        samples.push(PointerIntrinsic::Compare { op, lhs: 1, rhs: 2 }.into());
    }
    samples.push(PointerIntrinsic::ToInt { ptr: 1, target: 2 }.into());
    samples.push(PointerIntrinsic::Diff { lhs: 1, rhs: 2, element: 3 }.into());
    samples.push(InternalIntrinsic::Bitcast { value: 1, target: 2 }.into());
    samples.push(InternalIntrinsic::Assert { condition: 1, message: None }.into());
    samples.push(InternalIntrinsic::Assert { condition: 1, message: Some("m".into()) }.into());
    samples.push(InternalIntrinsic::Assume { condition: 1 }.into());
    samples
}

#[test]
fn paths_round_trip() {
    for sample in samples() {
        let args = sample
            .args()
            .into_iter()
            .map(|arg| match arg {
                IntrinsicArg::Value(value) => Owned::Value(*value),
                IntrinsicArg::Type(ty) => Owned::Type(*ty),
                IntrinsicArg::Bool(value) => Owned::Bool(value),
                IntrinsicArg::String(value) => Owned::String(value.to_string()),
                IntrinsicArg::Index(_) => unreachable!("no sample takes an index"),
            })
            .collect::<Vec<_>>();
        let mut replay = Replay(args.into_iter());
        let rebuilt = Intrinsic::from_path(sample.path(), &mut replay);
        assert_eq!(rebuilt, Ok(Some(sample.clone())), "{}", sample.path());
        assert!(replay.0.next().is_none(), "{} left arguments unread", sample.path());
    }
}
