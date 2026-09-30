mod intrinsic;
mod mapper;

pub use intrinsic::{
    Intrinsic, IntrinsicArg,
    aggregate::AggregateIntrinsic,
    float::{FloatBinaryOp, FloatIntrinsic},
    int::{IntBinaryOp, IntIntrinsic, IntUnaryOp},
    internal::InternalIntrinsic,
    pointer::{PointerCompareOp, PointerIntrinsic, PointerOffsetOp},
    va::VAIntrinsic,
};
pub use mapper::IntrinsicMapper;
