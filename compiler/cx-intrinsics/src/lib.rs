mod intrinsic;
mod mapper;
mod source;
#[cfg(test)]
mod tests;

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
pub use source::IntrinsicSource;
