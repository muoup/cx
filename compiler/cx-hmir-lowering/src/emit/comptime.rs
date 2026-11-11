pub mod float;
pub mod int;
pub mod intrinsics;
pub mod storage;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarFault {
    ZeroDivisor,
    Undefined,
}
