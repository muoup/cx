use crate::{
    arg::IntrinsicArg,
    float::FloatIntrinsic,
    int::IntIntrinsic,
    internal::InternalIntrinsic,
    mapper::{ClosureMapper, IntrinsicMapper},
    pointer::PointerIntrinsic,
    va::VAIntrinsic,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Intrinsic<V, T> {
    Int(IntIntrinsic<V, T>),
    Float(FloatIntrinsic<V, T>),
    Pointer(PointerIntrinsic<V, T>),
    Internal(InternalIntrinsic<V, T>),
    VA(VAIntrinsic<V, T>),
}

impl<V, T> Intrinsic<V, T> {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Int(op) => op.path(),
            Self::Float(op) => op.path(),
            Self::Pointer(op) => op.path(),
            Self::Internal(op) => op.path(),
            Self::VA(op) => op.path(),
        }
    }

    pub fn has_result(&self) -> bool {
        match self {
            Self::Int(_) | Self::Float(_) | Self::Pointer(_) => true,
            Self::Internal(op) => op.has_result(),
            Self::VA(op) => op.has_result(),
        }
    }

    pub fn args(&self) -> Vec<IntrinsicArg<'_, V, T>> {
        match self {
            Self::Int(op) => op.args(),
            Self::Float(op) => op.args(),
            Self::Pointer(op) => op.args(),
            Self::Internal(op) => op.args(),
            Self::VA(op) => op.args(),
        }
    }

    pub fn map<V2, T2>(
        &self,
        value: impl FnMut(&V) -> V2,
        ty: impl FnMut(&T) -> T2,
    ) -> Intrinsic<V2, T2> {
        let Ok(mapped) = self.try_map(&mut ClosureMapper::new(value, ty));
        mapped
    }

    pub fn try_map<M: IntrinsicMapper<V, T>>(
        &self,
        mapper: &mut M,
    ) -> Result<Intrinsic<M::Value, M::Type>, M::Error> {
        Ok(match self {
            Self::Int(op) => Intrinsic::Int(op.try_map(mapper)?),
            Self::Float(op) => Intrinsic::Float(op.try_map(mapper)?),
            Self::Pointer(op) => Intrinsic::Pointer(op.try_map(mapper)?),
            Self::Internal(op) => Intrinsic::Internal(op.try_map(mapper)?),
            Self::VA(op) => Intrinsic::VA(op.try_map(mapper)?),
        })
    }
}

impl<V, T> From<IntIntrinsic<V, T>> for Intrinsic<V, T> {
    fn from(value: IntIntrinsic<V, T>) -> Self {
        Self::Int(value)
    }
}

impl<V, T> From<FloatIntrinsic<V, T>> for Intrinsic<V, T> {
    fn from(value: FloatIntrinsic<V, T>) -> Self {
        Self::Float(value)
    }
}

impl<V, T> From<PointerIntrinsic<V, T>> for Intrinsic<V, T> {
    fn from(value: PointerIntrinsic<V, T>) -> Self {
        Self::Pointer(value)
    }
}

impl<V, T> From<InternalIntrinsic<V, T>> for Intrinsic<V, T> {
    fn from(value: InternalIntrinsic<V, T>) -> Self {
        Self::Internal(value)
    }
}

impl<V, T> From<VAIntrinsic<V, T>> for Intrinsic<V, T> {
    fn from(value: VAIntrinsic<V, T>) -> Self {
        Self::VA(value)
    }
}
