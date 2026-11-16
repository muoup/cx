use std::{convert::Infallible, marker::PhantomData};

pub trait IntrinsicMapper<V, T> {
    type Value;
    type Type;
    type Error;

    fn value(&mut self, value: &V) -> Result<Self::Value, Self::Error>;
    fn ty(&mut self, ty: &T) -> Result<Self::Type, Self::Error>;
}

pub(crate) struct ClosureMapper<FV, FT, V2, T2> {
    value: FV,
    ty: FT,
    output: PhantomData<fn() -> (V2, T2)>,
}

impl<FV, FT, V2, T2> ClosureMapper<FV, FT, V2, T2> {
    pub(crate) fn new(value: FV, ty: FT) -> Self {
        Self {
            value,
            ty,
            output: PhantomData,
        }
    }
}

impl<V, T, V2, T2, FV, FT> IntrinsicMapper<V, T> for ClosureMapper<FV, FT, V2, T2>
where
    FV: FnMut(&V) -> V2,
    FT: FnMut(&T) -> T2,
{
    type Value = V2;
    type Type = T2;
    type Error = Infallible;

    fn value(&mut self, value: &V) -> Result<V2, Infallible> {
        Ok((self.value)(value))
    }

    fn ty(&mut self, ty: &T) -> Result<T2, Infallible> {
        Ok((self.ty)(ty))
    }
}
