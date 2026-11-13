pub trait IntrinsicSource<V, T> {
    type Error;

    fn value(&mut self) -> Result<V, Self::Error>;
    fn ty(&mut self) -> Result<T, Self::Error>;
    fn bool(&mut self) -> Result<bool, Self::Error>;
    fn string(&mut self) -> Result<Option<String>, Self::Error>;
}
