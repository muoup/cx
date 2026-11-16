use speedy::{Context, Readable, Reader, Writable};
use std::ops::Deref;
use std::sync::RwLock;

#[derive(Debug)]
pub struct RwLockSer<T> {
    pub inner: RwLock<T>,
}

impl<T> RwLockSer<T> {
    pub fn new(inner: T) -> Self {
        RwLockSer {
            inner: RwLock::new(inner),
        }
    }
}

impl<T> Deref for RwLockSer<T> {
    type Target = RwLock<T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> AsRef<RwLock<T>> for RwLockSer<T> {
    fn as_ref(&self) -> &RwLock<T> {
        &self.inner
    }
}

impl<T> AsMut<RwLock<T>> for RwLockSer<T> {
    fn as_mut(&mut self) -> &mut RwLock<T> {
        &mut self.inner
    }
}

impl<T> From<RwLock<T>> for RwLockSer<T> {
    fn from(inner: RwLock<T>) -> Self {
        RwLockSer { inner }
    }
}

impl<'a, C: Context, T: Readable<'a, C>> Readable<'a, C> for RwLockSer<T> {
    fn read_from<R: Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let inner = T::read_from(reader)?;
        Ok(RwLockSer {
            inner: RwLock::new(inner),
        })
    }
}

impl<C: Context, T: Writable<C>> Writable<C> for RwLockSer<T> {
    fn write_to<W: ?Sized + speedy::Writer<C>>(&self, writer: &mut W) -> Result<(), C::Error> {
        self.inner.read().unwrap().write_to(writer)
    }
}

impl<T: Clone> Clone for RwLockSer<T> {
    fn clone(&self) -> Self {
        RwLockSer {
            inner: RwLock::new(self.inner.read().unwrap().clone()),
        }
    }
}
