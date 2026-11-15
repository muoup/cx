#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HMIRApplication<V> {
    callee: Box<V>,
    args: Vec<Option<V>>,
}

impl<V> HMIRApplication<V> {
    pub fn new(callee: V, args: Vec<Option<V>>) -> Self {
        Self {
            callee: Box::new(callee),
            args,
        }
    }

    pub fn callee(&self) -> &V {
        &self.callee
    }

    pub fn args(&self) -> &[Option<V>] {
        &self.args
    }

    pub fn remaining(&self) -> usize {
        self.args.iter().filter(|arg| arg.is_none()).count()
    }

    pub fn bind(&mut self, values: Vec<V>) -> Result<(), usize> {
        let remaining = self.remaining();
        if values.len() > remaining {
            return Err(remaining);
        }
        let mut values = values.into_iter();
        for arg in &mut self.args {
            if arg.is_none() {
                *arg = values.next();
            }
        }
        Ok(())
    }

    pub fn into_parts(self) -> (V, Vec<Option<V>>) {
        (*self.callee, self.args)
    }

    pub fn try_map<U, E>(
        self,
        mut map: impl FnMut(V) -> Result<U, E>,
    ) -> Result<HMIRApplication<U>, E> {
        let callee = map(*self.callee)?;
        let args = self
            .args
            .into_iter()
            .map(|arg| arg.map(&mut map).transpose())
            .collect::<Result<_, _>>()?;
        Ok(HMIRApplication::new(callee, args))
    }
}
