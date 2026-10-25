#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Permission {
    Safe,
    Nonsafe,
    Unsafe,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Expectations {
    permission: Permission,
    pure: bool,
    deferred: bool,
}

impl Expectations {
    pub fn new(permission: Permission) -> Self {
        Self {
            permission,
            pure: false,
            deferred: false,
        }
    }

    pub fn permission(self) -> Permission {
        self.permission
    }

    pub fn permitting(mut self, permission: Permission) -> Self {
        self.permission = permission;
        self
    }

    pub fn expecting_pure(mut self) -> Self {
        self.pure = true;
        self
    }

    pub fn deferring(mut self) -> Self {
        self.deferred = true;
        self
    }

    pub fn deferred(self) -> bool {
        self.deferred
    }

    pub fn allows_impure(self) -> bool {
        !self.pure || self.permission == Permission::Unsafe
    }
}
