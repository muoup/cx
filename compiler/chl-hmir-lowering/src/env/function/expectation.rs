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
    escaping: bool,
}

impl Expectations {
    pub fn new(permission: Permission) -> Self {
        Self {
            permission,
            pure: false,
            escaping: false,
        }
    }

    pub fn allowing_escape(mut self) -> Self {
        self.escaping = true;
        self
    }

    pub fn allows_escape(self) -> bool {
        self.escaping
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

    pub fn allows_impure(self) -> bool {
        !self.pure || self.permission == Permission::Unsafe
    }
}
