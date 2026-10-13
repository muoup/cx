use cx_util::identifier::CXIdent;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HMIRTypeSemantics {
    pub constant: bool,
    pub unsafe_move: bool,
    pub move_semantics: HMIRMoveSemantics,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HMIRMoveSemantics {
    #[default]
    POD,
    Nocopy,
    Nodrop,
}

#[derive(Debug, Clone)]
pub struct HMIRFieldDef<T> {
    name: Option<CXIdent>,
    ty: T,
    bit_width: Option<usize>,
}

impl HMIRMoveSemantics {
    pub fn is_nocopy(&self) -> bool {
        matches!(self, HMIRMoveSemantics::Nocopy | HMIRMoveSemantics::Nodrop)
    }

    pub fn is_nodrop(&self) -> bool {
        matches!(self, HMIRMoveSemantics::Nodrop)
    }
}

impl HMIRTypeSemantics {
    pub fn apply(self, specifiers: Self) -> Self {
        Self {
            constant: self.constant || specifiers.constant,
            unsafe_move: self.unsafe_move || specifiers.unsafe_move,
            move_semantics: self.move_semantics.max(specifiers.move_semantics),
        }
    }
}

impl Default for HMIRTypeSemantics {
    fn default() -> Self {
        Self {
            constant: false,
            unsafe_move: false,
            move_semantics: HMIRMoveSemantics::POD,
        }
    }
}

impl<T> HMIRFieldDef<T> {
    pub fn new(name: Option<CXIdent>, ty: T, bit_width: Option<usize>) -> Self {
        Self {
            name,
            ty,
            bit_width,
        }
    }

    pub fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    pub fn ty(&self) -> &T {
        &self.ty
    }

    pub fn bit_width(&self) -> Option<usize> {
        self.bit_width
    }

    pub fn map<U, E>(&self, ty: impl FnOnce(&T) -> Result<U, E>) -> Result<HMIRFieldDef<U>, E> {
        Ok(HMIRFieldDef {
            name: self.name.clone(),
            ty: ty(&self.ty)?,
            bit_width: self.bit_width,
        })
    }
}
