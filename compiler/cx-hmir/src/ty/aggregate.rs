use cx_util::identifier::CXIdent;

use crate::expr::HMIRExprID;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HMIRTypeSemantics<M = HMIRMoveSemantics> {
    pub constant: bool,
    pub volatile: bool,
    pub unsafe_move: bool,
    pub move_semantics: M,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HMIRMoveSemantics {
    #[default]
    POD,
    Nocopy,
    Nodrop,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HMIRDeclaredMove {
    Explicit(HMIRMoveSemantics),
    CopyTraits(HMIRExprID),
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
            volatile: self.volatile || specifiers.volatile,
            unsafe_move: self.unsafe_move || specifiers.unsafe_move,
            move_semantics: self.move_semantics.max(specifiers.move_semantics),
        }
    }
}

impl<M> HMIRTypeSemantics<M> {
    pub fn map<N, E>(
        &self,
        move_semantics: impl FnOnce(&M) -> Result<N, E>,
    ) -> Result<HMIRTypeSemantics<N>, E> {
        Ok(HMIRTypeSemantics {
            constant: self.constant,
            volatile: self.volatile,
            unsafe_move: self.unsafe_move,
            move_semantics: move_semantics(&self.move_semantics)?,
        })
    }
}

impl Default for HMIRDeclaredMove {
    fn default() -> Self {
        Self::Explicit(HMIRMoveSemantics::default())
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
