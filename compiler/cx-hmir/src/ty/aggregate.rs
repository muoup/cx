use cx_util::identifier::CXIdent;

use crate::expr::HMIRExprID;

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
pub struct HMIRFieldDef {
    name: Option<CXIdent>,
    ty: HMIRExprID,
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

impl Default for HMIRTypeSemantics {
    fn default() -> Self {
        Self {
            constant: false,
            unsafe_move: false,
            move_semantics: HMIRMoveSemantics::POD,
        }
    }
}

impl HMIRFieldDef {
    pub fn new(name: Option<CXIdent>, ty: HMIRExprID, bit_width: Option<usize>) -> Self {
        Self {
            name,
            ty,
            bit_width,
        }
    }

    pub fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    pub fn ty(&self) -> HMIRExprID {
        self.ty
    }

    pub fn bit_width(&self) -> Option<usize> {
        self.bit_width
    }
}
