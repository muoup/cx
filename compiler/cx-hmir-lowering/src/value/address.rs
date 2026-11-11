use cx_hmir::HMIRDefID;
use cx_mir::MIRFunctionID;
use cx_util::identifier::CXIdent;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HMIRAddress<B> {
    base: B,
    offset: i64,
}

impl<B> HMIRAddress<B> {
    pub fn new(base: B, offset: i64) -> Self {
        Self { base, offset }
    }

    pub fn base(&self) -> &B {
        &self.base
    }

    pub fn offset(&self) -> i64 {
        self.offset
    }

    pub fn offset_by(&self, bytes: i64) -> Option<Self>
    where
        B: Clone,
    {
        Some(Self::new(self.base.clone(), self.offset.checked_add(bytes)?))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Addressable {
    Def(HMIRDefID),
    Label {
        function: MIRFunctionID,
        label: CXIdent,
    },
    Literal(String),
}
