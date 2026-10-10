use std::rc::Rc;

use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::{HMIRDefID, expr::HMIRExprID, ty::HMIRTypeID};

#[derive(Debug, Clone)]
pub enum HMIRConstant {
    Unit,
    Int {
        value: i128,
        ty: HMIRTypeID,
    },
    Float {
        value: FloatWrapper,
        ty: HMIRTypeID,
    },
    Str(String),
    Null(HMIRTypeID),
    Type(HMIRTypeID),
    // A function def with its leading comptime arguments applied
    Function {
        def: HMIRDefID,
        args: Vec<HMIRConstant>,
    },
    Quote(QuoteRef),
    Aggregate {
        ty: HMIRTypeID,
        fields: Vec<(usize, Box<HMIRConstant>)>,
    },
    // Designates a runtime global; only its address can be taken statically
    Global(HMIRDefID),
    GlobalAddress {
        def: HMIRDefID,
        offset: i64,
        ty: HMIRTypeID,
    },
    // The address of a label of a runtime function
    LabelAddress {
        function: HMIRDefID,
        label: CXIdent,
        ty: HMIRTypeID,
    },
}

#[derive(Debug, Clone)]
pub struct Quote {
    def: HMIRDefID,
    body: HMIRExprID,
}

#[derive(Debug, Clone)]
pub struct QuoteRef(Rc<Quote>);
