use cx_util::{dense_id, unsafe_float::FloatWrapper};

use crate::{
    MIRGlobalRef,
    staged::MIRStagedExpression,
    ty::{MIRFloatType, MIRIntType, MIRTypeID},
    unit::function::MIRFunctionID,
};

dense_id!(MIRStagedID, "%s");

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRConstant {
    Unit,
    Integer {
        ty: MIRIntType,
        value: i128,
    },
    Float {
        value: FloatWrapper,
        ty: MIRFloatType,
    },
    Aggregate {
        ty: MIRTypeID,
        fields: Vec<(usize, MIRConstant)>,
    },
    String(String),
    StringAddress(String),
    ArrayAddress(Box<MIRConstant>),
    GlobalAddress(MIRGlobalRef),
    Nullptr {
        ty: MIRTypeID,
    },
    Function(MIRFunctionID),
    Undefined,
}

#[derive(Debug, Clone, Default)]
pub struct MIRStagedExprPool<'thir> {
    staged_expressions: Vec<MIRStagedExpression<'thir>>,
}

impl<'thir> MIRStagedExprPool<'thir> {
    pub fn new() -> Self {
        Self {
            staged_expressions: Vec::new(),
        }
    }

    pub fn add_staged_expression(&mut self, expr: MIRStagedExpression<'thir>) -> MIRStagedID {
        self.staged_expressions.push(expr);
        MIRStagedID::new(self.staged_expressions.len() - 1)
    }

    pub fn staged_expressions(&self) -> &[MIRStagedExpression<'thir>] {
        &self.staged_expressions
    }

    pub fn staged_expression(&self, id: MIRStagedID) -> Option<&MIRStagedExpression<'thir>> {
        self.staged_expressions.get(id.index())
    }
}
