use cx_util::{dense_id, unsafe_float::FloatWrapper};

use crate::{
    staged::MIRStagedExpression,
    ty::{MIRFloatType, MIRIntType, MIRTypeID},
    unit::{MIRGlobalID, function::MIRFunctionID},
    value::MIRValue,
};

dense_id!(MIRConstantID, "%c");
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
        fields: Vec<(usize, MIRConstantID)>,
    },
    String(String),
    Global {
        global: MIRGlobalID,
        offset: i64,
        ty: MIRTypeID,
    },
    Nullptr {
        ty: MIRTypeID,
    },
    Function(MIRFunctionID),
    Staged(MIRStagedID),
    RuntimeValue(MIRValue),
    Undefined,
}

#[derive(Debug, Clone, Default)]
pub struct MIRConstantPool<'thir> {
    constants: Vec<MIRConstant>,
    staged_expressions: Vec<MIRStagedExpression<'thir>>,
}

impl<'thir> MIRConstantPool<'thir> {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            staged_expressions: Vec::new(),
        }
    }

    pub fn add_constant(&mut self, constant: MIRConstant) -> MIRConstantID {
        self.constants.push(constant);
        MIRConstantID::new(self.constants.len() - 1)
    }

    pub fn add_staged_expression(&mut self, expr: MIRStagedExpression<'thir>) -> MIRStagedID {
        self.staged_expressions.push(expr);
        MIRStagedID::new(self.staged_expressions.len() - 1)
    }

    pub fn constants(&self) -> &[MIRConstant] {
        &self.constants
    }

    pub fn constant(&self, id: MIRConstantID) -> Option<&MIRConstant> {
        self.constants.get(id.index())
    }

    pub fn staged_expressions(&self) -> &[MIRStagedExpression<'thir>] {
        &self.staged_expressions
    }

    pub fn staged_expression(&mut self, id: MIRStagedID) -> Option<&MIRStagedExpression<'thir>> {
        self.staged_expressions.get(id.index())
    }
}
