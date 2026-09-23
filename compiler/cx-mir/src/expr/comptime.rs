use std::collections::BTreeMap;

use cx_thir::thir::{
    comptime::THIRStagedParameter,
    expression::{THIRExpression, THIRLocalID},
};
use cx_tokens::TokenRange;

use crate::{
    expr::{
        body::MIRBody,
        instruction::{MIRInstruction, MIRInstructionLike},
    },
    unit::function::MIRFunctionID,
    value::{MIRRegisterID, MIRValue},
};

pub type MIRComptimeBody<'thir> = MIRBody<MIRComptimeInstruction<'thir>>;

#[derive(Debug, Clone)]
pub enum MIRComptimeInstruction<'thir> {
    Runtime(MIRInstruction),
    Comptime {
        op: MIRComptimeOp<'thir>,
        token_range: TokenRange,
    },
}

impl MIRInstructionLike for MIRComptimeInstruction<'_> {
    fn is_terminator(&self) -> bool {
        match self {
            Self::Runtime(instruction) => instruction.is_terminator(),
            Self::Comptime { .. } => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum MIRComptimeOp<'thir> {
    Call {
        out: Option<MIRRegisterID>,
        callee: MIRFunctionID,
        args: Vec<MIRValue>,
    },
    Emit {
        out: MIRRegisterID,
        expression: &'thir THIRExpression,
        parameters: &'thir [THIRStagedParameter],
        captures: BTreeMap<THIRLocalID, MIRValue>,
    },
}
