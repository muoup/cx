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
    value::{MIRComptimeOperand, MIRComptimeOutput},
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
            Self::Comptime {
                op: MIRComptimeOp::Return { .. },
                ..
            } => true,
            Self::Comptime { .. } => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum MIRComptimeOp<'thir> {
    Call {
        out: Option<MIRComptimeOutput>,
        callee: MIRFunctionID,
        args: Vec<MIRComptimeOperand>,
    },
    Emit {
        out: crate::value::MIRComptimeRegisterID,
        expression: &'thir THIRExpression,
        parameters: &'thir [THIRStagedParameter],
        captures: BTreeMap<THIRLocalID, MIRComptimeOperand>,
    },
    Return {
        value: Option<MIRComptimeOperand>,
    },
}

pub use crate::value::MIRComptimeParameter;
