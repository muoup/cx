use std::sync::Arc;

use cx_mir::{MIRConstant, MIRFunctionID, MIRStagedTemplate, MIRValue};

#[derive(Debug, Clone)]
pub enum MIRComptimeValue {
    Constant(MIRConstant),
    Staged(Arc<MIRStagedValue>),
}

impl MIRComptimeValue {
    pub fn constant(self) -> Option<MIRConstant> {
        match self {
            Self::Constant(value) => Some(value),
            Self::Staged(_) => None,
        }
    }
}

impl From<MIRConstant> for MIRComptimeValue {
    fn from(value: MIRConstant) -> Self {
        Self::Constant(value)
    }
}

#[derive(Debug, Clone)]
pub enum MIRStagedBinding {
    Value(MIRValue),
    Comptime(MIRComptimeValue),
}

#[derive(Debug)]
pub struct MIRStagedValue {
    template: Arc<MIRStagedTemplate>,
    captures: Arc<[MIRStagedBinding]>,
    args: Arc<[MIRStagedBinding]>,
    runtime_origin: Option<MIRFunctionID>,
}

impl MIRStagedValue {
    pub fn new(
        template: Arc<MIRStagedTemplate>,
        captures: Vec<MIRStagedBinding>,
        args: Vec<MIRStagedBinding>,
        runtime_origin: Option<MIRFunctionID>,
    ) -> Self {
        Self {
            template,
            captures: captures.into(),
            args: args.into(),
            runtime_origin,
        }
    }

    pub fn template(&self) -> &Arc<MIRStagedTemplate> {
        &self.template
    }

    pub fn captures(&self) -> &[MIRStagedBinding] {
        &self.captures
    }

    pub fn args(&self) -> &[MIRStagedBinding] {
        &self.args
    }

    pub fn runtime_origin(&self) -> Option<MIRFunctionID> {
        self.runtime_origin
    }

    pub fn apply(&self, args: Vec<MIRStagedBinding>) -> Self {
        Self::new(
            self.template.clone(),
            self.captures.to_vec(),
            args,
            self.runtime_origin,
        )
    }
}
