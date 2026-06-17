use std::path::Path;

use cx_mir::mir::data::MIRFunctionPrototype;

use crate::builder::LMIRBuilder;

pub struct ComptimeEngine<'builder> {
    builder: &'builder mut LMIRBuilder,
    current_prototype: MIRFunctionPrototype,
}

impl<'builder> ComptimeEngine<'builder> {
    pub fn new(
        builder: &'builder mut LMIRBuilder,
        current_prototype: MIRFunctionPrototype,
    ) -> Self {
        Self {
            builder,
            current_prototype,
        }
    }

    pub(crate) fn source_path(&self) -> &Path {
        self.builder.source_path()
    }
}
