use cx_mir::mir::data::MIRFunctionPrototype;

use crate::environment::TypeEnvironment;

pub struct ComptimeEngine<'builder> {
    pub(crate) env: &'builder mut TypeEnvironment<'builder>,
    current_prototype: MIRFunctionPrototype,
}

impl<'builder> ComptimeEngine<'builder> {
    pub fn new(
        builder: &'builder mut TypeEnvironment<'builder>,
        current_prototype: MIRFunctionPrototype,
    ) -> Self {
        Self {
            env: builder,
            current_prototype,
        }
    }
}
