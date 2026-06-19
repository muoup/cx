use cx_mir::mir::data::MIRFunctionPrototype;

use crate::environment::TypeEnvironment;

pub struct ComptimeEngine<'env, 'data> {
    env: &'env mut TypeEnvironment<'data>,
    #[allow(dead_code)]
    current_prototype: Option<MIRFunctionPrototype>,
}

impl<'env, 'data> ComptimeEngine<'env, 'data> {
    pub fn new(env: &'env mut TypeEnvironment<'data>) -> Self {
        Self {
            current_prototype: env.try_current_function().cloned(),
            env,
        }
    }

    pub fn env(&self) -> &TypeEnvironment<'data> {
        self.env
    }

    #[allow(dead_code)]
    pub fn env_mut(&mut self) -> &mut TypeEnvironment<'data> {
        self.env
    }

    #[allow(dead_code)]
    pub fn current_fn_prototype(&self) -> Option<&MIRFunctionPrototype> {
        self.current_prototype.as_ref()
    }
}
