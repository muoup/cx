use cx_typechecker_data::mir::{
    expression::MIRExpression,
    program::MIRFunction,
    types::MIRFunctionPrototype,
};

pub(crate) struct MIRBuilder {
    pub generated_functions: Vec<MIRFunction>,
    pub function_context: Option<FunctionContext>,
}

#[derive(Clone, Debug)]
pub(crate) struct Lifetime {
    pub name: String,
    pub _type: cx_typechecker_data::mir::types::MIRType,
}

pub(crate) struct FunctionContext {
    pub current_prototype: MIRFunctionPrototype,
    pub lifetime_stack: Vec<Vec<Lifetime>>,
}

impl MIRBuilder {
    pub fn new() -> Self {
        MIRBuilder {
            generated_functions: Vec::new(),
            function_context: None,
        }
    }

    pub fn start_function(&mut self, prototype: MIRFunctionPrototype) {
        let function_context = FunctionContext {
            current_prototype: prototype,
            lifetime_stack: Vec::new(),
        };

        self.function_context = Some(function_context);
        self.push_scope();
    }

    pub fn lifetime_stack_ref(&self) -> &[Vec<Lifetime>] {
        let Some(func_ctx) = &self.function_context else {
            unreachable!()
        };

        &func_ctx.lifetime_stack
    }

    pub fn add_lifetime(&mut self, lifetime: Lifetime) {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        let current_scope = func_ctx.lifetime_stack.last_mut().unwrap();
        current_scope.push(lifetime);
    }

    pub fn push_scope(&mut self) {
        let Some(func_ctx) = &mut self.function_context else {
            return;
        };

        func_ctx.lifetime_stack.push(Vec::new());
    }

    pub fn pop_scope(&mut self) {
        let Some(func_ctx) = &mut self.function_context else {
            return;
        };

        let _scope = func_ctx.lifetime_stack.pop().unwrap();

        // TODO: Destructor logic moved to MIR→LMIR lowering
        // for lifetime in scope.into_iter().rev() {
        //     acknowledge_destructed_object(self, lifetime);
        // }
    }

    pub fn current_prototype(&self) -> &MIRFunctionPrototype {
        let Some(func_ctx) = &self.function_context else {
            unreachable!()
        };

        &func_ctx.current_prototype
    }

    pub fn finish_function(&mut self, body: MIRExpression) {
        let Some(func_ctx) = self.function_context.take() else {
            unreachable!()
        };

        let mir_function = MIRFunction {
            prototype: func_ctx.current_prototype,
            body,
        };

        self.generated_functions.push(mir_function);
    }
}
