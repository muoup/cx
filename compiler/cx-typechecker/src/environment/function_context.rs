use cx_mir::mir::data::MIRFunctionPrototype;

#[derive(Default)]
pub struct FunctionContext {
    current_function: Option<MIRFunctionPrototype>,
    safe_mode: bool,
    contract_pure_mode: bool,
    unsafe_depth: usize,
}

#[derive(Clone)]
pub struct FunctionModeSnapshot {
    safe_mode: bool,
    contract_pure_mode: bool,
    unsafe_depth: usize,
}

impl FunctionContext {
    pub fn begin_function(&mut self, prototype: MIRFunctionPrototype) {
        self.safe_mode = prototype.contract.safe;
        self.contract_pure_mode = false;
        self.unsafe_depth = 0;
        self.current_function = Some(prototype);
    }

    pub fn end_function(&mut self) {
        self.current_function = None;
        self.safe_mode = false;
        self.contract_pure_mode = false;
        self.unsafe_depth = 0;
    }

    pub fn current_function(&self) -> &MIRFunctionPrototype {
        self.current_function.as_ref().unwrap()
    }

    pub fn in_safe_context(&self) -> bool {
        self.safe_mode && self.unsafe_depth == 0
    }

    pub fn enter_unsafe(&mut self) {
        self.unsafe_depth += 1;
    }

    pub fn exit_unsafe(&mut self) {
        self.unsafe_depth -= 1;
    }

    pub fn snapshot_mode(&self) -> FunctionModeSnapshot {
        FunctionModeSnapshot {
            safe_mode: self.safe_mode,
            contract_pure_mode: self.contract_pure_mode,
            unsafe_depth: self.unsafe_depth,
        }
    }

    pub fn set_contract_mode(&mut self, safe: bool) {
        self.safe_mode = safe;
        self.contract_pure_mode = safe;
        self.unsafe_depth = 0;
    }

    pub fn restore_mode(&mut self, snapshot: FunctionModeSnapshot) {
        self.safe_mode = snapshot.safe_mode;
        self.contract_pure_mode = snapshot.contract_pure_mode;
        self.unsafe_depth = snapshot.unsafe_depth;
    }
}
