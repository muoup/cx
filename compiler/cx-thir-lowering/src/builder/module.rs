use std::collections::{HashMap, HashSet};

use cx_log::{CXResult, catalogue::mir as catalogue};
use cx_mir::{
    MIRBody, MIRComptimeBody, MIRComptimeFnPrototype, MIRComptimeFunction, MIRFnPrototype,
    MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalState, MIRGlobalVariable,
    constant::MIRStagedExprPool,
};
use cx_tokens::TokenRange;

use crate::log::mir_error;

#[derive(Debug)]
struct ModuleSymbol<T: Clone> {
    id: T,
    used: bool,
}

impl<T: Clone> ModuleSymbol<T> {
    fn new(id: T) -> Self {
        Self { id, used: false }
    }

    fn get(&mut self) -> &mut T {
        self.used = true;
        &mut self.id
    }

    fn id(&self) -> T {
        self.id.clone()
    }

    fn is_used(&self) -> bool {
        self.used
    }
}

pub(crate) struct MIRUnitBuilder<'thir> {
    functions: HashMap<MIRFunctionID, MIRFunction>,
    comptime_functions: HashMap<MIRFunctionID, MIRComptimeFunction<'thir>>,
    globals: HashMap<MIRGlobalID, MIRGlobalVariable>,

    staged_expressions: MIRStagedExprPool<'thir>,

    function_symbols: HashMap<String, ModuleSymbol<MIRFunctionID>>,
    global_symbols: HashMap<String, ModuleSymbol<MIRGlobalID>>,

    global_order: Vec<MIRGlobalID>,
    next_function_id: usize,
    next_global_id: usize,
}

pub(crate) struct ModuleParts<'thir> {
    pub functions: HashMap<MIRFunctionID, MIRFunction>,
    pub comptime_functions: HashMap<MIRFunctionID, MIRComptimeFunction<'thir>>,
    pub staged_expressions: MIRStagedExprPool<'thir>,

    pub globals: HashMap<MIRGlobalID, MIRGlobalVariable>,
    pub global_order: Vec<MIRGlobalID>,

    pub used_functions: HashSet<MIRFunctionID>,
    pub used_globals: HashSet<MIRGlobalID>,
}

impl<'thir> MIRUnitBuilder<'thir> {
    pub(crate) fn new() -> Self {
        Self {
            functions: HashMap::new(),
            comptime_functions: HashMap::new(),
            staged_expressions: MIRStagedExprPool::new(),

            globals: HashMap::new(),
            function_symbols: HashMap::new(),

            global_symbols: HashMap::new(),
            global_order: Vec::new(),
            next_function_id: 0,
            next_global_id: 0,
        }
    }

    pub(crate) fn declare_function(&mut self, prototype: MIRFnPrototype) -> MIRFunctionID {
        let name = prototype.symbol_name.as_string();
        if let Some(symbol) = self.function_symbols.get(&name) {
            return symbol.id();
        }

        let id = MIRFunctionID::new(self.next_function_id);
        self.next_function_id += 1;
        self.functions.insert(id, MIRFunction::new(prototype, None));
        self.function_symbols.insert(name, ModuleSymbol::new(id));
        id
    }

    #[allow(dead_code)]
    pub(crate) fn declare_comptime_function(
        &mut self,
        prototype: MIRComptimeFnPrototype,
    ) -> MIRFunctionID {
        let name = prototype.name().as_string();
        if let Some(symbol) = self.function_symbols.get(&name) {
            return symbol.id();
        }

        let id = MIRFunctionID::new(self.next_function_id);
        self.next_function_id += 1;
        self.comptime_functions
            .insert(id, MIRComptimeFunction::new(prototype));
        self.function_symbols.insert(name, ModuleSymbol::new(id));
        id
    }

    #[allow(dead_code)]
    pub(crate) fn allocate_function_id(&mut self) -> MIRFunctionID {
        let id = MIRFunctionID::new(self.next_function_id);
        self.next_function_id += 1;
        id
    }

    pub(crate) fn allocate_global_id(&mut self) -> MIRGlobalID {
        let id = MIRGlobalID::new(self.next_global_id);
        self.next_global_id += 1;
        id
    }

    pub(crate) fn declare_global(&mut self, var: MIRGlobalVariable) -> CXResult<MIRGlobalID> {
        let name = var.name().as_string();
        let id = self.allocate_global_id();
        self.globals.insert(id, var);
        self.global_symbols.insert(name, ModuleSymbol::new(id));
        self.global_order.push(id);

        Ok(id)
    }

    pub(crate) fn define_function(&mut self, id: MIRFunctionID, def: MIRBody) {
        let Some(function) = self.functions.get_mut(&id) else {
            unreachable!("Could not define function id: {}", id);
        };

        function.define(def);
    }

    pub(crate) fn define_comptime_function(
        &mut self,
        id: MIRFunctionID,
        def: MIRComptimeBody<'thir>,
    ) -> CXResult<()> {
        let Some(function) = self.comptime_functions.get_mut(&id) else {
            unreachable!("Could not define comptime function id: {}", id);
        };

        function.set_body(def);

        Ok(())
    }

    pub(crate) fn function(&self, id: MIRFunctionID) -> Option<&MIRFunction> {
        self.functions.get(&id)
    }

    #[allow(dead_code)]
    pub(crate) fn comptime_function(&self, id: MIRFunctionID) -> Option<&MIRComptimeFunction<'_>> {
        self.comptime_functions.get(&id)
    }

    #[allow(dead_code)]
    pub(crate) fn global(&self, id: MIRGlobalID) -> Option<&MIRGlobalVariable> {
        self.globals.get(&id)
    }

    pub(crate) fn global_symbol(&mut self, name: &str) -> Option<MIRGlobalID> {
        self.global_symbols
            .get_mut(name)
            .map(ModuleSymbol::get)
            .map(|id| *id)
    }

    #[allow(dead_code)]
    pub(crate) fn begin_global_initializer(
        &mut self,
        id: MIRGlobalID,
        source_range: &TokenRange,
    ) -> CXResult<()> {
        let global = self
            .globals
            .get_mut(&id)
            .expect("global symbol points to a missing global");
        let name = global.name().clone();

        if matches!(global.state(), MIRGlobalState::Initialized(_)) {
            return Err(mir_error(
                source_range,
                (
                    &catalogue::DUPLICATE_ENTITY,
                    (format!("global '{name}'"), "MIR module".into()),
                ),
            ));
        }

        Ok(())
    }

    pub(crate) fn function_symbol(&mut self, name: &str) -> Option<MIRFunctionID> {
        self.function_symbols
            .get_mut(name)
            .map(ModuleSymbol::get)
            .map(|id| *id)
    }

    pub(crate) fn set_global_state(&mut self, id: MIRGlobalID, state: MIRGlobalState) {
        let global = self
            .globals
            .get_mut(&id)
            .expect("global is missing from module state");

        global.define(state);
    }

    pub(crate) fn into_parts(self) -> ModuleParts<'thir> {
        ModuleParts {
            used_functions: self
                .function_symbols
                .values()
                .filter(|symbol| symbol.is_used())
                .map(|symbol| symbol.id())
                .collect(),
            used_globals: self
                .global_symbols
                .values()
                .filter(|symbol| symbol.is_used())
                .map(|symbol| symbol.id())
                .collect(),
            staged_expressions: self.staged_expressions,
            functions: self.functions,
            comptime_functions: self.comptime_functions,
            globals: self.globals,
            global_order: self.global_order,
        }
    }
}
