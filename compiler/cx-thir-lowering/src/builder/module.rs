use std::collections::{HashMap, HashSet};

use cx_log::{CXResult, catalogue::mir as catalogue};
use cx_mir::{
    MIRComptimeFnPrototype, MIRComptimeFunction, MIRConstant, MIRConstantID, MIRFnPrototype, MIRFunction, MIRFunctionBody, MIRFunctionID, MIRGlobalID, MIRGlobalState, MIRGlobalVariable, global::MIRGlobalKind,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

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

    fn with_used(mut self, used: bool) -> Self {
        self.used = used;
        self
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

pub(crate) struct MIRModuleBuilder {
    functions: HashMap<MIRFunctionID, MIRFunction>,
    comptime_functions: HashMap<MIRFunctionID, MIRComptimeFunction>,
    globals: HashMap<MIRGlobalID, MIRGlobalVariable>,
    
    function_symbols: HashMap<String, ModuleSymbol<MIRFunctionID>>,
    global_symbols: HashMap<String, ModuleSymbol<MIRGlobalID>>,

    global_order: Vec<MIRGlobalID>,
    global_initializer: HashMap<MIRGlobalID, MIRFunctionID>,

    next_string_literal: usize,
    next_function_id: usize,
    next_global_id: usize,
}

pub(crate) struct ModuleParts {
    pub functions: HashMap<MIRFunctionID, MIRFunction>,
    pub comptime_functions: HashMap<MIRFunctionID, MIRFunctionBody>,
    
    pub globals: HashMap<MIRGlobalID, MIRGlobalVariable>,
    pub global_order: Vec<MIRGlobalID>,
    
    pub used_functions: HashSet<MIRFunctionID>,
    pub used_globals: HashSet<MIRGlobalID>,
}

impl MIRModuleBuilder {
    pub(crate) fn new() -> Self {
        Self {
            functions: HashMap::new(),
            comptime_functions: HashMap::new(),
            globals: HashMap::new(),
            function_symbols: HashMap::new(),
            global_symbols: HashMap::new(),
            global_initializer: HashMap::new(),
            global_order: Vec::new(),
            
            next_string_literal: 0,
            next_function_id: 0,
            next_global_id: 0,
        }
    }

    pub(crate) fn declare_function(&mut self, prototype: MIRFnPrototype) -> MIRFunctionID {
        let name = prototype.signature.symbol_name.as_string();
        if let Some(symbol) = self.function_symbols.get(&name) {
            return symbol.id();
        }

        let id = MIRFunctionID::new(self.next_function_id);
        self.next_function_id += 1;
        self.functions
            .insert(id, MIRFunction::new(prototype, None));
        self.function_symbols.insert(name, ModuleSymbol::new(id));
        id
    }

    pub(crate) fn declare_comptime_function(
        &mut self,
        prototype: MIRComptimeFnPrototype,
    ) -> MIRFunctionID {
        let name = prototype.signature().symbol_name.as_string();
        if let Some(symbol) = self.function_symbols.get(&name) {
            return symbol.id();
        }

        let id = MIRFunctionID::new(self.next_function_id);
        self.next_function_id += 1;
        self.comptime_functions.insert(id, MIRComptimeFunction::new(prototype));
        self.function_symbols.insert(name, ModuleSymbol::new(id));
        id
    }

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

    pub(crate) fn declare_global(
        &mut self,
        var: MIRGlobalVariable
    ) -> CXResult<MIRGlobalID> {
        let id = self.allocate_global_id();
        self.globals.insert(id, var);

        Ok(id)
    }

    pub(crate) fn define_function(&mut self, id: MIRFunctionID, def: MIRFunctionBody) {
        let Some(function) = self.functions.get_mut(&id) else {
            unreachable!("Could not define function id: {}", id);
        };

        function.define(def);
    }

    pub(crate) fn function(&self, id: MIRFunctionID) -> Option<&MIRFunction> {
        self.functions.get(&id)
    }

    pub(crate) fn global(&self, id: MIRGlobalID) -> Option<&MIRGlobalVariable> {
        self.globals.get(&id)
    }

    pub(crate) fn global_initializer(&self, id: MIRGlobalID) -> Option<MIRFunctionID> {
        self.global_initializer.get(&id).cloned()
    }

    pub(crate) fn global_symbol(&mut self, name: &str) -> Option<MIRGlobalID> {
        self.global_symbols
            .get_mut(name)
            .map(ModuleSymbol::get)
            .map(|id| *id)
    }

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
        
        let MIRGlobalKind::Variable { state, .. } = &mut global.kind else {
            return Err(mir_error(
                source_range,
                (
                    &catalogue::ENTITY_REQUIREMENT,
                    (
                        format!("global '{name}' initializer"),
                        "a variable global".into(),
                        Some("a string literal global".into()),
                    ),
                ),
            ));
        };

        if matches!(state, MIRGlobalState::Initialized(_)) {
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
        let MIRGlobalKind::Variable { state: s, .. } = &mut global.kind else {
            panic!("global is not a variable");
        };

        *s = state;
    }

    pub(crate) fn into_parts(self) -> ModuleParts {
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
            functions: self.functions,
            comptime_functions: self.comptime_functions,
            globals: self.globals,
            global_order: self.global_order,
        }
    }
}
