use std::collections::HashMap;

use cx_mir::{
    MIRFnPrototype, MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalState, MIRGlobalVariable,
    MIRTypeRegistryBuilder, MIRUnit, global::MIRGlobalKind,
};
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

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

    fn get(&mut self) -> T {
        self.used = true;
        self.id.clone()
    }

    fn id(&self) -> T {
        self.id.clone()
    }

    fn is_used(&self) -> bool {
        self.used
    }
}

pub(crate) struct MIRModuleState {
    functions: HashMap<MIRFunctionID, MIRFunction>,
    globals: HashMap<MIRGlobalID, MIRGlobalVariable>,

    function_symbols: HashMap<String, ModuleSymbol<MIRFunctionID>>,
    global_symbols: HashMap<String, ModuleSymbol<MIRGlobalID>>,
    function_ids: Vec<MIRFunctionID>,

    next_function_id: usize,
    next_global_id: usize,
}

impl MIRModuleState {
    pub(crate) fn new() -> Self {
        Self {
            functions: HashMap::new(),
            globals: HashMap::new(),
            function_symbols: HashMap::new(),
            global_symbols: HashMap::new(),
            function_ids: Vec::new(),
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
            .insert(id, MIRFunction::declaration(id, prototype));
        self.function_symbols.insert(name, ModuleSymbol::new(id));
        self.function_ids.push(id);
        id
    }

    pub(crate) fn declare_global(
        &mut self,
        name: CXIdent,
        linkage: LinkageMode,
        kind: MIRGlobalKind,
        pre_used: bool,
    ) -> MIRGlobalID {
        let name_string = name.as_string();
        if let Some(symbol) = self.global_symbols.get(&name_string) {
            return symbol.id();
        }

        let id = MIRGlobalID::new(self.next_global_id);
        self.next_global_id += 1;
        let global = MIRGlobalVariable::new(id, name, linkage, kind);
        self.globals.insert(id, global);
        self.global_symbols
            .insert(name_string, ModuleSymbol::new(id).with_used(pre_used));
        id
    }

    pub(crate) fn function_ids(&self) -> &[MIRFunctionID] {
        &self.function_ids
    }

    pub(crate) fn take_function(&mut self, id: MIRFunctionID) -> MIRFunction {
        self.functions
            .remove(&id)
            .expect("active MIR function is missing from module state")
    }

    pub(crate) fn insert_function(&mut self, function: MIRFunction) {
        let id = function.id();
        assert!(
            self.functions.insert(id, function).is_none(),
            "MIR function was inserted into module state twice"
        );
    }

    pub(crate) fn _global(&self, id: MIRGlobalID) -> Option<&MIRGlobalVariable> {
        self.globals.get(&id)
    }

    pub(crate) fn global_id(&self, name: &str) -> Option<MIRGlobalID> {
        self.global_symbols.get(name).map(ModuleSymbol::id)
    }

    pub(crate) fn function_symbol(&mut self, name: &str) -> Option<MIRFunctionID> {
        self.function_symbols.get_mut(name).map(ModuleSymbol::get)
    }

    pub(crate) fn global_symbol(&mut self, name: &str) -> Option<MIRGlobalID> {
        self.global_symbols.get_mut(name).map(ModuleSymbol::get)
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

    pub(crate) fn finish(self, types: MIRTypeRegistryBuilder) -> MIRUnit {
        let Self {
            functions,
            globals,
            function_symbols,
            global_symbols,
            function_ids: _,
            ..
        } = self;

        let functions = functions
            .into_iter()
            .filter_map(|(_, function)| {
                if function.prototype().linkage == LinkageMode::Standard {
                    return Some((function.id(), function));
                }

                let name = &function.prototype().signature.symbol_name;
                let symbol = function_symbols
                    .get(&name.as_string())
                    .expect("function symbol missing");

                if symbol.is_used() {
                    Some((function.id(), function))
                } else {
                    None
                }
            })
            .collect();
        let globals = globals
            .into_iter()
            .filter_map(|(_, global)| {
                if global.linkage == LinkageMode::Standard {
                    return Some((global.id, global));
                }

                let name = &global.name;
                let symbol = global_symbols
                    .get(&name.as_string())
                    .expect("global symbol missing");

                if symbol.is_used() {
                    Some((global.id, global))
                } else {
                    None
                }
            })
            .collect();

        MIRUnit::new(types, functions, globals)
    }
}
