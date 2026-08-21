use std::collections::HashMap;

use cx_mir::{
    MIRBody, MIRFnPrototype, MIRFunction, MIRFunctionID, MIRFunctionMode, MIRGlobalID,
    MIRGlobalState, MIRGlobalVariable, MIRTypeID, MIRTypeRegistryBuilder, MIRUnit,
    global::MIRGlobalKind,
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

pub(crate) struct MIRModuleBuilder {
    functions: HashMap<MIRFunctionID, MIRFunction>,
    globals: HashMap<MIRGlobalID, MIRGlobalVariable>,

    function_symbols: HashMap<String, ModuleSymbol<MIRFunctionID>>,
    global_symbols: HashMap<String, ModuleSymbol<MIRGlobalID>>,

    next_function_id: usize,
    next_global_id: usize,
}

impl MIRModuleBuilder {
    pub(crate) fn new() -> Self {
        Self {
            functions: HashMap::new(),
            globals: HashMap::new(),
            function_symbols: HashMap::new(),
            global_symbols: HashMap::new(),
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
            .insert(id, MIRFunction::new(id, prototype, None));
        self.function_symbols.insert(name, ModuleSymbol::new(id));
        self.function_ids.push(id);
        id
    }

    pub(crate) fn declare_global(
        &mut self,
        pre_used: bool,
        name: CXIdent,
        linkage: LinkageMode,
        kind: MIRGlobalKind,
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
        self.global_ids.push(id);
        id
    }

    pub(crate) fn define_function(&mut self, id: MIRFunctionID, def: MIRBody) {
        let Some(function) = self.functions.get_mut(&id) else {
            unreachable!("Could not define function id: {}", id);
        };

        function.define(def);
    }

    pub(crate) fn function(&self, id: MIRFunctionID) -> Option<&MIRFunction> {
        self.functions.get(&id)
    }

    pub(crate) fn global_id(&self, name: &str) -> Option<MIRGlobalID> {
        self.global_symbols.get(name).map(ModuleSymbol::id)
    }

    pub(crate) fn function_symbol(&mut self, name: &str) -> Option<MIRFunctionID> {
        self.function_symbols.get_mut(name).map(ModuleSymbol::get)
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
}
