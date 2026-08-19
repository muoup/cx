use std::collections::HashMap;

use cx_mir::{
    MIRFnPrototype, MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalState, MIRGlobalVariable,
    MIRTypeRegistryBuilder, MIRUnit,
};
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

#[derive(Debug)]
struct ModuleSymbol<T: Copy> {
    id: T,
    used: bool,
}

impl<T: Copy> ModuleSymbol<T> {
    fn new(id: T) -> Self {
        Self { id, used: false }
    }

    fn get(&mut self) -> T {
        self.used = true;
        self.id
    }

    fn id(&self) -> T {
        self.id
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
    definition_ids: Vec<MIRFunctionID>,
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
            definition_ids: Vec::new(),
            next_function_id: 0,
            next_global_id: 0,
        }
    }

    pub(crate) fn declare_function(
        &mut self,
        prototype: MIRFnPrototype,
        is_definition: bool,
    ) -> MIRFunctionID {
        let name = prototype.signature.symbol_name.to_string();
        if let Some(symbol) = self.function_symbols.get(&name) {
            let id = symbol.id();
            if is_definition && !self.definition_ids.contains(&id) {
                self.definition_ids.push(id);
            }
            return id;
        }

        let id = MIRFunctionID::new(self.next_function_id);
        self.next_function_id += 1;
        self.functions.insert(id, MIRFunction::new(id, prototype));
        self.function_symbols.insert(name, ModuleSymbol::new(id));
        if is_definition {
            self.definition_ids.push(id);
        }
        id
    }

    pub(crate) fn declare_global(
        &mut self,
        name: CXIdent,
        ty: cx_mir::MIRTypeID,
        linkage: LinkageMode,
        is_mutable: bool,
        nodrop: bool,
        state: MIRGlobalState,
    ) -> MIRGlobalID {
        let name_string = name.as_string();
        if let Some(symbol) = self.global_symbols.get(&name_string) {
            return symbol.id();
        }

        let id = MIRGlobalID::new(self.next_global_id);
        self.next_global_id += 1;
        let mut global = MIRGlobalVariable::new(id, name, ty, linkage, is_mutable);
        global.nodrop = nodrop;
        global.state = state;
        self.globals.insert(id, global);
        self.global_symbols
            .insert(name_string, ModuleSymbol::new(id));
        id
    }

    pub(crate) fn function_ids(&self) -> &[MIRFunctionID] {
        &self.definition_ids
    }

    pub(crate) fn take_function(&mut self, id: MIRFunctionID) -> MIRFunction {
        self.functions
            .remove(&id)
            .expect("active MIR function is missing from module state")
    }

    pub(crate) fn insert_function(&mut self, function: MIRFunction) {
        assert!(
            self.functions.insert(function.id, function).is_none(),
            "MIR function was inserted into module state twice"
        );
    }

    pub(crate) fn global(&self, id: MIRGlobalID) -> Option<&MIRGlobalVariable> {
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
        self.globals
            .get_mut(&id)
            .expect("MIR global state update targets a missing global")
            .state = state;
    }

    pub(crate) fn finish(self, types: MIRTypeRegistryBuilder) -> MIRUnit {
        let Self {
            mut functions,
            mut globals,
            function_symbols,
            global_symbols,
            next_function_id,
            next_global_id,
            ..
        } = self;

        for symbol in function_symbols.values() {
            functions
                .get_mut(&symbol.id())
                .expect("builder function symbol points to a missing MIR function")
                .is_used = symbol.is_used();
        }
        for symbol in global_symbols.values() {
            globals
                .get_mut(&symbol.id())
                .expect("builder global symbol points to a missing MIR global")
                .is_used = symbol.is_used();
        }

        MIRUnit::from_parts(
            types,
            dense_functions(functions, next_function_id),
            dense_globals(globals, next_global_id),
        )
    }
}

fn dense_functions(
    functions: HashMap<MIRFunctionID, MIRFunction>,
    length: usize,
) -> Vec<MIRFunction> {
    let mut dense = (0..length)
        .map(|_| None)
        .collect::<Vec<Option<MIRFunction>>>();
    for (id, function) in functions {
        let slot = dense
            .get_mut(id.index())
            .expect("MIR function ID is outside the builder range");
        assert!(slot.is_none(), "MIR function ID was declared twice");
        *slot = Some(function);
    }
    dense
        .into_iter()
        .enumerate()
        .map(|(index, function)| {
            let function = function.expect("MIR function IDs are not dense");
            assert_eq!(function.id.index(), index);
            function
        })
        .collect()
}

fn dense_globals(
    globals: HashMap<MIRGlobalID, MIRGlobalVariable>,
    length: usize,
) -> Vec<MIRGlobalVariable> {
    let mut dense = (0..length)
        .map(|_| None)
        .collect::<Vec<Option<MIRGlobalVariable>>>();
    for (id, global) in globals {
        let slot = dense
            .get_mut(id.index())
            .expect("MIR global ID is outside the builder range");
        assert!(slot.is_none(), "MIR global ID was declared twice");
        *slot = Some(global);
    }
    dense
        .into_iter()
        .enumerate()
        .map(|(index, global)| {
            let global = global.expect("MIR global IDs are not dense");
            assert_eq!(global.id.index(), index);
            global
        })
        .collect()
}
