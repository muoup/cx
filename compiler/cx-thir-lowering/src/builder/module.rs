use std::collections::{HashMap, HashSet};

use cx_log::{
    CXResult,
    error::{CXErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_mir::{
    MIRBody, MIRFnPrototype, MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalState,
    MIRGlobalVariable, global::MIRGlobalKind,
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
    globals: HashMap<MIRGlobalID, MIRGlobalVariable>,

    function_symbols: HashMap<String, ModuleSymbol<MIRFunctionID>>,
    global_symbols: HashMap<String, ModuleSymbol<MIRGlobalID>>,

    global_order: Vec<MIRGlobalID>,
    
    next_string_literal: usize,
    next_function_id: usize,
    next_global_id: usize,
}

pub(crate) struct ModuleParts {
    pub functions: HashMap<MIRFunctionID, MIRFunction>,
    pub globals: HashMap<MIRGlobalID, MIRGlobalVariable>,
    pub global_order: Vec<MIRGlobalID>,
    pub used_functions: HashSet<MIRFunctionID>,
    pub used_globals: HashSet<MIRGlobalID>,
}

impl MIRModuleBuilder {
    pub(crate) fn new() -> Self {
        Self {
            functions: HashMap::new(),
            globals: HashMap::new(),
            function_symbols: HashMap::new(),
            global_symbols: HashMap::new(),
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
            .insert(id, MIRFunction::new(id, prototype, None));
        self.function_symbols.insert(name, ModuleSymbol::new(id));
        id
    }

    pub(crate) fn allocate_function_id(&mut self) -> MIRFunctionID {
        let id = MIRFunctionID::new(self.next_function_id);
        self.next_function_id += 1;
        id
    }

    pub(crate) fn declare_global(
        &mut self,
        pre_used: bool,
        name: CXIdent,
        linkage: LinkageMode,
        kind: MIRGlobalKind,
    ) -> CXResult<MIRGlobalID> {
        let name_string = name.as_string();
        if let Some(id) = self.global_symbols.get(&name_string).map(ModuleSymbol::id) {
            let compatible = match (self.globals.get(&id).map(|global| &global.kind), &kind) {
                (
                    Some(MIRGlobalKind::Variable {
                        ty: existing_ty, ..
                    }),
                    MIRGlobalKind::Variable {
                        ty: incoming_ty, ..
                    },
                ) => existing_ty == incoming_ty,
                _ => false,
            };
            if !compatible {
                return Err(CXErr::new(
                    CXStdErrMessage::error(
                        "TYPE ERROR",
                        format!("Incompatible global declaration '{name}'"),
                    ),
                    CXInternalContext::error("incompatible global declaration during MIR lowering"),
                ));
            }

            let existing_is_external = self.globals.get(&id).is_some_and(|global| {
                matches!(
                    &global.kind,
                    MIRGlobalKind::Variable {
                        state: MIRGlobalState::External,
                        ..
                    }
                )
            });
            let incoming_is_external = matches!(
                &kind,
                MIRGlobalKind::Variable {
                    state: MIRGlobalState::External,
                    ..
                }
            );

            if existing_is_external && !incoming_is_external {
                let global = self
                    .globals
                    .get_mut(&id)
                    .expect("global symbol points to a missing global");
                global.linkage = linkage;
                global.kind = kind;
            }
            if pre_used && let Some(symbol) = self.global_symbols.get_mut(&name_string) {
                symbol.used = true;
            }
            return Ok(id);
        }

        let id = MIRGlobalID::new(self.next_global_id);
        self.next_global_id += 1;
        let global = MIRGlobalVariable::new(id, name, linkage, kind);
        self.globals.insert(id, global);
        self.global_order.push(id);
        self.global_symbols
            .insert(name_string, ModuleSymbol::new(id).with_used(pre_used));
        Ok(id)
    }

    pub(crate) fn add_string_literal(&mut self, value: &str) -> CXResult<MIRGlobalID> {
        let name = CXIdent::from(format!("__anon_str_{}", self.next_string_literal));
        self.next_string_literal += 1;
        self.declare_global(
            true,
            name,
            LinkageMode::Static,
            MIRGlobalKind::StringLiteral {
                value: value.to_owned(),
            },
        )
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

    pub(crate) fn global_symbol(&mut self, name: &str) -> Option<MIRGlobalID> {
        self.global_symbols
            .get_mut(name)
            .map(ModuleSymbol::get)
            .map(|id| *id)
    }

    pub(crate) fn begin_global_initializer(
        &mut self,
        id: MIRGlobalID,
        init_id: MIRFunctionID,
    ) -> CXResult<()> {
        let global = self
            .globals
            .get_mut(&id)
            .expect("global symbol points to a missing global");
        let name = global.name.clone();
        let MIRGlobalKind::Variable { state, .. } = &mut global.kind else {
            return Err(CXErr::new(
                CXStdErrMessage::error(
                    "TYPE ERROR",
                    format!("Global '{name}' cannot have an initializer"),
                ),
                CXInternalContext::error("non-variable global initializer during MIR lowering"),
            ));
        };

        if matches!(
            state,
            MIRGlobalState::Initializer(_) | MIRGlobalState::Initialized(_)
        ) {
            return Err(CXErr::new(
                CXStdErrMessage::error(
                    "TYPE ERROR",
                    format!("Duplicate global definition '{name}'"),
                ),
                CXInternalContext::error("duplicate global initializer during MIR lowering"),
            ));
        }

        *state = MIRGlobalState::Initializer(init_id);
        Ok(())
    }

    pub(crate) fn function_symbol(&mut self, name: &str) -> Option<MIRFunctionID> {
        self.function_symbols.get_mut(name)
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
            globals: self.globals,
            global_order: self.global_order,
        }
    }
}
