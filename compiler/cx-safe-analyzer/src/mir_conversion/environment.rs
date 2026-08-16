use cx_mir::mir::types::MIRFunctionPrototype;
use cx_safe_ir::ast::{FMIRNode, FMIRType, MemoryLocation};
use cx_util::{identifier::CXIdent, scoped_map::ScopedMap};

/// Represents a variable in a safe context. Safe contexts necessitate stricter
/// aliasing and lifetime rules, giving a variable a stricter definition.
///
/// For now, safe contexts do not allow mutual region aliasing, meaning reads/writes
/// on a variable are unique and must not intefere with other variables in the same
/// function context.
#[derive(Clone)]
pub struct VariableIdentifier {
    pub depth: usize,
    pub name: CXIdent,
    pub _type: FMIRType,
    pub location: MemoryLocation,
    pub known_value: Option<FMIRNode>,
}

pub(crate) struct FMIREnvironment {
    current_mir_prototype: Option<MIRFunctionPrototype>,
    region_table: ScopedMap<VariableIdentifier>,
}

impl FMIREnvironment {
    pub fn new() -> Self {
        Self {
            current_mir_prototype: None,
            region_table: ScopedMap::new_with_starting_scope(),
        }
    }

    pub fn begin_function(&mut self, prototype: MIRFunctionPrototype) {
        self.region_table = ScopedMap::new_with_starting_scope();
        self.current_mir_prototype = Some(prototype);

        let params = self.current_mir_prototype().params.clone();
        for param in params.iter() {
            let Some(name) = param.name.clone() else {
                continue;
            };

            self.insert_variable(
                name.clone(),
                FMIRType::pure(param._type.clone()),
                MemoryLocation::Parameter(name.as_string()),
                None,
            );
        }
    }

    pub fn current_mir_prototype(&self) -> &MIRFunctionPrototype {
        self.current_mir_prototype
            .as_ref()
            .expect("Current MIR function prototype not set in FMIR environment")
    }

    pub fn push_scope(&mut self) {
        self.region_table.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.region_table.pop_scope();
    }

    pub fn insert_variable(
        &mut self,
        name: CXIdent,
        _type: FMIRType,
        location: MemoryLocation,
        known_value: Option<FMIRNode>,
    ) {
        self.region_table.insert(
            name.to_string(),
            VariableIdentifier {
                depth: self.region_table.scope_depth(),
                name,
                _type,
                location,
                known_value,
            },
        );
    }

    pub fn set_known_value(&mut self, name: &CXIdent, value: Option<FMIRNode>) {
        let Some(variable) = self.query_variable(name).cloned() else {
            return;
        };

        self.insert_variable(
            variable.name.clone(),
            variable._type.clone(),
            variable.location.clone(),
            value,
        );
    }

    pub fn query_known_value(&self, name: &CXIdent) -> Option<FMIRNode> {
        self.query_variable(name)
            .and_then(|identifier| identifier.known_value.clone())
    }

    pub fn query_memory_location(&self, name: &CXIdent) -> Option<MemoryLocation> {
        self.query_variable(name)
            .map(|identifier| identifier.location.clone())
    }

    pub fn query_variable(&self, name: &CXIdent) -> Option<&VariableIdentifier> {
        self.region_table.get(name.as_str())
    }
}
