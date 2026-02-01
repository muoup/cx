use cx_mir::mir::types::MIRFunctionPrototype;
use cx_safe_ir::ast::FMIRType;
use cx_util::{identifier::CXIdent, scoped_map::ScopedMap};

/// Represents a variable in a safe context. Safe contexts necessitate stricter
/// aliasing and lifetime rules, giving a variable a stricter definition.
///
/// For now, safe contexts do not allow mutual region aliasing, meaning reads/writes
/// on a variable are unique and must not intefere with other variables in the same
/// function context.
pub struct VariableIdentifier {
    depth: usize,
    name: CXIdent,
    _type: FMIRType,
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
    
    pub fn set_current_mir_prototype(&mut self, prototype: MIRFunctionPrototype) {
        self.current_mir_prototype = Some(prototype);
    }
    
    pub fn current_mir_prototype(&self) -> &MIRFunctionPrototype {
        self.current_mir_prototype.as_ref()
            .expect("Current MIR function prototype not set in FMIR environment")
    }
    
    pub fn push_scope(&mut self) {
        self.region_table.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.region_table.pop_scope();
    }

    pub fn insert_variable(&mut self, name: CXIdent, _type: FMIRType) {
        self.region_table.insert(
            name.to_string(),
            VariableIdentifier {
                depth: self.region_table.scope_depth(),
                name,
                _type,
            },
        );
    }
    
    pub fn query_variable(&self, name: &CXIdent) -> Option<&VariableIdentifier> {
        self.region_table.get(name.as_str())
    }
}
