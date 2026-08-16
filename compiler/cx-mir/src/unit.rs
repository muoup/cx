use cx_target::ArchitectureConfig;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    expr::{MIRBasicBlockID, MIRScopeID},
    global::{
        MIRFnPrototype, MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalState, MIRGlobalVariable,
    },
    ty::{MIRTypeID, MIRTypeRegistry},
    validator::MIRValidationError,
};

#[derive(Debug, Clone)]
pub struct MIRUnit {
    pub types: MIRTypeRegistry,
    pub functions: Vec<MIRFunction>,
    pub globals: Vec<MIRGlobalVariable>,
}

impl MIRUnit {
    pub fn new(architecture: ArchitectureConfig) -> Self {
        Self {
            types: MIRTypeRegistry::new(architecture),
            functions: Vec::new(),
            globals: Vec::new(),
        }
    }

    pub fn compute_layouts(&mut self) -> Result<(), crate::MIRLayoutError> {
        self.types.compute_layouts()
    }

    pub fn add_function(&mut self, prototype: MIRFnPrototype) -> MIRFunctionID {
        let id = MIRFunctionID::new(self.functions.len());
        self.functions.push(MIRFunction::new(id, prototype));
        id
    }

    /// Inserts an already-built function and assigns its canonical dense ID.
    pub fn push_function(&mut self, mut function: MIRFunction) -> MIRFunctionID {
        let id = MIRFunctionID::new(self.functions.len());
        function.id = id;
        self.functions.push(function);
        id
    }

    pub fn add_global(
        &mut self,
        name: CXIdent,
        ty: MIRTypeID,
        linkage: LinkageMode,
        is_mutable: bool,
        nodrop: bool,
        state: MIRGlobalState,
    ) -> MIRGlobalID {
        let id = MIRGlobalID::new(self.globals.len());
        let mut global = MIRGlobalVariable::new(id, name, ty, linkage, is_mutable);
        global.nodrop = nodrop;
        global.state = state;
        self.globals.push(global);
        id
    }

    pub fn push_global(&mut self, mut global: MIRGlobalVariable) -> MIRGlobalID {
        let id = MIRGlobalID::new(self.globals.len());
        global.id = id;
        self.globals.push(global);
        id
    }

    pub fn function(&self, id: MIRFunctionID) -> Option<&MIRFunction> {
        self.functions.get(id.index())
    }

    pub fn function_mut(&mut self, id: MIRFunctionID) -> Option<&mut MIRFunction> {
        self.functions.get_mut(id.index())
    }

    pub fn global(&self, id: MIRGlobalID) -> Option<&MIRGlobalVariable> {
        self.globals.get(id.index())
    }

    pub fn global_mut(&mut self, id: MIRGlobalID) -> Option<&mut MIRGlobalVariable> {
        self.globals.get_mut(id.index())
    }

    pub fn validation_error_range(&self, error: &MIRValidationError) -> Option<&TokenRange> {
        let (function, block, instruction) = error.instruction_location()?;
        self.instruction_range(function, block, instruction)
    }

    pub fn instruction_range(
        &self,
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        instruction: usize,
    ) -> Option<&TokenRange> {
        self.function(function)
            .and_then(|function| function.block(block))
            .and_then(|block| block.instrs.get(instruction))
            .map(|instruction| &instruction.token_range)
    }

    pub fn scope_range(&self, function: MIRFunctionID, scope: MIRScopeID) -> Option<&TokenRange> {
        self.function(function)
            .and_then(|function| function.scope(scope))
            .map(|scope| &scope.token_range)
    }
}
