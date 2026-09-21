use std::collections::HashMap;

use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent, linkage::LinkageMode};

pub mod comptime_function;
pub mod function;

use crate::{
    constant::{MIRConstantID, pool::MIRConstantPool},
    expr::instruction::MIRScopeID,
    ty::{MIRTypeID, registry::MIRTypeRegistry},
    unit::comptime_function::MIRComptimeFunction,
    unit::function::{MIRFunction, MIRFunctionID},
    value::{MIRPlaceID, MIRRegisterID as MIRRegister},
};

dense_id!(MIRGlobalID);
dense_id!(MIRBasicBlockID);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRGlobalState {
    External,
    ZeroInitialized,
    Initialized(MIRConstantID),
}

#[derive(Debug, Clone)]
pub struct MIRUnit<'thir> {
    constants: MIRConstantPool<'thir>,
    types: MIRTypeRegistry,
 
    functions: HashMap<MIRFunctionID, MIRFunction>,
    comptime_functions: HashMap<MIRFunctionID, MIRComptimeFunction<'thir>>,
 
    globals: HashMap<MIRGlobalID, MIRGlobalVariable>,
    global_order: Vec<MIRGlobalID>,
}

impl<'thir> MIRUnit<'thir> {
    pub fn new(
        types: MIRTypeRegistry,
        functions: HashMap<MIRFunctionID, MIRFunction>,
        comptime_functions: HashMap<MIRFunctionID, MIRComptimeFunction<'thir>>,
        constants: MIRConstantPool<'thir>,
        globals: HashMap<MIRGlobalID, MIRGlobalVariable>,
        global_order: Vec<MIRGlobalID>,
    ) -> Self {
        Self {
            types,
            functions,
            comptime_functions,
            constants,
            globals,
            global_order,
        }
    }

    pub fn types(&self) -> &MIRTypeRegistry {
        &self.types
    }

    pub fn functions(&self) -> impl ExactSizeIterator<Item = &MIRFunction> {
        self.functions.values()
    }

    pub fn comptime_functions(&self) -> impl ExactSizeIterator<Item = &MIRComptimeFunction<'thir>> {
        self.comptime_functions.values()
    }

    pub fn constants(&self) -> &MIRConstantPool<'thir> {
        &self.constants
    }

    pub fn globals(&self) -> impl ExactSizeIterator<Item = &MIRGlobalVariable> {
        self.globals.values()
    }

    pub fn global_order(&self) -> &[MIRGlobalID] {
        &self.global_order
    }

    pub fn function(&self, id: MIRFunctionID) -> Option<&MIRFunction> {
        self.functions.get(&id)
    }

    pub fn comptime_function(&self, id: MIRFunctionID) -> Option<&MIRComptimeFunction<'thir>> {
        self.comptime_functions.get(&id)
    }

    pub fn global(&self, id: MIRGlobalID) -> Option<&MIRGlobalVariable> {
        self.globals.get(&id)
    }
}

#[derive(Debug, Clone)]
pub struct MIRPlaceDecl {
    pub id: MIRPlaceID,
    pub ty: MIRTypeID,
    pub debug_name: Option<CXIdent>,
    pub nodrop: bool,
    pub scope: MIRScopeID,
}

#[derive(Debug, Clone)]
pub struct MIRScopeDecl {
    pub id: MIRScopeID,
    pub token_range: TokenRange,
}

#[derive(Debug, Clone)]
pub struct MIRRegisterDecl {
    pub id: MIRRegister,
    pub ty: MIRTypeID,
    pub debug_name: Option<CXIdent>,
}

#[derive(Debug, Clone)]
pub struct MIRGlobalVariable {
    pub id: MIRGlobalID,
    pub name: CXIdent,
    pub linkage: LinkageMode,
    pub kind: MIRGlobalKind,
}

#[derive(Debug, Clone)]
pub enum MIRGlobalKind {
    Variable {
        ty: MIRTypeID,
        state: MIRGlobalState,
        is_mutable: bool,
    },
}

impl MIRGlobalVariable {
    pub fn new(id: MIRGlobalID, name: CXIdent, linkage: LinkageMode, kind: MIRGlobalKind) -> Self {
        Self {
            id,
            name,
            linkage,
            kind,
        }
    }

    pub fn variable(
        id: MIRGlobalID,
        name: CXIdent,
        ty: MIRTypeID,
        linkage: LinkageMode,
        is_mutable: bool,
    ) -> Self {
        Self {
            id,
            name,
            linkage,
            kind: MIRGlobalKind::Variable {
                ty,
                state: if linkage == LinkageMode::Extern {
                    MIRGlobalState::External
                } else {
                    MIRGlobalState::ZeroInitialized
                },
                is_mutable,
            },
        }
    }
}
