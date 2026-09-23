use std::collections::HashMap;

use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent, linkage::LinkageMode};

pub mod comptime_function;
pub mod function;

use crate::{
    constant::{MIRConstant, MIRStagedExprPool},
    expr::instruction::MIRScopeID,
    ty::{MIRTypeID, comptime::MIRComptimeType, registry::MIRTypeRegistry},
    unit::comptime_function::MIRComptimeFunction,
    unit::function::{MIRFunction, MIRFunctionID},
    value::{MIRComptimeRegisterID, MIRPlaceID, MIRRegisterID as MIRRegister},
};

dense_id!(MIRGlobalID, "global.");
dense_id!(MIRBasicBlockID, "bb");

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRGlobalState {
    External,
    ZeroInitialized,
    Initialized(MIRConstant),
}

#[derive(Debug, Clone)]
pub struct MIRUnit<'thir> {
    staged_expr_pool: MIRStagedExprPool<'thir>,
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
        staged_expr_pool: MIRStagedExprPool<'thir>,
        globals: HashMap<MIRGlobalID, MIRGlobalVariable>,
        global_order: Vec<MIRGlobalID>,
    ) -> Self {
        Self {
            types,
            functions,
            comptime_functions,
            staged_expr_pool,
            globals,
            global_order,
        }
    }

    pub fn into_static_runtime_only(self) -> MIRUnit<'static> {
        MIRUnit {
            staged_expr_pool: MIRStagedExprPool::new(),
            types: self.types,
            functions: self.functions,
            comptime_functions: HashMap::new(),
            globals: self.globals,
            global_order: self.global_order,
        }
    }

    pub fn types(&self) -> &MIRTypeRegistry {
        &self.types
    }

    pub fn functions(&self) -> impl ExactSizeIterator<Item = (MIRFunctionID, &MIRFunction)> {
        self.functions.iter().map(|(id, func)| (*id, func))
    }

    pub fn comptime_functions(&self) -> impl ExactSizeIterator<Item = &MIRComptimeFunction<'thir>> {
        self.comptime_functions.values()
    }

    pub fn constants(&self) -> &MIRStagedExprPool<'thir> {
        &self.staged_expr_pool
    }

    pub fn globals(&self) -> impl ExactSizeIterator<Item = (MIRGlobalID, &MIRGlobalVariable)> {
        self.globals.iter().map(|(id, global)| (*id, global))
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
pub struct MIRComptimeRegisterDecl {
    pub id: MIRComptimeRegisterID,
    pub ty: MIRComptimeType,
    pub debug_name: Option<CXIdent>,
}

#[derive(Debug, Clone)]
pub struct MIRGlobalVariable {
    name: CXIdent,
    linkage: LinkageMode,

    ty: MIRTypeID,
    state: MIRGlobalState,
    is_mutable: bool,
}

impl MIRGlobalVariable {
    pub fn new(
        name: CXIdent,
        linkage: LinkageMode,
        ty: MIRTypeID,
        state: MIRGlobalState,
        is_mutable: bool,
    ) -> Self {
        Self {
            name,
            linkage,
            ty,
            state,
            is_mutable,
        }
    }

    pub fn define(&mut self, state: MIRGlobalState) {
        assert!(
            matches!(self.state, MIRGlobalState::External),
            "Attempt to redefine global variable: {}",
            self.name
        );

        self.state = state;
    }

    pub fn name(&self) -> &CXIdent {
        &self.name
    }

    pub fn linkage(&self) -> LinkageMode {
        self.linkage
    }

    pub fn ty(&self) -> MIRTypeID {
        self.ty
    }

    pub fn state(&self) -> &MIRGlobalState {
        &self.state
    }

    pub fn is_mutable(&self) -> bool {
        self.is_mutable
    }
}
