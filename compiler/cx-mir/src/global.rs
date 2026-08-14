use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    expr::{
        MIRBasicBlock, MIRBasicBlockID, MIRConstant, MIRInstr, MIRInstrKind, MIRPlace, MIRPlaceID,
        MIRRegister, MIRScopeID,
    },
    ty::MIRTypeID,
};

macro_rules! dense_id {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(pub usize);

        impl $name {
            pub const fn new(index: usize) -> Self {
                Self(index)
            }

            pub const fn index(self) -> usize {
                self.0
            }
        }
    };
}

dense_id!(MIRFunctionID);
dense_id!(MIRGlobalID);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRGlobalState {
    External,
    ZeroInitialized,
    Initialized(MIRConstant),
}

#[derive(Debug, Clone)]
pub struct MIRGlobalVariable {
    pub id: MIRGlobalID,
    pub name: CXIdent,
    pub ty: MIRTypeID,
    pub linkage: LinkageMode,
    pub state: MIRGlobalState,
    pub is_mutable: bool,
    pub nodrop: bool,
}

impl MIRGlobalVariable {
    pub fn new(
        id: MIRGlobalID,
        name: CXIdent,
        ty: MIRTypeID,
        linkage: LinkageMode,
        is_mutable: bool,
    ) -> Self {
        Self {
            id,
            name,
            ty,
            linkage,
            state: if linkage == LinkageMode::Extern {
                MIRGlobalState::External
            } else {
                MIRGlobalState::ZeroInitialized
            },
            is_mutable,
            nodrop: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MIRFnParam {
    pub name: Option<CXIdent>,
    pub ty: MIRTypeID,
    pub nodrop: bool,
}

impl MIRFnParam {
    pub fn new(ty: MIRTypeID) -> Self {
        Self {
            name: None,
            ty,
            nodrop: false,
        }
    }

    pub fn named(name: CXIdent, ty: MIRTypeID) -> Self {
        Self {
            name: Some(name),
            ty,
            nodrop: false,
        }
    }

    pub fn with_nodrop(mut self, nodrop: bool) -> Self {
        self.nodrop = nodrop;
        self
    }
}

#[derive(Debug, Clone)]
pub struct MIRFnSignature {
    pub symbol_name: CXIdent,
    pub debug_name: Option<CXIdent>,
    pub params: Vec<MIRFnParam>,
    pub return_type: MIRTypeID,
    pub variadic: bool,
    pub safe: bool,
}

impl MIRFnSignature {
    pub fn new(symbol_name: CXIdent, params: Vec<MIRFnParam>, return_type: MIRTypeID) -> Self {
        Self {
            symbol_name,
            debug_name: None,
            params,
            return_type,
            variadic: false,
            safe: false,
        }
    }

    pub fn display_name(&self) -> &CXIdent {
        self.debug_name.as_ref().unwrap_or(&self.symbol_name)
    }
}

#[derive(Debug, Clone)]
pub struct MIRFnPrototype {
    pub signature: MIRFnSignature,
    pub linkage: LinkageMode,
}

impl MIRFnPrototype {
    pub fn new(signature: MIRFnSignature, linkage: LinkageMode) -> Self {
        Self { signature, linkage }
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
pub struct MIRFunction {
    pub id: MIRFunctionID,
    pub prototype: MIRFnPrototype,
    pub entry: Option<MIRBasicBlockID>,
    pub blocks: Vec<MIRBasicBlock>,
    pub places: Vec<MIRPlaceDecl>,
    pub registers: Vec<MIRRegisterDecl>,
    pub scopes: Vec<MIRScopeDecl>,
}

impl MIRFunction {
    pub fn new(id: MIRFunctionID, prototype: MIRFnPrototype) -> Self {
        Self {
            id,
            prototype,
            entry: None,
            blocks: Vec::new(),
            places: Vec::new(),
            registers: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn add_scope(&mut self, token_range: TokenRange) -> MIRScopeID {
        let id = MIRScopeID::new(self.scopes.len());
        self.scopes.push(MIRScopeDecl { id, token_range });
        id
    }

    pub fn is_declaration(&self) -> bool {
        self.blocks.is_empty()
    }

    pub fn add_place(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        nodrop: bool,
        scope: MIRScopeID,
    ) -> MIRPlace {
        let id = MIRPlaceID::new(self.places.len());
        self.places.push(MIRPlaceDecl {
            id,
            ty,
            debug_name,
            nodrop,
            scope,
        });
        MIRPlace::FunctionLocal(id)
    }

    pub fn add_register(&mut self, ty: MIRTypeID, debug_name: Option<CXIdent>) -> MIRRegister {
        let id = MIRRegister::new(self.registers.len());
        self.registers.push(MIRRegisterDecl { id, ty, debug_name });
        id
    }

    pub fn add_block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
    ) -> Option<MIRRegister> {
        if self.block(block).is_none() {
            return None;
        }
        let register = self.add_register(ty, debug_name);
        self.block_mut(block)?.params.push(register);
        Some(register)
    }

    pub fn add_block(&mut self) -> MIRBasicBlockID {
        let id = MIRBasicBlockID::new(self.blocks.len());
        self.blocks.push(MIRBasicBlock::new(id));
        if self.entry.is_none() {
            self.entry = Some(id);
        }
        id
    }

    pub fn block(&self, id: MIRBasicBlockID) -> Option<&MIRBasicBlock> {
        self.blocks.get(id.index())
    }

    pub fn block_mut(&mut self, id: MIRBasicBlockID) -> Option<&mut MIRBasicBlock> {
        self.blocks.get_mut(id.index())
    }

    pub fn place(&self, id: MIRPlaceID) -> Option<&MIRPlaceDecl> {
        self.places.get(id.index())
    }

    pub fn place_mut(&mut self, id: MIRPlaceID) -> Option<&mut MIRPlaceDecl> {
        self.places.get_mut(id.index())
    }

    pub fn scope(&self, id: MIRScopeID) -> Option<&MIRScopeDecl> {
        self.scopes.get(id.index())
    }

    pub fn register(&self, id: MIRRegister) -> Option<&MIRRegisterDecl> {
        self.registers.get(id.index())
    }

    pub fn push_instr(
        &mut self,
        block: MIRBasicBlockID,
        kind: MIRInstrKind,
    ) -> Option<&mut MIRInstr> {
        self.push_instr_at(block, kind, TokenRange::internal())
    }

    pub fn push_instr_at(
        &mut self,
        block: MIRBasicBlockID,
        kind: MIRInstrKind,
        token_range: TokenRange,
    ) -> Option<&mut MIRInstr> {
        let block = self.block_mut(block)?;
        block.instrs.push(MIRInstr::new_at(kind, token_range));
        block.instrs.last_mut()
    }
}
