use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    expr::{MIRBasicBlock, MIRBasicBlockID, MIRConstant, MIRPlace, MIRPlaceID, MIRRegister, MIRScopeID},
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRFunctionMode {
    Runtime,
    Constexpr,
    Comptime,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRGlobalState {
    External,
    ZeroInitialized,
    Initializer(MIRFunctionID),
    Initialized(MIRConstant),
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
    StringLiteral {
        value: String,
    },

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

    pub fn string_literal(id: MIRGlobalID, name: CXIdent, value: String) -> Self {
        Self {
            id,
            name,
            linkage: LinkageMode::Static,
            kind: MIRGlobalKind::StringLiteral { value },
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

    pub return_type: MIRTypeID,
    pub params: Vec<MIRFnParam>,

    pub variadic: bool,
    pub safe: bool,
    pub mode: MIRFunctionMode,
}

impl MIRFnSignature {
    pub fn new(
        symbol_name: CXIdent,
        debug_name: Option<CXIdent>,
        params: Vec<MIRFnParam>,
        return_type: MIRTypeID,
        mode: MIRFunctionMode,
        variadic: bool,
        safe: bool,
    ) -> Self {
        Self {
            symbol_name,
            debug_name,
            params,
            return_type,
            mode,
            variadic,
            safe,
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
    id: MIRFunctionID,
    prototype: MIRFnPrototype,
    body: Option<MIRBody>,
}

impl MIRFunction {
    pub fn new(id: MIRFunctionID, prototype: MIRFnPrototype, definition: Option<MIRBody>) -> Self {
        Self {
            id,
            prototype,
            body: definition,
        }
    }

    pub fn id(&self) -> MIRFunctionID {
        self.id
    }

    pub fn body(&self) -> Option<&MIRBody> {
        self.body.as_ref()
    }

    pub fn prototype(&self) -> &MIRFnPrototype {
        &self.prototype
    }

    pub fn mode(&self) -> MIRFunctionMode {
        self.prototype().signature.mode
    }

    pub fn definition(&self) -> Option<&MIRBody> {
        self.body.as_ref()
    }

    pub fn define(&mut self, def: MIRBody) {
        assert!(
            self.definition().is_none(),
            "Attempt to redefine function: {}",
            self.prototype().signature.display_name()
        );
        self.body = Some(def);
    }
}

#[derive(Debug, Clone)]
pub struct MIRBody {
    entry: MIRBasicBlockID,

    blocks: Vec<MIRBasicBlock>,
    places: Vec<MIRPlaceDecl>,
    registers: Vec<MIRRegisterDecl>,
    scopes: Vec<MIRScopeDecl>,
}

impl MIRBody {
    pub fn new() -> Self {
        Self {
            entry: MIRBasicBlockID::new(0),
            blocks: Vec::new(),
            places: Vec::new(),
            registers: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn entry(&self) -> MIRBasicBlockID {
        self.entry
    }

    pub fn add_block(&mut self) -> MIRBasicBlockID {
        let id = MIRBasicBlockID::new(self.blocks.len());
        self.blocks.push(MIRBasicBlock::new(id));
        id
    }

    pub fn add_block_named(&mut self, debug_name: impl Into<CXIdent>) -> MIRBasicBlockID {
        let id = MIRBasicBlockID::new(self.blocks.len());
        let mut block = MIRBasicBlock::new(id);
        block.debug_name = Some(debug_name.into());
        self.blocks.push(block);
        id
    }

    pub fn add_block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        let register = self.add_register(ty, debug_name);
        self.block_mut(block)
            .expect("block param added to unknown block")
            .params
            .push(register);
        register
    }

    pub fn push_instr_at(
        &mut self,
        block: MIRBasicBlockID,
        kind: crate::MIRInstrKind,
        token_range: TokenRange,
    ) {
        let instr = crate::MIRInstr::new(kind, token_range);
        self.block_mut(block)
            .expect("instruction pushed to unknown block")
            .instrs
            .push(instr);
    }

    pub fn blocks(&self) -> &[MIRBasicBlock] {
        &self.blocks
    }

    pub fn block(&self, id: MIRBasicBlockID) -> Option<&MIRBasicBlock> {
        self.blocks().get(id.index())
    }

    pub fn block_mut(&mut self, id: MIRBasicBlockID) -> Option<&mut MIRBasicBlock> {
        self.blocks.get_mut(id.index())
    }
    
    pub fn places(&self) -> &[MIRPlaceDecl] {
        &self.places
    }

    pub fn place(&self, id: MIRPlaceID) -> Option<&MIRPlaceDecl> {
        self.places().get(id.index())
    }

    pub fn place_mut(&mut self, id: MIRPlaceID) -> Option<&mut MIRPlaceDecl> {
        self.places.get_mut(id.index())
    }

    pub fn scopes(&self) -> &[MIRScopeDecl] {
        &self.scopes
    }

    pub fn scope(&self, id: MIRScopeID) -> Option<&MIRScopeDecl> {
        self.scopes().get(id.index())
    }

    pub fn scope_mut(&mut self, id: MIRScopeID) -> Option<&mut MIRScopeDecl> {
        self.scopes.get_mut(id.index())
    }

    pub fn add_scope(&mut self, token_range: TokenRange) -> MIRScopeID {
        let id = MIRScopeID::new(self.scopes.len());
        self.scopes.push(MIRScopeDecl { id, token_range });
        id
    }

    pub fn add_register(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        let id = MIRRegister::new(self.registers.len());
        self.registers.push(MIRRegisterDecl { id, ty, debug_name });
        id
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

    pub fn registers(&self) -> &[MIRRegisterDecl] {
        &self.registers
    }

    pub fn register(&self, id: MIRRegister) -> Option<&MIRRegisterDecl> {
        self.registers().get(id.index())
    }
}
