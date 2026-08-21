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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRFunctionMode {
    Runtime,
    Comptime,
    ComptimeOnly,
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
        is_nodrop: bool,
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
                is_nodrop: false,
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
    id: MIRFunctionID,
    prototype: MIRFnPrototype,
    mode: MIRFunctionMode,
    definition: Option<MIRFunctionDefinition>,
}

#[derive(Debug, Clone)]
pub struct MIRFunctionDefinition {
    entry: Option<MIRBasicBlockID>,
    blocks: Vec<MIRBasicBlock>,
    places: Vec<MIRPlaceDecl>,
    registers: Vec<MIRRegisterDecl>,
    scopes: Vec<MIRScopeDecl>,
}

impl MIRFunction {
    pub fn declaration(
        id: MIRFunctionID,
        prototype: MIRFnPrototype,
        mode: MIRFunctionMode,
    ) -> Self {
        Self {
            id,
            prototype,
            mode,
            definition: None,
        }
    }

    pub fn defined(
        id: MIRFunctionID,
        prototype: MIRFnPrototype,
        definition: MIRFunctionDefinition,
        mode: MIRFunctionMode,
    ) -> Self {
        Self {
            id,
            prototype,
            mode,
            definition: Some(definition),
        }
    }

    pub fn id(&self) -> MIRFunctionID {
        self.id
    }

    pub fn prototype(&self) -> &MIRFnPrototype {
        &self.prototype
    }

    pub fn mode(&self) -> MIRFunctionMode {
        self.mode
    }

    pub fn definition(&self) -> Option<&MIRFunctionDefinition> {
        self.definition.as_ref()
    }

    pub fn into_definition(self) -> (MIRFunctionID, MIRFnPrototype, MIRFunctionDefinition) {
        let (id, prototype, definition, _) = self.into_definition_with_mode();
        (id, prototype, definition)
    }

    pub fn into_definition_with_mode(
        self,
    ) -> (
        MIRFunctionID,
        MIRFnPrototype,
        MIRFunctionDefinition,
        MIRFunctionMode,
    ) {
        let Self {
            id,
            prototype,
            mode,
            definition,
        } = self;
        (
            id,
            prototype,
            definition.unwrap_or_else(MIRFunctionDefinition::new),
            mode,
        )
    }
}

impl MIRFunctionDefinition {
    pub fn new() -> Self {
        Self {
            entry: None,
            blocks: Vec::new(),
            places: Vec::new(),
            registers: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn entry(&self) -> Option<MIRBasicBlockID> {
        self.entry
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

    pub fn blocks_mut(&mut self) -> &mut [MIRBasicBlock] {
        &mut self.blocks
    }

    pub fn add_block(&mut self) -> MIRBasicBlockID {
        let id = MIRBasicBlockID::new(self.blocks.len());
        self.blocks.push(MIRBasicBlock::new(id));
        if self.entry.is_none() {
            self.entry = Some(id);
        }
        id
    }

    pub fn places(&self) -> &[MIRPlaceDecl] {
        &self.places
    }

    pub fn place(&self, id: MIRPlaceID) -> Option<&MIRPlaceDecl> {
        self.places().get(id.index())
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

    pub fn scopes(&self) -> &[MIRScopeDecl] {
        &self.scopes
    }

    pub fn scope(&self, id: MIRScopeID) -> Option<&MIRScopeDecl> {
        self.scopes().get(id.index())
    }

    pub fn registers(&self) -> &[MIRRegisterDecl] {
        &self.registers
    }

    pub fn register(&self, id: MIRRegister) -> Option<&MIRRegisterDecl> {
        self.registers().get(id.index())
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
        self.block(block)?;
        let register = self.add_register(ty, debug_name);
        self.block_mut(block)?.params.push(register);
        Some(register)
    }

    pub fn add_scope(&mut self, token_range: TokenRange) -> MIRScopeID {
        let id = MIRScopeID::new(self.scopes.len());
        self.scopes.push(MIRScopeDecl { id, token_range });
        id
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
