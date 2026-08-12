use cx_ast::ast::modifiers::CXLinkageMode;
use cx_util::identifier::CXIdent;

use crate::{
    expr::{
        MIRBasicBlock, MIRBasicBlockID, MIRConstant, MIRInstr, MIRInstrKind, MIRPlace, MIRPlaceID,
        MIRRegister,
    },
    ty::MIRType,
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
pub enum MIRGlobalInitializer {
    Scalar(MIRConstant),
    Bytes(Box<[u8]>),
}

#[derive(Debug, Clone)]
pub struct MIRGlobalVariable {
    pub id: MIRGlobalID,
    pub name: CXIdent,
    
    pub ty: MIRType,
    pub linkage: CXLinkageMode,
    pub initializer: Option<MIRGlobalInitializer>,
 
    pub is_definition: bool,
    pub is_mutable: bool,
}

impl MIRGlobalVariable {
    pub fn new(
        id: MIRGlobalID,
        name: CXIdent,
        ty: MIRType,
        linkage: CXLinkageMode,
        is_mutable: bool,
    ) -> Self {
        Self {
            id,
            name,
            ty,
            linkage,
            initializer: None,
            is_definition: true,
            is_mutable,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MIRFnParam {
    pub name: Option<CXIdent>,
    pub ty: MIRType,
}

impl MIRFnParam {
    pub fn new(ty: MIRType) -> Self {
        Self { name: None, ty }
    }

    pub fn named(name: CXIdent, ty: MIRType) -> Self {
        Self {
            name: Some(name),
            ty,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MIRFnSignature {
    pub name: CXIdent,
    pub params: Vec<MIRFnParam>,
    pub return_type: Option<MIRType>,
    pub variadic: bool,
}

impl MIRFnSignature {
    pub fn new(name: CXIdent, params: Vec<MIRFnParam>, return_type: Option<MIRType>) -> Self {
        Self {
            name,
            params,
            return_type,
            variadic: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MIRFnPrototype {
    pub signature: MIRFnSignature,
    pub linkage: CXLinkageMode,
}

impl MIRFnPrototype {
    pub fn new(signature: MIRFnSignature, linkage: CXLinkageMode) -> Self {
        Self { signature, linkage }
    }
}

#[derive(Debug, Clone)]
pub struct MIRPlaceDecl {
    pub id: MIRPlaceID,
    pub ty: MIRType,
    pub debug_name: Option<CXIdent>,
}

#[derive(Debug, Clone)]
pub struct MIRRegisterDecl {
    pub id: MIRRegister,
    pub ty: MIRType,
    pub debug_name: Option<CXIdent>,
}

#[derive(Debug, Clone)]
pub struct MIRFunction {
    pub id: MIRFunctionID,
    pub prototype: MIRFnPrototype,
    /// Declarations have no entry and no blocks. Definitions have both.
    pub entry: Option<MIRBasicBlockID>,
    pub blocks: Vec<MIRBasicBlock>,
    pub places: Vec<MIRPlaceDecl>,
    pub registers: Vec<MIRRegisterDecl>,
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
        }
    }

    pub fn is_declaration(&self) -> bool {
        self.blocks.is_empty()
    }

    pub fn add_place(&mut self, ty: MIRType, debug_name: Option<CXIdent>) -> MIRPlace {
        let id = MIRPlaceID::new(self.places.len());
        self.places.push(MIRPlaceDecl { id, ty, debug_name });
        MIRPlace::FunctionLocal(id)
    }

    pub fn add_register(&mut self, ty: MIRType, debug_name: Option<CXIdent>) -> MIRRegister {
        let id = MIRRegister::new(self.registers.len());
        self.registers.push(MIRRegisterDecl { id, ty, debug_name });
        id
    }

    pub fn add_block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRType,
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

    pub fn register(&self, id: MIRRegister) -> Option<&MIRRegisterDecl> {
        self.registers.get(id.index())
    }

    pub fn push_instr(
        &mut self,
        block: MIRBasicBlockID,
        kind: MIRInstrKind,
    ) -> Option<&mut MIRInstr> {
        let block = self.block_mut(block)?;
        block.instrs.push(MIRInstr::new(kind));
        block.instrs.last_mut()
    }
}
