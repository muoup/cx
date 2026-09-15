use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    MIRBody, MIRComptimeBody,
    instruction::{MIRConstant, MIRPlaceID, MIRRegister, MIRScopeID},
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
    pub staged_params: Option<Vec<MIRTypeID>>,
    pub staged_diverges: bool,
}

impl MIRFnParam {
    pub fn new(ty: MIRTypeID) -> Self {
        Self {
            name: None,
            ty,
            nodrop: false,
            staged_params: None,
            staged_diverges: false,
        }
    }

    pub fn named(name: CXIdent, ty: MIRTypeID) -> Self {
        Self {
            name: Some(name),
            ty,
            nodrop: false,
            staged_params: None,
            staged_diverges: false,
        }
    }

    pub fn with_nodrop(mut self, nodrop: bool) -> Self {
        self.nodrop = nodrop;
        self
    }

    pub fn with_staged(mut self, params: Option<Vec<MIRTypeID>>, diverges: bool) -> Self {
        self.staged_params = params;
        self.staged_diverges = diverges;
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
    pub return_staged_params: Option<Vec<MIRTypeID>>,
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
            return_staged_params: None,
        }
    }

    pub fn display_name(&self) -> &CXIdent {
        self.debug_name.as_ref().unwrap_or(&self.symbol_name)
    }

    pub fn with_staged_return(mut self, params: Option<Vec<MIRTypeID>>) -> Self {
        self.return_staged_params = params;
        self
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
    body: Option<MIRFunctionBody>,
}

impl MIRFunction {
    pub fn new(
        id: MIRFunctionID,
        prototype: MIRFnPrototype,
        definition: Option<MIRFunctionBody>,
    ) -> Self {
        if let Some(body) = &definition {
            assert_eq!(
                matches!(body, MIRFunctionBody::Comptime(_)),
                prototype.signature.mode == MIRFunctionMode::Comptime
            );
        }
        Self {
            id,
            prototype,
            body: definition,
        }
    }

    pub fn id(&self) -> MIRFunctionID {
        self.id
    }

    pub fn body(&self) -> Option<&MIRFunctionBody> {
        self.body.as_ref()
    }

    pub fn prototype(&self) -> &MIRFnPrototype {
        &self.prototype
    }

    pub fn mode(&self) -> MIRFunctionMode {
        self.prototype().signature.mode
    }

    pub fn definition(&self) -> Option<&MIRBody> {
        match self.body.as_ref()? {
            MIRFunctionBody::Runtime(body) => Some(body),
            MIRFunctionBody::Comptime(_) => None,
        }
    }

    pub fn comptime_definition(&self) -> Option<&MIRComptimeBody> {
        match self.body.as_ref()? {
            MIRFunctionBody::Comptime(body) => Some(body),
            MIRFunctionBody::Runtime(_) => None,
        }
    }

    pub fn define(&mut self, def: MIRFunctionBody) {
        assert!(
            self.body.is_none(),
            "Attempt to redefine function: {}",
            self.prototype().signature.display_name()
        );
        assert_eq!(
            matches!(def, MIRFunctionBody::Comptime(_)),
            self.mode() == MIRFunctionMode::Comptime
        );
        self.body = Some(def);
    }
}

#[derive(Debug, Clone)]
pub enum MIRFunctionBody {
    Runtime(MIRBody),
    Comptime(MIRComptimeBody),
}

impl MIRFunctionBody {
    pub fn places(&self) -> &[MIRPlaceDecl] {
        match self {
            Self::Runtime(body) => body.places(),
            Self::Comptime(body) => body.places(),
        }
    }

    pub fn place(&self, id: MIRPlaceID) -> Option<&MIRPlaceDecl> {
        self.places().get(id.index())
    }

    pub fn register(&self, id: MIRRegister) -> Option<&MIRRegisterDecl> {
        match self {
            Self::Runtime(body) => body.register(id),
            Self::Comptime(body) => body.register(id),
        }
    }

    pub fn instruction_range(
        &self,
        block: crate::MIRBasicBlockID,
        index: usize,
    ) -> Option<&TokenRange> {
        match self {
            Self::Runtime(body) => body
                .block(block)?
                .instrs
                .get(index)
                .map(|instruction| &instruction.token_range),
            Self::Comptime(body) => body
                .block(block)?
                .instrs
                .get(index)
                .map(|instruction| &instruction.token_range),
        }
    }

    pub fn scope(&self, scope: MIRScopeID) -> Option<&MIRScopeDecl> {
        match self {
            Self::Runtime(body) => body.scope(scope),
            Self::Comptime(body) => body.scope(scope),
        }
    }
}
