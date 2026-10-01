mod mir;

use std::collections::HashMap;

use cx_hmir::{HMIRAggregateKind, HMIRExprID, HMIRFloatWidth, HMIRIntWidth, HMIRMoveSemantics};
use cx_log::CXResult;
use cx_mir::{
    MIRField, MIRFloatType, MIRFnParam, MIRFnSignature, MIRIntType, MIRType, MIRTypeID,
    MIRTypeKind,
    ty::{layout::calculate_type_layout, registry::MIRTypeRegistry},
};
use cx_target::ArchitectureConfig;
use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent};

use crate::{program::DefKey, staging_error, value::StaticValue};

pub(crate) use mir::MIRTypes;

dense_id!(TypeID, "ty");
dense_id!(NominalID, "nominal");

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum TypeKind {
    Void,
    Unreachable,
    Type,
    Str,
    Int {
        width: HMIRIntWidth,
        signed: bool,
    },
    Float {
        width: HMIRFloatWidth,
    },
    Pointer(TypeID),
    Reference(TypeID),
    Array {
        element: TypeID,
        length: Option<u64>,
    },
    Function(FunctionType),
    Expr {
        params: Vec<TypeID>,
        result: TypeID,
    },
    Nominal(NominalID),
    Opaque {
        size: usize,
        alignment: usize,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct FunctionType {
    params: Vec<TypeID>,
    ret: TypeID,
    variadic: bool,
}

// Identifies an aggregate type expression evaluated for a particular def instance
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct NominalKey {
    owner: DefKey,
    args: Vec<StaticValue>,
    expr: HMIRExprID,
}

#[derive(Debug, Clone)]
pub(crate) struct Field {
    name: Option<CXIdent>,
    ty: TypeID,
    bit_width: Option<usize>,
}

#[derive(Debug, Clone)]
pub(crate) struct Nominal {
    key: NominalKey,
    name: String,
    kind: HMIRAggregateKind,
    semantics: HMIRMoveSemantics,
    fields: Option<Vec<Field>>,
}

pub(crate) struct TypeTable {
    kinds: Vec<TypeKind>,
    ids: HashMap<TypeKind, TypeID>,
    nominals: Vec<Nominal>,
    nominal_ids: HashMap<NominalKey, NominalID>,
    mir: MIRTypes,
    lowered: HashMap<TypeID, MIRTypeID>,
}

impl FunctionType {
    pub(crate) fn new(params: Vec<TypeID>, ret: TypeID, variadic: bool) -> Self {
        Self {
            params,
            ret,
            variadic,
        }
    }

    pub(crate) fn params(&self) -> &[TypeID] {
        &self.params
    }

    pub(crate) fn ret(&self) -> TypeID {
        self.ret
    }

    pub(crate) fn is_variadic(&self) -> bool {
        self.variadic
    }
}

impl NominalKey {
    pub(crate) fn new(owner: DefKey, args: Vec<StaticValue>, expr: HMIRExprID) -> Self {
        Self { owner, args, expr }
    }

    pub(crate) fn owner(&self) -> DefKey {
        self.owner
    }

    pub(crate) fn args(&self) -> &[StaticValue] {
        &self.args
    }
}

impl Field {
    pub(crate) fn new(name: Option<CXIdent>, ty: TypeID, bit_width: Option<usize>) -> Self {
        Self {
            name,
            ty,
            bit_width,
        }
    }

    pub(crate) fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    pub(crate) fn ty(&self) -> TypeID {
        self.ty
    }

    pub(crate) fn bit_width(&self) -> Option<usize> {
        self.bit_width
    }
}

impl Nominal {
    pub(crate) fn key(&self) -> &NominalKey {
        &self.key
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn kind(&self) -> HMIRAggregateKind {
        self.kind
    }

    pub(crate) fn semantics(&self) -> HMIRMoveSemantics {
        self.semantics
    }

    pub(crate) fn fields(&self) -> &[Field] {
        self.fields.as_deref().unwrap_or_default()
    }

    pub(crate) fn is_complete(&self) -> bool {
        self.fields.is_some()
    }
}

impl TypeTable {
    pub(crate) fn new(architecture: ArchitectureConfig) -> Self {
        let mut table = Self {
            kinds: Vec::new(),
            ids: HashMap::new(),
            nominals: Vec::new(),
            nominal_ids: HashMap::new(),
            mir: MIRTypes::new(architecture),
            lowered: HashMap::new(),
        };
        table.mir.intern(MIRType::new(MIRTypeKind::Void));
        table
    }

    pub(crate) fn intern(&mut self, kind: TypeKind) -> TypeID {
        if let Some(id) = self.ids.get(&kind) {
            return *id;
        }
        let id = TypeID::new(self.kinds.len());
        self.kinds.push(kind.clone());
        self.ids.insert(kind, id);
        id
    }

    pub(crate) fn kind(&self, id: TypeID) -> &TypeKind {
        &self.kinds[id.index()]
    }

    pub(crate) fn void(&mut self) -> TypeID {
        self.intern(TypeKind::Void)
    }

    pub(crate) fn type_of_types(&mut self) -> TypeID {
        self.intern(TypeKind::Type)
    }

    pub(crate) fn str(&mut self) -> TypeID {
        self.intern(TypeKind::Str)
    }

    pub(crate) fn bool(&mut self) -> TypeID {
        self.int(HMIRIntWidth::I1, false)
    }

    pub(crate) fn int(&mut self, width: HMIRIntWidth, signed: bool) -> TypeID {
        self.intern(TypeKind::Int { width, signed })
    }

    pub(crate) fn size_type(&mut self) -> TypeID {
        self.int(HMIRIntWidth::I64, false)
    }

    pub(crate) fn pointer(&mut self, inner: TypeID) -> TypeID {
        self.intern(TypeKind::Pointer(inner))
    }

    pub(crate) fn reference(&mut self, inner: TypeID) -> TypeID {
        self.intern(TypeKind::Reference(inner))
    }

    pub(crate) fn char_pointer(&mut self) -> TypeID {
        let char = self.int(HMIRIntWidth::I8, true);
        self.pointer(char)
    }

    pub(crate) fn is_void(&self, id: TypeID) -> bool {
        matches!(self.kind(id), TypeKind::Void)
    }

    pub(crate) fn is_unreachable(&self, id: TypeID) -> bool {
        matches!(self.kind(id), TypeKind::Unreachable)
    }

    pub(crate) fn int_info(&self, id: TypeID) -> Option<(HMIRIntWidth, bool)> {
        match self.kind(id) {
            TypeKind::Int { width, signed } => Some((*width, *signed)),
            _ => None,
        }
    }

    pub(crate) fn is_signed(&self, id: TypeID) -> bool {
        self.int_info(id).is_some_and(|(_, signed)| signed)
    }

    pub(crate) fn pointee(&self, id: TypeID) -> Option<TypeID> {
        match self.kind(id) {
            TypeKind::Pointer(inner) => Some(*inner),
            _ => None,
        }
    }

    pub(crate) fn nominal(&self, id: NominalID) -> &Nominal {
        &self.nominals[id.index()]
    }

    pub(crate) fn nominal_of(&self, id: TypeID) -> Option<&Nominal> {
        match self.kind(id) {
            TypeKind::Nominal(nominal) => Some(self.nominal(*nominal)),
            _ => None,
        }
    }

    // Returns the type and whether its fields still have to be filled in
    pub(crate) fn intern_nominal(
        &mut self,
        key: NominalKey,
        name: String,
        kind: HMIRAggregateKind,
        semantics: HMIRMoveSemantics,
    ) -> (TypeID, bool) {
        if let Some(id) = self.nominal_ids.get(&key) {
            let pending = !self.nominals[id.index()].is_complete();
            return (self.intern(TypeKind::Nominal(*id)), pending);
        }
        let id = NominalID::new(self.nominals.len());
        self.nominals.push(Nominal {
            key: key.clone(),
            name,
            kind,
            semantics,
            fields: None,
        });
        self.nominal_ids.insert(key, id);
        (self.intern(TypeKind::Nominal(id)), true)
    }

    pub(crate) fn define_nominal(&mut self, ty: TypeID, fields: Vec<Field>) {
        if let TypeKind::Nominal(id) = self.kind(ty) {
            let id = *id;
            self.nominals[id.index()].fields = Some(fields);
        }
    }

    pub(crate) fn field(&self, ty: TypeID, name: &str) -> Option<(usize, &Field)> {
        self.nominal_of(ty)?
            .fields()
            .iter()
            .enumerate()
            .find(|(_, field)| field.name().is_some_and(|field| field.as_str() == name))
    }

    pub(crate) fn is_nodrop(&self, ty: TypeID) -> bool {
        self.nominal_of(ty)
            .is_some_and(|nominal| nominal.semantics() == HMIRMoveSemantics::Nodrop)
    }

    pub(crate) fn size_of(&mut self, ty: TypeID, span: &TokenRange) -> CXResult<u64> {
        let mir = self.mir(ty, span)?;
        Ok(calculate_type_layout(&self.mir, mir).size() as u64)
    }

    pub(crate) fn align_of(&mut self, ty: TypeID, span: &TokenRange) -> CXResult<u64> {
        let mir = self.mir(ty, span)?;
        Ok(calculate_type_layout(&self.mir, mir).alignment() as u64)
    }

    pub(crate) fn mir_types(&self) -> &MIRTypes {
        &self.mir
    }

    pub(crate) fn finish(self) -> MIRTypeRegistry {
        self.mir.finish()
    }

    pub(crate) fn mir_int(width: HMIRIntWidth) -> MIRIntType {
        match width {
            HMIRIntWidth::I1 => MIRIntType::I1,
            HMIRIntWidth::I8 => MIRIntType::I8,
            HMIRIntWidth::I16 => MIRIntType::I16,
            HMIRIntWidth::I32 => MIRIntType::I32,
            HMIRIntWidth::I64 => MIRIntType::I64,
            HMIRIntWidth::I128 => MIRIntType::I128,
        }
    }

    pub(crate) fn mir_float(width: HMIRFloatWidth) -> MIRFloatType {
        match width {
            HMIRFloatWidth::F32 => MIRFloatType::F32,
            HMIRFloatWidth::F64 => MIRFloatType::F64,
        }
    }

    pub(crate) fn mir(&mut self, ty: TypeID, span: &TokenRange) -> CXResult<MIRTypeID> {
        if let Some(id) = self.lowered.get(&ty) {
            return Ok(*id);
        }
        let kind = match self.kind(ty).clone() {
            TypeKind::Void | TypeKind::Unreachable => MIRTypeKind::Void,
            TypeKind::Type | TypeKind::Expr { .. } => {
                return Err(staging_error(
                    span,
                    format!("comptime-only type '{}' used at runtime", self.display(ty)),
                ));
            }
            TypeKind::Str => {
                let pointer = self.char_pointer();
                return self.mir(pointer, span);
            }
            TypeKind::Int { width, .. } => MIRTypeKind::Integer {
                ty: Self::mir_int(width),
            },
            TypeKind::Float { width } => MIRTypeKind::Float {
                ty: Self::mir_float(width),
            },
            TypeKind::Pointer(inner) => MIRTypeKind::PointerTo {
                inner: self.mir(inner, span)?,
            },
            TypeKind::Reference(inner) => MIRTypeKind::MemoryReference {
                inner: self.mir(inner, span)?,
            },
            TypeKind::Array {
                element,
                length: Some(length),
            } => MIRTypeKind::Array {
                length: length as usize,
                inner: self.mir(element, span)?,
            },
            TypeKind::Array {
                element,
                length: None,
            } => MIRTypeKind::IncompleteArray {
                inner: self.mir(element, span)?,
            },
            TypeKind::Function(function) => MIRTypeKind::Function {
                signature: self.mir_signature(&function, span)?,
            },
            TypeKind::Opaque { size, alignment } => MIRTypeKind::Opaque { size, alignment },
            TypeKind::Nominal(nominal) => return self.mir_nominal(ty, nominal, span),
        };
        let id = self.mir.intern(MIRType::new(kind));
        self.lowered.insert(ty, id);
        Ok(id)
    }

    pub(crate) fn mir_signature(
        &mut self,
        function: &FunctionType,
        span: &TokenRange,
    ) -> CXResult<MIRFnSignature> {
        let params = function
            .params()
            .iter()
            .map(|param| Ok(MIRFnParam::new(None, self.mir(*param, span)?, false)))
            .collect::<CXResult<Vec<_>>>()?;
        let ret = self.mir(function.ret(), span)?;
        Ok(MIRFnSignature::new(
            params,
            ret,
            function.is_variadic(),
            false,
        ))
    }

    fn mir_nominal(
        &mut self,
        ty: TypeID,
        nominal: NominalID,
        span: &TokenRange,
    ) -> CXResult<MIRTypeID> {
        let nominal = self.nominal(nominal).clone();
        let Some(fields) = nominal.fields else {
            let id = self.mir.intern(MIRType::new(MIRTypeKind::Opaque {
                size: 0,
                alignment: 1,
            }));
            self.lowered.insert(ty, id);
            return Ok(id);
        };

        let id = self.mir.reserve();
        self.lowered.insert(ty, id);
        let fields = fields
            .iter()
            .map(|field| {
                let name = field.name().map(CXIdent::as_string);
                let ty = self.mir(field.ty(), span)?;
                Ok(match field.bit_width() {
                    Some(width) => MIRField::Bitfield {
                        name,
                        integer_type_id: ty,
                        width,
                    },
                    None => MIRField::Standard { name, type_id: ty },
                })
            })
            .collect::<CXResult<Vec<_>>>()?;
        let kind = match nominal.kind {
            HMIRAggregateKind::Struct => MIRTypeKind::Structured { fields },
            HMIRAggregateKind::Union => MIRTypeKind::Union { variants: fields },
            HMIRAggregateKind::TaggedUnion => MIRTypeKind::TaggedUnion { variants: fields },
        };
        self.mir.define(id, MIRType::new(kind));
        self.mir.set_debug_name(id, nominal.name);
        Ok(id)
    }

    pub(crate) fn display(&self, ty: TypeID) -> String {
        match self.kind(ty) {
            TypeKind::Void => "void".into(),
            TypeKind::Unreachable => "unreachable".into(),
            TypeKind::Type => "type".into(),
            TypeKind::Str => "str".into(),
            TypeKind::Int { width, signed } => {
                format!("{}{}", if *signed { "i" } else { "u" }, width.bits())
            }
            TypeKind::Float { width } => format!("f{}", width.bits()),
            TypeKind::Pointer(inner) => format!("{}*", self.display(*inner)),
            TypeKind::Reference(inner) => format!("{}&", self.display(*inner)),
            TypeKind::Array { element, length } => match length {
                Some(length) => format!("{}[{length}]", self.display(*element)),
                None => format!("{}[]", self.display(*element)),
            },
            TypeKind::Function(function) => {
                let params = function
                    .params()
                    .iter()
                    .map(|param| self.display(*param))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({params})", self.display(function.ret()))
            }
            TypeKind::Expr { result, .. } => format!("expr {}", self.display(*result)),
            TypeKind::Nominal(nominal) => self.nominal(*nominal).name().to_string(),
            TypeKind::Opaque { size, alignment } => format!("opaque({size}, {alignment})"),
        }
    }

    // Matches THIR's template-argument mangling closely enough to keep instance symbols readable
    pub(crate) fn mangle(&self, ty: TypeID) -> String {
        match self.kind(ty) {
            TypeKind::Pointer(inner) => format!("P{}", self.mangle(*inner)),
            TypeKind::Reference(inner) => format!("R{}", self.mangle(*inner)),
            TypeKind::Array { element, length } => {
                format!("A{}_{}", length.unwrap_or(0), self.mangle(*element))
            }
            TypeKind::Nominal(nominal) => {
                let name = self
                    .nominal(*nominal)
                    .name()
                    .replace(|c: char| !c.is_ascii_alphanumeric(), "_");
                format!("{}{}", name.len(), name)
            }
            _ => self
                .display(ty)
                .replace(|c: char| !c.is_ascii_alphanumeric(), "_"),
        }
    }
}
