mod lower;
mod mir;

use lower::lower_type;

use std::collections::HashMap;

use cx_hmir::{HMIRAggregateKind, HMIRExprID, HMIRFloatWidth, HMIRIntWidth, HMIRMoveSemantics};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{
    MIRFloatType, MIRIntType, MIRType, MIRTypeID, MIRTypeKind,
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
    // Only ever wraps an unqualified type that is not a reference, array or function
    Const(TypeID),
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
    unsafe_move: bool,
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

    // The type's shape, looking through a 'const' qualifier
    pub(crate) fn kind(&self, id: TypeID) -> &TypeKind {
        match &self.kinds[id.index()] {
            TypeKind::Const(inner) => &self.kinds[inner.index()],
            kind => kind,
        }
    }

    // Arrays are qualified through their elements; references and functions cannot be
    pub(crate) fn const_of(&mut self, id: TypeID) -> TypeID {
        match self.kinds[id.index()].clone() {
            TypeKind::Const(_) | TypeKind::Reference(_) | TypeKind::Function(_) => id,
            TypeKind::Array { element, length } => {
                let element = self.const_of(element);
                self.array_of(element, length)
            }
            _ => self.intern(TypeKind::Const(id)),
        }
    }

    pub(crate) fn is_const(&self, id: TypeID) -> bool {
        match &self.kinds[id.index()] {
            TypeKind::Const(_) => true,
            TypeKind::Array { element, .. } => self.is_const(*element),
            _ => false,
        }
    }

    // The type without its own qualifier; what it points at or refers to keeps its own
    pub(crate) fn unqualified(&mut self, id: TypeID) -> TypeID {
        match self.kinds[id.index()].clone() {
            TypeKind::Const(inner) => inner,
            TypeKind::Array { element, length } if self.is_const(element) => {
                let element = self.unqualified(element);
                self.array_of(element, length)
            }
            _ => id,
        }
    }

    // The type with every qualifier removed, for comparing types up to qualification
    pub(crate) fn erased(&mut self, id: TypeID) -> TypeID {
        match self.kinds[id.index()].clone() {
            TypeKind::Const(inner) => self.erased(inner),
            TypeKind::Pointer(inner) => {
                let inner = self.erased(inner);
                self.pointer_to(inner)
            }
            TypeKind::Reference(inner) => {
                let inner = self.erased(inner);
                self.reference_to(inner)
            }
            TypeKind::Array { element, length } => {
                let element = self.erased(element);
                self.array_of(element, length)
            }
            TypeKind::Function(function) => {
                let params = function
                    .params
                    .iter()
                    .map(|param| self.erased(*param))
                    .collect();
                let ret = self.erased(function.ret);
                self.intern(TypeKind::Function(FunctionType::new(
                    params,
                    ret,
                    function.variadic,
                )))
            }
            _ => id,
        }
    }

    pub(crate) fn same_unqualified(&mut self, lhs: TypeID, rhs: TypeID) -> bool {
        lhs == rhs || self.erased(lhs) == self.erased(rhs)
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

    pub(crate) fn pointer_to(&mut self, inner: TypeID) -> TypeID {
        self.intern(TypeKind::Pointer(inner))
    }

    pub(crate) fn reference_to(&mut self, inner: TypeID) -> TypeID {
        self.intern(TypeKind::Reference(inner))
    }

    pub(crate) fn array_of(&mut self, element: TypeID, length: Option<u64>) -> TypeID {
        self.intern(TypeKind::Array { element, length })
    }

    pub(crate) fn pointer_inner(&self, id: TypeID) -> Option<TypeID> {
        match self.kind(id) {
            TypeKind::Pointer(inner) => Some(*inner),
            _ => None,
        }
    }

    pub(crate) fn reference_inner(&self, id: TypeID) -> Option<TypeID> {
        match self.kind(id) {
            TypeKind::Reference(inner) => Some(*inner),
            _ => None,
        }
    }

    pub(crate) fn array_inner(&self, id: TypeID) -> Option<TypeID> {
        match self.kind(id) {
            TypeKind::Array { element, .. } => Some(*element),
            _ => None,
        }
    }

    pub(crate) fn function_type(&self, id: TypeID) -> Option<&FunctionType> {
        match self.kind(id) {
            TypeKind::Function(function) => Some(function),
            _ => None,
        }
    }

    pub(crate) fn is_pointer(&self, id: TypeID) -> bool {
        self.pointer_inner(id).is_some()
    }

    pub(crate) fn is_reference(&self, id: TypeID) -> bool {
        self.reference_inner(id).is_some()
    }

    pub(crate) fn is_array(&self, id: TypeID) -> bool {
        self.array_inner(id).is_some()
    }

    pub(crate) fn is_function(&self, id: TypeID) -> bool {
        self.function_type(id).is_some()
    }

    pub(crate) fn char(&mut self) -> TypeID {
        self.int(HMIRIntWidth::I8, false)
    }

    pub(crate) fn char_pointer(&mut self) -> TypeID {
        let char = self.char();
        self.pointer_to(char)
    }

    // The pointer type an array, string or function takes when used as a value
    pub(crate) fn decayed(&mut self, id: TypeID) -> TypeID {
        match self.kind(id).clone() {
            TypeKind::Array { element, .. } => self.pointer_to(element),
            TypeKind::Str => self.char_pointer(),
            TypeKind::Function(_) => self.pointer_to(id),
            _ => id,
        }
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
        unsafe_move: bool,
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
            unsafe_move,
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

    // The field indices leading to the member 'name', through the anonymous members holding it
    pub(crate) fn member_path(&self, ty: TypeID, name: &str) -> Option<Vec<usize>> {
        if let Some((index, _)) = self.field(ty, name) {
            return Some(vec![index]);
        }
        let fields = self.nominal_of(ty)?.fields().iter().enumerate();
        fields
            .filter(|(_, field)| field.name().is_none() && field.bit_width().is_none())
            .find_map(|(index, field)| {
                let mut path = vec![index];
                path.extend(self.member_path(field.ty(), name)?);
                Some(path)
            })
    }

    // The move semantics a value of this type has where it is held by value
    pub(crate) fn owned_traits(&self, ty: TypeID) -> (HMIRMoveSemantics, bool) {
        match self.kind(ty) {
            TypeKind::Array { element, .. } => self.owned_traits(*element),
            TypeKind::Nominal(nominal) => {
                let nominal = self.nominal(*nominal);
                (nominal.semantics, nominal.unsafe_move)
            }
            _ => (HMIRMoveSemantics::POD, false),
        }
    }

    pub(crate) fn is_unsafe_move(&self, ty: TypeID) -> bool {
        self.owned_traits(ty).1
    }

    pub(crate) fn is_nodrop(&self, ty: TypeID) -> bool {
        self.nominal_of(ty)
            .is_some_and(|nominal| nominal.semantics() == HMIRMoveSemantics::Nodrop)
    }

    pub(crate) fn is_pod(&self, ty: TypeID) -> bool {
        self.nominal_of(ty)
            .is_none_or(|nominal| nominal.semantics() == HMIRMoveSemantics::POD)
    }

    // Why no object of this type can exist, if none can
    pub(crate) fn object_problem(&self, ty: TypeID) -> Option<&'static str> {
        match self.kind(ty) {
            TypeKind::Function(_) => Some("a function type"),
            TypeKind::Opaque { size: 0, .. } => Some("an incomplete type"),
            TypeKind::Nominal(nominal) if !self.nominal(*nominal).is_complete() => {
                Some("an incomplete type")
            }
            TypeKind::Array { element, .. } => self.object_problem(*element),
            _ => None,
        }
    }

    fn require_sized(&self, ty: TypeID, span: &TokenRange) -> CXResult<()> {
        let incomplete = matches!(self.kind(ty), TypeKind::Array { length: None, .. })
            || self
                .object_problem(ty)
                .is_some_and(|problem| problem == "an incomplete type");
        if incomplete {
            return Err(staging_error(
                span,
                &typecheck::INCOMPLETE_TYPE,
                format!("'{}'", self.display(ty)),
            ));
        }
        Ok(())
    }

    pub(crate) fn size_of(&mut self, ty: TypeID, span: &TokenRange) -> CXResult<u64> {
        self.require_sized(ty, span)?;
        let mir = self.mir(ty, span)?;
        Ok(calculate_type_layout(&self.mir, mir).size() as u64)
    }

    pub(crate) fn align_of(&mut self, ty: TypeID, span: &TokenRange) -> CXResult<u64> {
        self.require_sized(ty, span)?;
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
        lower_type(self, ty, span)
    }

    pub(crate) fn display(&self, ty: TypeID) -> String {
        if let TypeKind::Const(inner) = &self.kinds[ty.index()] {
            return match self.kind(*inner) {
                TypeKind::Pointer(_) => format!("{} const", self.display(*inner)),
                _ => format!("const {}", self.display(*inner)),
            };
        }
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
            TypeKind::Const(_) => unreachable!("'kind' looks through qualifiers"),
        }
    }

    // Matches THIR's template-argument mangling closely enough to keep instance symbols readable
    pub(crate) fn mangle(&self, ty: TypeID) -> String {
        if let TypeKind::Const(inner) = &self.kinds[ty.index()] {
            return format!("K{}", self.mangle(*inner));
        }
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
