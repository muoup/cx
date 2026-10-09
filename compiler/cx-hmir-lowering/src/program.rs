use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use cx_hmir::{
    HMIRBody, HMIRConstant, HMIRDef, HMIRDefID, HMIRDefKind, HMIRDefRef, HMIRFnTypeDesc,
    HMIRTypeDesc, HMIRTypeID, HMIRUnit,
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_namespace::module::QualifiedName;
use cx_target::ArchitectureConfig;
use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent};

use crate::{
    eval::{Signature, equivalent_def},
    module::Module,
    staging_error,
    ty::{FunctionType, HMIRTypeKind, TypeTable},
    value::StaticValue,
};

dense_id!(UnitID, "unit");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct DefKey {
    unit: UnitID,
    def: HMIRDefID,
}

pub(crate) type Instance = (DefKey, Vec<StaticValue>);

pub(crate) type ExternalLoader<'l> = dyn FnMut(&QualifiedName) -> Option<HMIRUnit> + 'l;

pub(crate) struct Program<'l> {
    units: Vec<Rc<HMIRUnit>>,
    
    names: Vec<HashMap<QualifiedName, HMIRDefID>>,
    externals: HashMap<QualifiedName, Option<DefKey>>,

    loader: Box<ExternalLoader<'l>>,
    types: TypeTable,

    module: Module,
    generated: HashMap<Instance, StaticValue>,
    active: HashSet<Instance>,
    reentered: HashSet<Instance>,
    signatures: HashMap<Instance, Rc<Signature>>,
    global_types: HashMap<DefKey, HMIRTypeID>,
    imported: HashMap<(UnitID, HMIRTypeID), HMIRTypeID>,
    serial: u64,
    require_explicit_return: bool,
    deferral: Deferral,
}

#[derive(Default)]
pub(crate) struct Deferral {
    // How many aggregates are having their fields evaluated
    pub(crate) defining: usize,
    // Set inside the expressions of a type, which may look through a pointer
    pub(crate) eager: bool,
    // Named aggregates that were pointed to before being defined
    pub(crate) pending: Vec<DefKey>,
}

impl DefKey {
    pub(crate) fn new(unit: UnitID, def: HMIRDefID) -> Self {
        Self { unit, def }
    }

    pub(crate) fn unit(self) -> UnitID {
        self.unit
    }

    pub(crate) fn def(self) -> HMIRDefID {
        self.def
    }
}

pub(crate) fn def_body(def: &HMIRDef) -> Option<&HMIRBody> {
    match def.kind() {
        HMIRDefKind::Function(function) => Some(function.body()),
        HMIRDefKind::Global(global) => Some(global.body()),
        HMIRDefKind::ComptimeGlobal(global) => Some(global.body()),
        HMIRDefKind::Type(_) => None,
    }
}

// Def names carry a tag prefix ("struct ", "union ", "enum ") that associated items omit
pub(crate) fn untagged_name(name: &CXIdent) -> &str {
    ["struct ", "union ", "enum "]
        .iter()
        .find_map(|prefix| name.as_str().strip_prefix(prefix))
        .unwrap_or(name.as_str())
}

impl<'l> Program<'l> {
    pub(crate) fn new(
        main: HMIRUnit,
        loader: Box<ExternalLoader<'l>>,
        architecture: ArchitectureConfig,
        require_explicit_return: bool,
    ) -> Self {
        let mut program = Self {
            units: Vec::new(),
            names: Vec::new(),
            externals: HashMap::new(),
            loader,
            types: TypeTable::new(architecture),
            module: Module::new(),
            generated: HashMap::new(),
            active: HashSet::new(),
            reentered: HashSet::new(),
            signatures: HashMap::new(),
            global_types: HashMap::new(),
            imported: HashMap::new(),
            serial: 0,
            require_explicit_return,
            deferral: Deferral::default(),
        };
        program.push_unit(main);
        program
    }

    pub(crate) fn require_explicit_return(&self) -> bool {
        self.require_explicit_return
    }

    fn push_unit(&mut self, unit: HMIRUnit) -> UnitID {
        let mut names = HashMap::new();
        for (id, def) in unit.defs() {
            names.entry(def.name().clone()).or_insert(id);
        }
        self.units.push(Rc::new(unit));
        self.names.push(names);
        UnitID::new(self.units.len() - 1)
    }

    pub(crate) fn main_unit(&self) -> UnitID {
        UnitID::new(0)
    }

    pub(crate) fn unit(&self, id: UnitID) -> Rc<HMIRUnit> {
        self.units[id.index()].clone()
    }

    pub(crate) fn def_name(&self, key: DefKey) -> QualifiedName {
        self.units[key.unit.index()].def(key.def).name().clone()
    }

    pub(crate) fn types(&self) -> &TypeTable {
        &self.types
    }

    pub(crate) fn types_mut(&mut self) -> &mut TypeTable {
        &mut self.types
    }

    pub(crate) fn module(&self) -> &Module {
        &self.module
    }

    pub(crate) fn module_mut(&mut self) -> &mut Module {
        &mut self.module
    }

    pub(crate) fn generated_mut(&mut self) -> &mut HashMap<Instance, StaticValue> {
        &mut self.generated
    }

    pub(crate) fn active_mut(&mut self) -> &mut HashSet<Instance> {
        &mut self.active
    }

    pub(crate) fn deferral_mut(&mut self) -> &mut Deferral {
        &mut self.deferral
    }

    pub(crate) fn reentered_mut(&mut self) -> &mut HashSet<Instance> {
        &mut self.reentered
    }

    pub(crate) fn signatures_mut(&mut self) -> &mut HashMap<Instance, Rc<Signature>> {
        &mut self.signatures
    }

    pub(crate) fn global_types_mut(&mut self) -> &mut HashMap<DefKey, HMIRTypeID> {
        &mut self.global_types
    }

    pub(crate) fn next_serial(&mut self) -> u64 {
        self.serial += 1;
        self.serial
    }

    pub(crate) fn into_parts(self) -> (TypeTable, Module) {
        (self.types, self.module)
    }

    pub(crate) fn resolve(
        &mut self,
        unit: UnitID,
        def: &HMIRDefRef,
        span: &TokenRange,
    ) -> CXResult<DefKey> {
        match def {
            HMIRDefRef::Local(id) => Ok(DefKey::new(unit, *id)),
            HMIRDefRef::External(name) => self
                .external(name)
                .ok_or_else(|| staging_error(span, &typecheck::UNKNOWN_SYMBOL, name.to_string())),
            HMIRDefRef::Candidates(candidates) => {
                let mut keys = Vec::new();
                for candidate in candidates {
                    let key = self.resolve(unit, candidate, span)?;
                    if !keys.contains(&key) {
                        keys.push(key);
                    }
                }
                equivalent_def(self, &keys, span)
            }
        }
    }

    pub(crate) fn external(&mut self, name: &QualifiedName) -> Option<DefKey> {
        if let Some(key) = self.externals.get(name) {
            return *key;
        }
        let key = match self.names[self.main_unit().index()].get(name) {
            Some(id) => Some(DefKey::new(self.main_unit(), *id)),
            None => (self.loader)(name).map(|unit| self.adopt_external(unit)),
        };
        self.externals.insert(name.clone(), key);
        key
    }

    // The same def can be reached under several spellings (with or without a tag, through aliases)
    fn adopt_external(&mut self, unit: HMIRUnit) -> DefKey {
        let canonical = unit.def(HMIRDefID::new(0)).name().clone();
        if let Some(id) = self.names[self.main_unit().index()].get(&canonical) {
            return DefKey::new(self.main_unit(), *id);
        }
        if let Some(Some(key)) = self.externals.get(&canonical) {
            return *key;
        }
        let key = DefKey::new(self.push_unit(unit), HMIRDefID::new(0));
        self.externals.insert(canonical, Some(key));
        key
    }

    pub(crate) fn import_type(
        &mut self,
        unit: UnitID,
        ty: HMIRTypeID,
        span: &TokenRange,
    ) -> CXResult<HMIRTypeID> {
        if let Some(id) = self.imported.get(&(unit, ty)) {
            return Ok(*id);
        }
        let desc = self.units[unit.index()].types().get(ty).clone();
        let kind = match desc {
            HMIRTypeDesc::Void => HMIRTypeKind::Void,
            HMIRTypeDesc::Unreachable => HMIRTypeKind::Unreachable,
            HMIRTypeDesc::Type => HMIRTypeKind::Type,
            HMIRTypeDesc::Str => HMIRTypeKind::Str,
            HMIRTypeDesc::Int { width, signed } => HMIRTypeKind::Int { width, signed },
            HMIRTypeDesc::Float { width } => HMIRTypeKind::Float { width },
            HMIRTypeDesc::Pointer(inner) => {
                HMIRTypeKind::PointerTo(self.import_type(unit, inner, span)?)
            }
            HMIRTypeDesc::Reference(inner) => {
                HMIRTypeKind::ReferenceTo(self.import_type(unit, inner, span)?)
            }
            HMIRTypeDesc::Array { element, length } => HMIRTypeKind::Array {
                element: self.import_type(unit, element, span)?,
                length,
            },
            HMIRTypeDesc::Function(function) => {
                HMIRTypeKind::Function(self.import_function_type(unit, &function, span)?)
            }
            HMIRTypeDesc::Expr { params, result } => HMIRTypeKind::StagedExpr {
                params: params
                    .iter()
                    .map(|param| self.import_type(unit, *param, span))
                    .collect::<CXResult<_>>()?,
                result: self.import_type(unit, result, span)?,
            },
            HMIRTypeDesc::Opaque { size, alignment } => HMIRTypeKind::Opaque { size, alignment },
            HMIRTypeDesc::Nominal(_) => {
                return Err(staging_error(span, &mir::UNRESOLVED_NOMINAL, ()));
            }
        };
        let id = self.types.intern(kind);
        self.imported.insert((unit, ty), id);
        Ok(id)
    }

    fn import_function_type(
        &mut self,
        unit: UnitID,
        function: &HMIRFnTypeDesc,
        span: &TokenRange,
    ) -> CXResult<FunctionType> {
        let params = function
            .params()
            .iter()
            .map(|param| self.import_type(unit, *param, span))
            .collect::<CXResult<_>>()?;
        let ret = self.import_type(unit, function.ret(), span)?;
        Ok(FunctionType::new(params, ret, function.is_variadic()))
    }

    pub(crate) fn import_constant(
        &mut self,
        unit: UnitID,
        constant: &HMIRConstant,
        span: &TokenRange,
    ) -> CXResult<StaticValue> {
        Ok(match constant {
            HMIRConstant::Unit => StaticValue::Unit,
            HMIRConstant::Bool(value) => StaticValue::bool(*value, &mut self.types),
            HMIRConstant::Int { value, ty } => StaticValue::Int {
                value: *value,
                ty: self.import_type(unit, *ty, span)?,
            },
            HMIRConstant::Float { value, ty } => StaticValue::Float {
                value: *value,
                ty: self.import_type(unit, *ty, span)?,
            },
            HMIRConstant::Str(value) => StaticValue::Str(value.clone()),
            HMIRConstant::Null(ty) => StaticValue::Null(self.import_type(unit, *ty, span)?),
            HMIRConstant::Type(ty) => StaticValue::Type(self.import_type(unit, *ty, span)?),
        })
    }
}
