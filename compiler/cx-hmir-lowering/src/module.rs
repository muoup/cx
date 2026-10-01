use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};

use cx_hmir::HMIRDefKind;
use cx_log::CXResult;
use cx_mir::{
    MIRBody, MIRConstant, MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunction, MIRFunctionID,
    MIRGlobalID, MIRGlobalRef, MIRGlobalState, MIRGlobalVariable, MIRUnit,
    constant::MIRStagedExprPool, ty::registry::MIRTypeRegistry,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    program::{DefKey, Instance, Program},
    staging_error,
    ty::{TypeID, TypeKind, TypeTable},
    value::StaticValue,
};

#[derive(Default)]
pub(crate) struct Module {
    functions: BTreeMap<MIRFunctionID, MIRFunction>,
    function_symbols: HashMap<String, MIRFunctionID>,
    instances: HashMap<Instance, MIRFunctionID>,
    queued: HashSet<MIRFunctionID>,
    pending: VecDeque<(Instance, MIRFunctionID)>,
    used_functions: HashSet<MIRFunctionID>,

    globals: BTreeMap<MIRGlobalID, MIRGlobalVariable>,
    global_symbols: HashMap<String, MIRGlobalID>,
    global_defs: HashMap<DefKey, MIRGlobalID>,
    global_order: Vec<MIRGlobalID>,
    used_globals: HashSet<MIRGlobalID>,
}

impl Module {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn function(&self, id: MIRFunctionID) -> &MIRFunction {
        &self.functions[&id]
    }

    pub(crate) fn define_function(&mut self, id: MIRFunctionID, body: MIRBody) {
        if let Some(function) = self.functions.get_mut(&id) {
            function.define(body);
        }
    }

    pub(crate) fn next_pending(&mut self) -> Option<(Instance, MIRFunctionID)> {
        self.pending.pop_front()
    }

    pub(crate) fn use_function(&mut self, id: MIRFunctionID) {
        self.used_functions.insert(id);
    }

    pub(crate) fn use_global(&mut self, id: MIRGlobalID) {
        self.used_globals.insert(id);
    }

    pub(crate) fn finish(self, types: MIRTypeRegistry) -> MIRUnit<'static> {
        let used_functions = self.used_functions;
        let functions = self
            .functions
            .into_iter()
            .filter(|(id, function)| {
                used_functions.contains(id)
                    || (function.body().is_some()
                        && function.prototype().linkage != LinkageMode::Static)
            })
            .collect();
        let used_globals = self.used_globals;
        let globals: BTreeMap<_, _> = self
            .globals
            .into_iter()
            .filter(|(id, global)| {
                global.linkage() != LinkageMode::Static || used_globals.contains(id)
            })
            .collect();
        let global_order = self
            .global_order
            .into_iter()
            .filter(|id| globals.contains_key(id))
            .collect();
        MIRUnit::new(
            types,
            functions,
            BTreeMap::new(),
            MIRStagedExprPool::new(),
            globals,
            global_order,
        )
    }
}

impl Program<'_> {
    pub(crate) fn lower_roots(&mut self) -> CXResult<()> {
        let main = self.main_unit();
        let unit = self.unit(main);
        for (id, def) in unit.defs() {
            let key = DefKey::new(main, id);
            match def.kind() {
                HMIRDefKind::Function(function)
                    if function.root().is_some()
                        && !function.has_comptime_params()
                        && function.signature().linkage() != LinkageMode::Static =>
                {
                    self.declare_function(&(key, Vec::new()), def.span())?;
                }
                HMIRDefKind::Global(global) if global.linkage() != LinkageMode::Extern => {
                    self.declare_global(key, def.span())?;
                }
                _ => {}
            }
        }
        while let Some((instance, id)) = self.module_mut().next_pending() {
            let body = self.lower_function(&instance, id)?;
            self.module_mut().define_function(id, body);
        }
        Ok(())
    }

    pub(crate) fn finish(self) -> MIRUnit<'static> {
        let (types, module) = self.into_parts();
        module.finish(types.finish())
    }

    fn instance_symbol(&mut self, instance: &Instance, link_name: &CXIdent) -> String {
        if instance.1.is_empty() {
            return link_name.as_string();
        }
        let args = instance
            .1
            .iter()
            .map(|arg| self.mangle_static(arg))
            .collect::<String>();
        format!("_T{}_{}_{}_", link_name.as_str().len(), link_name, args)
    }

    fn mangle_static(&mut self, value: &StaticValue) -> String {
        match value {
            StaticValue::Type(ty) => self.types().mangle(*ty),
            StaticValue::Int { value, .. } => format!("I{value}"),
            other => format!("V{:x}", {
                use std::hash::{Hash, Hasher};
                let mut hasher = std::collections::hash_map::DefaultHasher::new();
                other.hash(&mut hasher);
                hasher.finish()
            }),
        }
    }

    pub(crate) fn declare_function(
        &mut self,
        instance: &Instance,
        span: &TokenRange,
    ) -> CXResult<MIRFunctionID> {
        if let Some(id) = self.module().instances.get(instance) {
            return Ok(*id);
        }
        let signature = self.signature(instance, span)?;
        let symbol = self.instance_symbol(instance, signature.link_name());
        let linkage = if instance.1.is_empty() {
            signature.linkage()
        } else {
            LinkageMode::Static
        };

        let id = match self.module().function_symbols.get(&symbol) {
            Some(id) => *id,
            None => {
                let mut params = Vec::with_capacity(signature.params().len());
                for (name, ty) in signature.params() {
                    let mir = self.types_mut().mir(*ty, span)?;
                    let nodrop = self.types().is_nodrop(*ty);
                    params.push(MIRFnParam::new(name.clone(), mir, nodrop));
                }
                let ret = self.types_mut().mir(signature.ret(), span)?;
                let prototype = MIRFnPrototype::new(
                    MIRFnSignature::new(params, ret, signature.is_variadic(), false),
                    linkage,
                    CXIdent::from(symbol.as_str()),
                    Some(CXIdent::from(self.def_name(instance.0).to_string())),
                );
                let module = self.module_mut();
                let id = MIRFunctionID::new(module.functions.len());
                module
                    .functions
                    .insert(id, MIRFunction::new(prototype, None));
                module.function_symbols.insert(symbol, id);
                id
            }
        };
        let unit = self.unit(instance.0.unit());
        let has_root = match unit.def(instance.0.def()).kind() {
            HMIRDefKind::Function(function) => function.root().is_some(),
            _ => false,
        };
        let owned = instance.0.unit() == self.main_unit() || !instance.1.is_empty();
        let module = self.module_mut();
        module.instances.insert(instance.clone(), id);
        if has_root && owned && module.queued.insert(id) {
            module.pending.push_back((instance.clone(), id));
        }
        Ok(id)
    }

    pub(crate) fn declare_global(
        &mut self,
        key: DefKey,
        span: &TokenRange,
    ) -> CXResult<MIRGlobalID> {
        if let Some(id) = self.module().global_defs.get(&key) {
            return Ok(*id);
        }
        let unit = self.unit(key.unit());
        let def = unit.def(key.def());
        let HMIRDefKind::Global(global) = def.kind() else {
            return Err(staging_error(
                span,
                format!("'{}' is not a global", def.name()),
            ));
        };
        let ty = self.global_type(key, span)?;
        let mir = self.types_mut().mir(ty, span)?;
        let symbol = global.link_name().as_string();

        let id = match self.module().global_symbols.get(&symbol) {
            Some(id) => *id,
            None => {
                let module = self.module_mut();
                let id = MIRGlobalID::new(module.global_order.len());
                module.global_order.push(id);
                module.global_symbols.insert(symbol, id);
                module.globals.insert(
                    id,
                    MIRGlobalVariable::new(
                        global.link_name().clone(),
                        global.linkage(),
                        mir,
                        MIRGlobalState::External,
                        global.is_mutable(),
                    ),
                );
                id
            }
        };
        self.module_mut().global_defs.insert(key, id);

        // A tentative C definition is replaced by a later initialized one
        let tentative = global.initializer().is_some()
            && matches!(
                self.module().globals[&id].state(),
                MIRGlobalState::ZeroInitialized
            );
        let defined = !matches!(self.module().globals[&id].state(), MIRGlobalState::External);
        if global.linkage() == LinkageMode::Extern || (defined && !tentative) {
            return Ok(id);
        }
        let state = match global.initializer() {
            Some(initializer) => {
                let value = self.eval_global_initializer(key, initializer, ty, span)?;
                MIRGlobalState::Initialized(self.to_constant(&value, ty, span)?)
            }
            None => MIRGlobalState::ZeroInitialized,
        };
        let module = self.module_mut();
        let variable = module.globals.get_mut(&id).expect("declared global");
        if tentative
            || (global.linkage() == LinkageMode::Standard
                && variable.linkage() == LinkageMode::Extern)
        {
            *variable = MIRGlobalVariable::new(
                global.link_name().clone(),
                global.linkage(),
                mir,
                state,
                global.is_mutable(),
            );
        } else {
            variable.define(state);
        }
        Ok(id)
    }

    pub(crate) fn global_ref(&mut self, key: DefKey, span: &TokenRange) -> CXResult<MIRGlobalRef> {
        let global = self.declare_global(key, span)?;
        self.module_mut().use_global(global);
        let ty = self.global_type(key, span)?;
        Ok(MIRGlobalRef {
            global,
            offset: 0,
            ty: self.types_mut().mir(ty, span)?,
        })
    }

    pub(crate) fn to_constant(
        &mut self,
        value: &StaticValue,
        ty: TypeID,
        span: &TokenRange,
    ) -> CXResult<MIRConstant> {
        let value = self.coerce_static(value.clone(), ty, span)?;
        Ok(match value {
            StaticValue::Unit => MIRConstant::Unit,
            StaticValue::Int { value, ty } => match self.types().kind(ty).clone() {
                TypeKind::Pointer(_) if value == 0 => MIRConstant::Nullptr {
                    ty: self.types_mut().mir(ty, span)?,
                },
                _ => {
                    let (width, _) = self.types().int_info(ty).ok_or_else(|| {
                        staging_error(span, "integer constant of a non-integer type".into())
                    })?;
                    MIRConstant::Integer {
                        ty: TypeTable::mir_int(width),
                        value,
                    }
                }
            },
            StaticValue::Float { value, ty } => match self.types().kind(ty) {
                TypeKind::Float { width } => MIRConstant::Float {
                    value,
                    ty: TypeTable::mir_float(*width),
                },
                _ => {
                    return Err(staging_error(
                        span,
                        "float constant of a non-float type".into(),
                    ));
                }
            },
            StaticValue::Str(string) => match self.types().kind(ty).clone() {
                TypeKind::Array { element, length } => {
                    let length = length.unwrap_or(string.len() as u64 + 1) as usize;
                    let int = self
                        .types()
                        .int_info(element)
                        .map(|(width, _)| TypeTable::mir_int(width))
                        .unwrap_or(cx_mir::MIRIntType::I8);
                    let mut fields = string
                        .bytes()
                        .take(length)
                        .enumerate()
                        .map(|(index, byte)| {
                            (
                                index,
                                MIRConstant::Integer {
                                    ty: int,
                                    value: byte as i128,
                                },
                            )
                        })
                        .collect::<Vec<_>>();
                    if string.len() < length {
                        fields.push((string.len(), MIRConstant::Integer { ty: int, value: 0 }));
                    }
                    MIRConstant::Aggregate {
                        ty: self.types_mut().mir(ty, span)?,
                        fields,
                    }
                }
                _ => MIRConstant::String(string),
            },
            StaticValue::Null(ty) => MIRConstant::Nullptr {
                ty: self.types_mut().mir(ty, span)?,
            },
            StaticValue::Function { def, args } => {
                let id = self.declare_function(&(def, args), span)?;
                self.module_mut().use_function(id);
                MIRConstant::Function(id)
            }
            StaticValue::GlobalAddress { def, offset, .. } => {
                let mut global = self.global_ref(def, span)?;
                global.offset = offset;
                MIRConstant::GlobalRef(global)
            }
            StaticValue::Aggregate { ty, fields } => {
                let mut constants = Vec::with_capacity(fields.len());
                for (index, field) in fields {
                    let field_ty = self.member_type(ty, index, span)?;
                    constants.push((index, self.to_constant(&field, field_ty, span)?));
                }
                MIRConstant::Aggregate {
                    ty: self.types_mut().mir(ty, span)?,
                    fields: constants,
                }
            }
            StaticValue::Type(_) | StaticValue::Quote(_) | StaticValue::Global(_) => {
                return Err(staging_error(
                    span,
                    "comptime-only value used as a runtime constant".into(),
                ));
            }
        })
    }

    // The type of the 'index'th member of an aggregate: a field, an array element, or a variant
    pub(crate) fn member_type(
        &mut self,
        ty: TypeID,
        index: usize,
        span: &TokenRange,
    ) -> CXResult<TypeID> {
        match self.types().kind(ty).clone() {
            TypeKind::Array { element, .. } => Ok(element),
            TypeKind::Nominal(_) => self
                .types()
                .nominal_of(ty)
                .and_then(|nominal| nominal.fields().get(index))
                .map(|field| field.ty())
                .ok_or_else(|| staging_error(span, format!("no member {index} in aggregate"))),
            _ => Err(staging_error(
                span,
                format!("'{}' is not an aggregate", self.types().display(ty)),
            )),
        }
    }
}
