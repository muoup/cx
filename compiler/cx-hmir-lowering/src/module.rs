use std::collections::{BTreeMap, HashMap, HashSet, VecDeque, hash_map::Entry};

use cx_hmir::{HMIRAggregateKind, HMIRDefKind, HMIRFunctionStage};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_mir::{
    MIRBody, MIRConstant, MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunction, MIRFunctionID,
    MIRGlobalID, MIRGlobalRef, MIRGlobalState, MIRGlobalVariable, MIRIntType, MIRUnit,
    ty::registry::MIRTypeRegistry,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    eval::{
        check_redeclaration, eval_global_initializer, eval_global_type, eval_signature,
        ops::coerce_static,
    },
    function::lower_function,
    program::{DefKey, Instance, Program},
    staging_error,
    ty::{HMIRTypeID, HMIRTypeKind, TypeTable},
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

    pub(crate) fn finish(self, types: MIRTypeRegistry) -> MIRUnit {
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
        MIRUnit::new(types, functions, globals, global_order)
    }
}

pub(crate) fn lower_roots(cx: &mut Program<'_>) -> CXResult<()> {
    let main = cx.main_unit();
    let unit = cx.unit(main);
    let mut declared = HashMap::new();
    for (id, def) in unit.defs() {
        if let HMIRDefKind::Function(function) = def.kind() {
            crate::eval::check_variadic(function, def.span())?;
        }
        match declared.entry(def.name()) {
            Entry::Vacant(entry) => {
                entry.insert(id);
            }
            Entry::Occupied(entry) => {
                check_redeclaration(
                    cx,
                    DefKey::new(main, *entry.get()),
                    DefKey::new(main, id),
                    def.span(),
                )?;
            }
        }
    }
    for (id, def) in unit.defs() {
        let key = DefKey::new(main, id);
        match def.kind() {
            HMIRDefKind::Function(function)
                if function.root().is_some()
                    && function.stage() == HMIRFunctionStage::Runtime
                    && !function.has_comptime_params()
                    && function.signature().linkage() != LinkageMode::Static =>
            {
                declare_function(cx, &(key, Vec::new()), def.span())?;
            }
            HMIRDefKind::Global(global) if global.linkage() != LinkageMode::Extern => {
                declare_global(cx, key, def.span())?;
            }
            _ => {}
        }
    }
    while let Some((instance, id)) = cx.module_mut().next_pending() {
        let body = lower_function(cx, &instance, id)?;
        cx.module_mut().define_function(id, body);
    }
    Ok(())
}

fn instance_symbol(cx: &mut Program<'_>, instance: &Instance, link_name: &CXIdent) -> String {
    if instance.1.is_empty() {
        return link_name.as_string();
    }
    let args = instance
        .1
        .iter()
        .map(|arg| mangle_static(cx, arg))
        .collect::<String>();
    format!("_T{}_{}_{}_", link_name.as_str().len(), link_name, args)
}

fn mangle_static(cx: &mut Program<'_>, value: &StaticValue) -> String {
    match value {
        StaticValue::Type(ty) => cx.types().mangle(*ty),
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
    cx: &mut Program<'_>,
    instance: &Instance,
    span: &TokenRange,
) -> CXResult<MIRFunctionID> {
    let unit = cx.unit(instance.0.unit());
    let def = unit.def(instance.0.def());
    if let HMIRDefKind::Function(function) = def.kind()
        && function.stage() == HMIRFunctionStage::Comptime
    {
        return Err(staging_error(
            span,
            &mir::COMPTIME_FUNCTION_AT_RUNTIME,
            def.name().to_string(),
        ));
    }
    if let Some(id) = cx.module().instances.get(instance) {
        return Ok(*id);
    }
    let safe = matches!(
        def.kind(),
        HMIRDefKind::Function(function) if function.signature().contract().is_safe()
    );
    let signature = eval_signature(cx, instance, span)?;
    let symbol = instance_symbol(cx, instance, signature.link_name());
    let linkage = if instance.1.is_empty() {
        signature.linkage()
    } else {
        LinkageMode::Static
    };

    let id = match cx.module().function_symbols.get(&symbol) {
        Some(id) => *id,
        None => {
            let mut params = Vec::with_capacity(signature.params().len());
            for (name, ty) in signature.params() {
                let mir = cx.types_mut().mir(*ty, span)?;
                let nodrop = cx.types().is_nodrop(*ty);
                params.push(MIRFnParam::new(name.clone(), mir, nodrop));
            }
            let ret = cx.types_mut().mir(signature.ret(), span)?;
            let prototype = MIRFnPrototype::new(
                MIRFnSignature::new(params, ret, signature.is_variadic(), safe),
                linkage,
                CXIdent::from(symbol.as_str()),
                Some(CXIdent::from(cx.def_name(instance.0).to_string())),
            );
            let module = cx.module_mut();
            let id = MIRFunctionID::new(module.functions.len());
            module
                .functions
                .insert(id, MIRFunction::new(prototype, None));
            module.function_symbols.insert(symbol, id);
            id
        }
    };
    let unit = cx.unit(instance.0.unit());
    let has_root = match unit.def(instance.0.def()).kind() {
        HMIRDefKind::Function(function) => function.root().is_some(),
        _ => false,
    };
    let owned = instance.0.unit() == cx.main_unit() || !instance.1.is_empty();
    let module = cx.module_mut();
    module.instances.insert(instance.clone(), id);
    if has_root && owned && module.queued.insert(id) {
        module.pending.push_back((instance.clone(), id));
    }
    Ok(id)
}

pub(crate) fn declare_global(
    cx: &mut Program<'_>,
    key: DefKey,
    span: &TokenRange,
) -> CXResult<MIRGlobalID> {
    if let Some(id) = cx.module().global_defs.get(&key) {
        return Ok(*id);
    }
    let unit = cx.unit(key.unit());
    let def = unit.def(key.def());
    let HMIRDefKind::Global(global) = def.kind() else {
        return Err(staging_error(
            span,
            &typecheck::UNEXPECTED_KIND,
            (format!("'{}'", def.name()), "a global".into()),
        ));
    };
    let ty = eval_global_type(cx, key, span)?;
    if global.linkage() != LinkageMode::Extern
        && let Some(problem) = cx.types().object_problem(ty)
    {
        return Err(staging_error(
            span,
            &typecheck::INVALID_OBJECT_TYPE,
            (format!("'{}'", def.name()), problem.into()),
        ));
    }
    let mir = cx.types_mut().mir(ty, span)?;
    let symbol = global.link_name().as_string();

    let id = match cx.module().global_symbols.get(&symbol) {
        Some(id) => *id,
        None => {
            let module = cx.module_mut();
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
    cx.module_mut().global_defs.insert(key, id);

    // A tentative C definition is replaced by a later initialized one
    let tentative = global.initializer().is_some()
        && matches!(
            cx.module().globals[&id].state(),
            MIRGlobalState::ZeroInitialized
        );
    let defined = !matches!(cx.module().globals[&id].state(), MIRGlobalState::External);
    if global.linkage() == LinkageMode::Extern || (defined && !tentative) {
        return Ok(id);
    }
    let state = match global.initializer() {
        Some(initializer) => {
            let value = eval_global_initializer(cx, key, initializer, ty, span)?;
            MIRGlobalState::Initialized(to_constant(cx, &value, ty, span)?)
        }
        None => MIRGlobalState::ZeroInitialized,
    };
    let module = cx.module_mut();
    let variable = module.globals.get_mut(&id).expect("declared global");
    if tentative
        || (global.linkage() == LinkageMode::Standard && variable.linkage() == LinkageMode::Extern)
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

pub(crate) fn global_ref(
    cx: &mut Program<'_>,
    key: DefKey,
    span: &TokenRange,
) -> CXResult<MIRGlobalRef> {
    let global = declare_global(cx, key, span)?;
    cx.module_mut().use_global(global);
    let ty = eval_global_type(cx, key, span)?;
    Ok(MIRGlobalRef {
        global,
        offset: 0,
        ty: cx.types_mut().mir(ty, span)?,
    })
}

pub(crate) fn to_constant(
    cx: &mut Program<'_>,
    value: &StaticValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRConstant> {
    let value = coerce_static(cx, value.clone(), ty, span)?;
    Ok(match value {
        StaticValue::Unit => MIRConstant::Unit,
        StaticValue::Int { value, ty } => match cx.types().kind(ty).clone() {
            HMIRTypeKind::PointerTo(_) if value == 0 => MIRConstant::Nullptr {
                ty: cx.types_mut().mir(ty, span)?,
            },
            _ => {
                let (width, _) = cx.types().int_info(ty).ok_or_else(|| {
                    staging_error(
                        span,
                        &mir::CONSTANT_TYPE,
                        ("integer".into(), cx.types().display(ty)),
                    )
                })?;
                MIRConstant::Integer {
                    ty: TypeTable::mir_int(width),
                    value,
                }
            }
        },
        StaticValue::Float { value, ty } => match cx.types().kind(ty) {
            HMIRTypeKind::Float { width } => MIRConstant::Float {
                value,
                ty: TypeTable::mir_float(*width),
            },
            _ => {
                return Err(staging_error(
                    span,
                    &mir::CONSTANT_TYPE,
                    ("float".into(), cx.types().display(ty)),
                ));
            }
        },
        StaticValue::Str(string) => match cx.types().kind(ty).clone() {
            HMIRTypeKind::Array { element, length } => {
                let length = length.unwrap_or(string.len() as u64 + 1) as usize;
                let int = cx
                    .types()
                    .int_info(element)
                    .map(|(width, _)| TypeTable::mir_int(width))
                    .unwrap_or(MIRIntType::I8);
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
                    ty: cx.types_mut().mir(ty, span)?,
                    fields,
                }
            }
            _ => MIRConstant::String(string),
        },
        StaticValue::Null(ty) => MIRConstant::Nullptr {
            ty: cx.types_mut().mir(ty, span)?,
        },
        StaticValue::Function { def, args } => {
            let id = declare_function(cx, &(def, args), span)?;
            cx.module_mut().use_function(id);
            MIRConstant::Function(id)
        }
        StaticValue::LabelAddress {
            function, label, ..
        } => {
            let function = declare_function(cx, &(function, Vec::new()), span)?;
            cx.module_mut().use_function(function);
            MIRConstant::BlockAddress {
                function,
                label: label.to_string(),
            }
        }
        StaticValue::GlobalAddress { def, offset, .. } => {
            let mut global = global_ref(cx, def, span)?;
            global.offset = offset;
            MIRConstant::GlobalRef(global)
        }
        StaticValue::Aggregate { ty, fields } => {
            let mut constants = Vec::with_capacity(fields.len());
            for (index, field) in fields {
                let field_ty = member_type(cx, ty, index, span)?;
                constants.push((index, to_constant(cx, &field, field_ty, span)?));
            }
            MIRConstant::Aggregate {
                ty: cx.types_mut().mir(ty, span)?,
                fields: constants,
            }
        }
        StaticValue::Type(_) | StaticValue::Quote(_) | StaticValue::Global(_) => {
            return Err(staging_error(span, &mir::COMPTIME_VALUE_AT_RUNTIME, ()));
        }
    })
}

// The position of the variant a pattern names within the sum it is matched against
pub(crate) fn variant_index(
    cx: &Program<'_>,
    ty: HMIRTypeID,
    name: &CXIdent,
    span: &TokenRange,
) -> CXResult<usize> {
    let Some(nominal) = cx
        .types()
        .nominal_of(ty)
        .filter(|nominal| nominal.kind() == HMIRAggregateKind::TaggedUnion)
    else {
        return Err(staging_error(
            span,
            &typecheck::TYPE_REQUIREMENT,
            (
                format!("variant pattern '{name}'"),
                "a tagged union".into(),
                Some(format!("'{}'", cx.types().display(ty))),
            ),
        ));
    };
    nominal
        .fields()
        .iter()
        .position(|field| field.name() == Some(name))
        .ok_or_else(|| {
            staging_error(
                span,
                &typecheck::UNKNOWN_VARIANT,
                (cx.types().display(ty), name.to_string()),
            )
        })
}

// The type of the 'index'th member of an aggregate: a field, an array element, or a variant
pub(crate) fn member_type(
    cx: &mut Program<'_>,
    ty: HMIRTypeID,
    index: usize,
    span: &TokenRange,
) -> CXResult<HMIRTypeID> {
    if let Some(element) = cx.types().array_inner(ty) {
        return Ok(element);
    }
    cx.types()
        .nominal_of(ty)
        .and_then(|nominal| nominal.fields().get(index))
        .map(|field| field.ty())
        .ok_or_else(|| {
            staging_error(
                span,
                &typecheck::UNEXPECTED_KIND,
                (format!("'{}'", cx.types().display(ty)), "an aggregate".into()),
            )
        })
}
