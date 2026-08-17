use crate::GlobalState;
use crate::typing::{any_to_basic_type, bc_llvm_type, convert_linkage};
use cx_lmir::{LMIRGlobalInitializer, LMIRGlobalState, LMIRGlobalType, LMIRGlobalValue};
use inkwell::module::Linkage;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::ArrayValue;
use std::sync::atomic::AtomicUsize;

fn string_literal_name() -> String {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    let id = COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    format!(".str_{id}")
}

pub(crate) fn generate_global_variable(
    state: &mut GlobalState,
    variable: &LMIRGlobalValue,
) -> Option<()> {
    match &variable._type {
        LMIRGlobalType::StringLiteral(str) => {
            let val = state.context.const_string(str.as_bytes(), true);

            let global =
                state
                    .module
                    .add_global(val.get_type(), None, string_literal_name().as_str());

            global.set_linkage(Linkage::Private);
            global.set_initializer(&val);
            global.set_unnamed_addr(true);
            global.set_constant(true);

            state.globals.push(global);
        }

        LMIRGlobalType::Variable {
            _type,
            state: global_state,
        } => {
            let basic_type = match global_state {
                LMIRGlobalState::Initialized(initializer) => {
                    global_llvm_type(state, _type, &[initializer])
                }
                LMIRGlobalState::ZeroInitialized | LMIRGlobalState::External => {
                    let llvm_type = bc_llvm_type(state.context, _type)?;
                    any_to_basic_type(llvm_type)
                        .unwrap_or_else(|| panic!("Unsupported global variable type"))
                }
            };

            let global = get_global(state, basic_type, variable.name.as_str(), global_state);

            if matches!(global_state, LMIRGlobalState::External) {
                global.set_linkage(Linkage::External);
            } else {
                if matches!(variable.linkage, cx_lmir::LinkageType::Static) {
                    global.set_linkage(convert_linkage(variable.linkage));
                }
                let initializer = match global_state {
                    LMIRGlobalState::ZeroInitialized => basic_type.const_zero(),
                    LMIRGlobalState::Initialized(initializer) => {
                        global_initializer(state, basic_type, initializer)
                    }
                    LMIRGlobalState::External => unreachable!(),
                };
                global.set_initializer(&initializer);
            }

            state.globals.push(global);
        }
    }

    Some(())
}

fn get_global<'ctx>(
    state: &mut GlobalState<'ctx>,
    basic_type: BasicTypeEnum<'ctx>,
    name: &str,
    global_state: &LMIRGlobalState,
) -> inkwell::values::GlobalValue<'ctx> {
    let Some(existing) = state.module.get_global(name) else {
        return state.module.add_global(basic_type, None, name);
    };

    if matches!(global_state, LMIRGlobalState::External)
        || existing.get_initializer().is_some()
    {
        return if matches!(global_state, LMIRGlobalState::External) {
            existing
        } else {
            state.module.add_global(basic_type, None, name)
        };
    }

    let replacement = state.module.add_global(basic_type, None, name);
    existing
        .as_pointer_value()
        .replace_all_uses_with(replacement.as_pointer_value());
    for global in &mut state.globals {
        if global.get_name().to_bytes() == name.as_bytes() {
            *global = replacement;
        }
    }
    unsafe { existing.delete() };
    replacement.set_name(name);
    replacement
}

fn global_llvm_type<'ctx>(
    state: &GlobalState<'ctx>,
    _type: &cx_lmir::types::LMIRType,
    initializers: &[&LMIRGlobalInitializer],
) -> BasicTypeEnum<'ctx> {
    let base_type = || {
        let llvm_type = bc_llvm_type(state.context, _type)
            .unwrap_or_else(|| panic!("Unsupported global variable type"));
        any_to_basic_type(llvm_type).unwrap_or_else(|| panic!("Unsupported global variable type"))
    };

    if !initializers
        .iter()
        .any(|initializer| has_function_pointer_initializer(_type, initializer))
    {
        return base_type();
    }

    match &_type.kind {
        cx_lmir::types::LMIRTypeKind::Opaque { bytes }
            if *bytes == state.architecture.pointer_size() =>
        {
            state
                .context
                .ptr_type(inkwell::AddressSpace::from(0))
                .into()
        }
        cx_lmir::types::LMIRTypeKind::Array { element, size } => {
            let element_initializers = initializers
                .iter()
                .flat_map(|initializer| match initializer {
                    LMIRGlobalInitializer::Aggregate { fields } => fields
                        .iter()
                        .filter(|(index, _)| *index < *size)
                        .map(|(_, initializer)| initializer)
                        .collect::<Vec<_>>(),
                    _ => Vec::new(),
                })
                .collect::<Vec<_>>();
            global_llvm_type(state, element, &element_initializers)
                .array_type(*size as u32)
                .into()
        }
        cx_lmir::types::LMIRTypeKind::Struct { fields, .. } => {
            let field_types = fields
                .iter()
                .enumerate()
                .map(|(index, (_, field_type))| {
                    let field_initializers = initializers
                        .iter()
                        .filter_map(|initializer| match initializer {
                            LMIRGlobalInitializer::Aggregate { fields } => fields
                                .iter()
                                .find(|(field_index, _)| *field_index == index)
                                .map(|(_, initializer)| initializer),
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    global_llvm_type(state, field_type, &field_initializers)
                })
                .collect::<Vec<_>>();
            state.context.struct_type(&field_types, false).into()
        }
        _ => base_type(),
    }
}

fn has_function_pointer_initializer(
    _type: &cx_lmir::types::LMIRType,
    initializer: &LMIRGlobalInitializer,
) -> bool {
    match (&_type.kind, initializer) {
        (cx_lmir::types::LMIRTypeKind::Opaque { .. }, LMIRGlobalInitializer::Function(_)) => true,
        (
            cx_lmir::types::LMIRTypeKind::Opaque { .. },
            LMIRGlobalInitializer::Aggregate { fields },
        ) => fields.iter().any(|(index, initializer)| {
            *index == 0 && matches!(initializer, LMIRGlobalInitializer::Function(_))
        }),
        (
            cx_lmir::types::LMIRTypeKind::Array { element, size },
            LMIRGlobalInitializer::Aggregate { fields },
        ) => fields.iter().any(|(index, initializer)| {
            *index < *size && has_function_pointer_initializer(element, initializer)
        }),
        (
            cx_lmir::types::LMIRTypeKind::Struct { fields, .. },
            LMIRGlobalInitializer::Aggregate {
                fields: initializers,
            },
        ) => initializers.iter().any(|(index, initializer)| {
            fields.get(*index).is_some_and(|(_, field_type)| {
                has_function_pointer_initializer(field_type, initializer)
            })
        }),
        _ => false,
    }
}

fn global_initializer<'ctx>(
    state: &GlobalState<'ctx>,
    basic_type: inkwell::types::BasicTypeEnum<'ctx>,
    initializer: &LMIRGlobalInitializer,
) -> inkwell::values::BasicValueEnum<'ctx> {
    match initializer {
        LMIRGlobalInitializer::Integer { value, signed, .. } => basic_type
            .into_int_type()
            .const_int(*value as u64, *signed)
            .into(),
        LMIRGlobalInitializer::Float { value, .. } => basic_type
            .into_float_type()
            .const_float(value.into())
            .into(),
        LMIRGlobalInitializer::Aggregate { fields }
            if matches!(basic_type, inkwell::types::BasicTypeEnum::PointerType(_))
                && fields.len() == 1
                && fields[0].0 == 0 =>
        {
            global_initializer(state, basic_type, &fields[0].1)
        }
        LMIRGlobalInitializer::Aggregate { fields } => match basic_type {
            inkwell::types::BasicTypeEnum::StructType(struct_type) => {
                let values = (0..struct_type.count_fields())
                    .map(|index| {
                        let field_type = struct_type
                            .get_field_type_at_index(index)
                            .expect("struct field index is in bounds");
                        fields
                            .iter()
                            .find(|(field_index, _)| *field_index == index as usize)
                            .map(|(_, initializer)| {
                                global_initializer(state, field_type, initializer)
                            })
                            .unwrap_or_else(|| field_type.const_zero())
                    })
                    .collect::<Vec<_>>();
                struct_type.const_named_struct(&values).into()
            }
            inkwell::types::BasicTypeEnum::ArrayType(array_type) => {
                let element_type = array_type.get_element_type();
                let values = (0..array_type.len())
                    .map(|index| {
                        fields
                            .iter()
                            .find(|(field_index, _)| *field_index == index as usize)
                            .map(|(_, initializer)| {
                                global_initializer(state, element_type, initializer)
                            })
                            .unwrap_or_else(|| element_type.const_zero())
                    })
                    .collect::<Vec<_>>();
                unsafe { ArrayValue::new_const_array(&element_type, &values) }.into()
            }
            _ => panic!("aggregate initializer used with non-aggregate LLVM type"),
        },
        LMIRGlobalInitializer::Global(global) => {
            let pointer_type = basic_type.into_pointer_type();
            let value = state
                .globals
                .get(*global as usize)
                .unwrap_or_else(|| panic!("invalid global initializer reference {global}"))
                .as_pointer_value();
            value.const_cast(pointer_type).into()
        }
        LMIRGlobalInitializer::GlobalOffset { global, offset } => {
            let pointer_type = basic_type.into_pointer_type();
            let value = state
                .globals
                .get(*global as usize)
                .unwrap_or_else(|| panic!("invalid global initializer reference {global}"))
                .as_pointer_value();
            let index = state.context.i64_type().const_int(*offset as u64, true);
            let value = unsafe { value.const_gep(state.context.i8_type(), &[index]) };
            value.const_cast(pointer_type).into()
        }
        LMIRGlobalInitializer::Function(function) => {
            let pointer_type = basic_type.into_pointer_type();
            let value = state
                .module
                .get_function(function)
                .unwrap_or_else(|| panic!("invalid function initializer reference {function}"));
            value
                .as_global_value()
                .as_pointer_value()
                .const_cast(pointer_type)
                .into()
        }
        LMIRGlobalInitializer::Null => match basic_type {
            inkwell::types::BasicTypeEnum::PointerType(pointer) => pointer.const_null().into(),
            _ => basic_type.const_zero(),
        },
    }
}
