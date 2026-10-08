use crate::GlobalState;
use crate::log::{LLVMError, LLVMResult};
use crate::typing::{any_to_basic_type, bc_llvm_type, convert_linkage};
use cx_lmir::types::{LMIRType, LMIRTypeKind};
use cx_lmir::{LMIRGlobalInitializer, LMIRGlobalState, LMIRGlobalType, LMIRGlobalValue, LinkageType};
use cx_log::catalogue::backend as catalogue;
use inkwell::AddressSpace;
use inkwell::module::Linkage;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{ArrayValue, BasicValueEnum, GlobalValue};
use std::sync::atomic::{AtomicUsize, Ordering};

fn string_literal_name() -> String {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    format!(".str_{id}")
}

pub(crate) fn declare_global_variable(
    state: &mut GlobalState,
    variable: &LMIRGlobalValue,
) -> LLVMResult<()> {
    match &variable.ty {
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
            ty,
            state: global_state,
        } => {
            let basic_type = match global_state {
                LMIRGlobalState::Initialized(initializer) => {
                    global_llvm_type(state, ty, &[initializer])?
                }
                LMIRGlobalState::ZeroInitialized | LMIRGlobalState::External => {
                    let llvm_type = bc_llvm_type(state.context, ty)?;
                    any_to_basic_type(llvm_type)?
                }
            };

            let global = get_global(state, basic_type, variable.name.as_str(), global_state);

            if matches!(global_state, LMIRGlobalState::External) {
                global.set_linkage(Linkage::External);
            } else if matches!(variable.linkage, LinkageType::Static) {
                global.set_linkage(convert_linkage(variable.linkage));
            }

            state.globals.push(global);
        }
    }

    Ok(())
}

pub(crate) fn define_global_variable(
    state: &mut GlobalState,
    index: usize,
    variable: &LMIRGlobalValue,
) -> LLVMResult<()> {
    let LMIRGlobalType::Variable {
        ty,
        state: global_state,
    } = &variable.ty
    else {
        return Ok(());
    };
    if matches!(global_state, LMIRGlobalState::External) {
        return Ok(());
    }

    let basic_type = match global_state {
        LMIRGlobalState::Initialized(initializer) => global_llvm_type(state, ty, &[initializer])?,
        LMIRGlobalState::ZeroInitialized => {
            let llvm_type = bc_llvm_type(state.context, ty)?;
            any_to_basic_type(llvm_type)?
        }
        LMIRGlobalState::External => unreachable!(),
    };
    let global = *state.globals.get(index).ok_or_else(|| {
        LLVMError::new(
            &catalogue::INDEX_BOUNDS,
            ("global definition".into(), format!("{index}")),
        )
    })?;
    let initializer = match global_state {
        LMIRGlobalState::ZeroInitialized => basic_type.const_zero(),
        LMIRGlobalState::Initialized(initializer) if has_overlay(initializer) => {
            flat_initializer(state, ty, initializer)?
        }
        LMIRGlobalState::Initialized(initializer) => {
            global_initializer(state, basic_type, initializer)?
        }
        LMIRGlobalState::External => unreachable!(),
    };
    global.set_initializer(&initializer);
    global.set_alignment(u32::from(ty.alignment()));
    Ok(())
}

fn get_global<'ctx>(
    state: &mut GlobalState<'ctx>,
    basic_type: BasicTypeEnum<'ctx>,
    name: &str,
    global_state: &LMIRGlobalState,
) -> GlobalValue<'ctx> {
    let Some(existing) = state.module.get_global(name) else {
        return state.module.add_global(basic_type, None, name);
    };

    if matches!(global_state, LMIRGlobalState::External) || existing.get_initializer().is_some() {
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
    ty: &LMIRType,
    initializers: &[&LMIRGlobalInitializer],
) -> LLVMResult<BasicTypeEnum<'ctx>> {
    if let [initializer] = initializers
        && has_overlay(initializer)
    {
        return flat_type(state, ty, initializer);
    }

    let base_type = || -> LLVMResult<BasicTypeEnum<'ctx>> {
        let llvm_type = bc_llvm_type(state.context, ty)?;
        any_to_basic_type(llvm_type)
    };

    if !initializers
        .iter()
        .any(|initializer| has_function_pointer_initializer(ty, initializer))
    {
        return base_type();
    }

    match &ty.kind {
        LMIRTypeKind::Opaque { bytes }
            if *bytes == state.architecture.pointer_size()
                && usize::from(ty.alignment) == state.architecture.pointer_alignment() =>
        {
            Ok(state
                .context
                .ptr_type(AddressSpace::from(0))
                .into())
        }
        LMIRTypeKind::Array { element, size } => {
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
            Ok(global_llvm_type(state, element, &element_initializers)?
                .array_type(*size as u32)
                .into())
        }
        LMIRTypeKind::Struct { fields, .. } => {
            let field_types = fields
                .iter()
                .enumerate()
                .map(|(index, (_, field_type))| -> LLVMResult<_> {
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
                .collect::<LLVMResult<Vec<_>>>()?;
            Ok(state.context.struct_type(&field_types, false).into())
        }
        _ => base_type(),
    }
}

pub(crate) fn has_block_address(variable: &LMIRGlobalValue) -> bool {
    fn contains(initializer: &LMIRGlobalInitializer) -> bool {
        match initializer {
            LMIRGlobalInitializer::BlockAddress { .. } => true,
            LMIRGlobalInitializer::Aggregate { fields } => {
                fields.iter().any(|(_, field)| contains(field))
            }
            LMIRGlobalInitializer::Overlay { value, .. } => contains(value),
            _ => false,
        }
    }

    matches!(
        &variable.ty,
        LMIRGlobalType::Variable {
            state: LMIRGlobalState::Initialized(initializer),
            ..
        } if contains(initializer)
    )
}

fn has_function_pointer_initializer(ty: &LMIRType, initializer: &LMIRGlobalInitializer) -> bool {
    match (&ty.kind, initializer) {
        (LMIRTypeKind::Opaque { .. }, LMIRGlobalInitializer::Function(_)) => true,
        (
            LMIRTypeKind::Opaque { .. },
            LMIRGlobalInitializer::Aggregate { fields },
        ) => fields.iter().any(|(index, initializer)| {
            *index == 0 && matches!(initializer, LMIRGlobalInitializer::Function(_))
        }),
        (
            LMIRTypeKind::Array { element, size },
            LMIRGlobalInitializer::Aggregate { fields },
        ) => fields.iter().any(|(index, initializer)| {
            *index < *size && has_function_pointer_initializer(element, initializer)
        }),
        (
            LMIRTypeKind::Struct { fields, .. },
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
    basic_type: BasicTypeEnum<'ctx>,
    initializer: &LMIRGlobalInitializer,
) -> LLVMResult<BasicValueEnum<'ctx>> {
    match initializer {
        LMIRGlobalInitializer::Integer { value, .. } => Ok(basic_type
            .into_int_type()
            .const_int(*value as u64, false)
            .into()),
        LMIRGlobalInitializer::Float { value, .. } => Ok(basic_type
            .into_float_type()
            .const_float(value.into())
            .into()),
        LMIRGlobalInitializer::Aggregate { fields }
            if matches!(basic_type, BasicTypeEnum::PointerType(_))
                && fields.len() == 1
                && fields[0].0 == 0 =>
        {
            Ok(global_initializer(state, basic_type, &fields[0].1)?)
        }
        LMIRGlobalInitializer::Aggregate { fields } => match basic_type {
            BasicTypeEnum::StructType(struct_type) => {
                let initializers = initializers_by_index(fields, struct_type.count_fields() as usize);
                let values = (0..struct_type.count_fields())
                    .map(|index| -> LLVMResult<_> {
                        let field_type =
                            struct_type.get_field_type_at_index(index).ok_or_else(|| {
                                LLVMError::new(
                                    &catalogue::INDEX_BOUNDS,
                                    ("struct field".into(), format!("{index}")),
                                )
                            })?;
                        Ok(initializers[index as usize]
                            .map(|initializer| global_initializer(state, field_type, initializer))
                            .transpose()?
                            .unwrap_or_else(|| field_type.const_zero()))
                    })
                    .collect::<LLVMResult<Vec<_>>>()?;
                Ok(struct_type.const_named_struct(&values).into())
            }
            BasicTypeEnum::ArrayType(array_type) => {
                let element_type = array_type.get_element_type();
                let initializers = initializers_by_index(fields, array_type.len() as usize);
                let values = (0..array_type.len())
                    .map(|index| -> LLVMResult<_> {
                        Ok(initializers[index as usize]
                            .map(|initializer| global_initializer(state, element_type, initializer))
                            .transpose()?
                            .unwrap_or_else(|| element_type.const_zero()))
                    })
                    .collect::<LLVMResult<Vec<_>>>()?;
                Ok(unsafe { ArrayValue::new_const_array(&element_type, &values) }.into())
            }
            _ => Err(LLVMError::new(
                &catalogue::ENTITY_REQUIREMENT,
                (
                    "aggregate initializer".into(),
                    "an aggregate LLVM type".into(),
                    Some(format!("{basic_type:?}")),
                ),
            )),
        },
        LMIRGlobalInitializer::Global(global) => {
            let pointer_type = basic_type.into_pointer_type();
            let value = state
                .globals
                .get(*global as usize)
                .ok_or_else(|| {
                    LLVMError::new(
                        &catalogue::MISSING_ENTITY,
                        (format!("global g{global}"), "LLVM global table".into()),
                    )
                })?
                .as_pointer_value();
            Ok(value.const_cast(pointer_type).into())
        }
        LMIRGlobalInitializer::GlobalOffset { global, offset } => {
            let pointer_type = basic_type.into_pointer_type();
            let value = state
                .globals
                .get(*global as usize)
                .ok_or_else(|| {
                    LLVMError::new(
                        &catalogue::MISSING_ENTITY,
                        (format!("global g{global}"), "LLVM global table".into()),
                    )
                })?
                .as_pointer_value();
            let index = state.context.i64_type().const_int(*offset as u64, true);
            let value = unsafe { value.const_gep(state.context.i8_type(), &[index]) };
            Ok(value.const_cast(pointer_type).into())
        }
        LMIRGlobalInitializer::Function(function) => {
            let pointer_type = basic_type.into_pointer_type();
            let value = state.module.get_function(function).ok_or_else(|| {
                LLVMError::new(
                    &catalogue::MISSING_ENTITY,
                    (
                        format!("function '{function}'"),
                        "LLVM module".into(),
                    ),
                )
            })?;
            Ok(value
                .as_global_value()
                .as_pointer_value()
                .const_cast(pointer_type)
                .into())
        }
        LMIRGlobalInitializer::BlockAddress { function, block } => {
            let address = state
                .module
                .get_function(function)
                .and_then(|function| {
                    function
                        .get_basic_block_iter()
                        .find(|candidate| candidate.get_name().to_bytes() == block.as_str().as_bytes())
                })
                // SAFETY: the address is only ever the operand of an indirect branch.
                .and_then(|block| unsafe { block.get_address() })
                .ok_or_else(|| {
                    LLVMError::new(
                        &catalogue::MISSING_ENTITY,
                        (
                            format!("block '{block}' of function '{function}'"),
                            "LLVM module".into(),
                        ),
                    )
                })?;
            Ok(address.const_cast(basic_type.into_pointer_type()).into())
        }
        LMIRGlobalInitializer::Overlay { .. } => Err(LLVMError::new(
            &catalogue::ENTITY_REQUIREMENT,
            (
                "typed global initializer".into(),
                "no overlaid value".into(),
                None,
            ),
        )),
        LMIRGlobalInitializer::Null => Ok(match basic_type {
            BasicTypeEnum::PointerType(pointer) => pointer.const_null().into(),
            _ => basic_type.const_zero(),
        }),
    }
}

/// A scalar of a global initializer and where it lies in the global.
struct Leaf<'i> {
    offset: usize,
    ty: &'i LMIRType,
    initializer: &'i LMIRGlobalInitializer,
}

fn has_overlay(initializer: &LMIRGlobalInitializer) -> bool {
    match initializer {
        LMIRGlobalInitializer::Overlay { .. } => true,
        LMIRGlobalInitializer::Aggregate { fields } => {
            fields.iter().any(|(_, field)| has_overlay(field))
        }
        _ => false,
    }
}

fn aggregate_field(ty: &LMIRType, index: usize) -> Option<(&LMIRType, usize)> {
    match &ty.kind {
        LMIRTypeKind::Array { element, size } if index < *size => {
            Some((element, index * usize::from(element.size())))
        }
        LMIRTypeKind::Struct { fields, .. } => {
            let mut offset = 0usize;
            for (field_index, (_, field_type)) in fields.iter().enumerate() {
                offset = offset.next_multiple_of(usize::from(field_type.alignment()).max(1));
                if field_index == index {
                    return Some((field_type, offset));
                }
                offset += usize::from(field_type.size());
            }
            None
        }
        LMIRTypeKind::Opaque { .. } if index == 0 => Some((ty, 0)),
        _ => None,
    }
}

fn collect_leaves<'i>(
    ty: &'i LMIRType,
    initializer: &'i LMIRGlobalInitializer,
    offset: usize,
    leaves: &mut Vec<Leaf<'i>>,
) -> LLVMResult<()> {
    match initializer {
        LMIRGlobalInitializer::Null => {}
        LMIRGlobalInitializer::Overlay { ty, value } => {
            collect_leaves(ty, value, offset, leaves)?;
        }
        LMIRGlobalInitializer::Aggregate { fields } => {
            for (index, field) in fields {
                let (field_type, field_offset) = aggregate_field(ty, *index).ok_or_else(|| {
                    LLVMError::new(
                        &catalogue::INDEX_BOUNDS,
                        ("aggregate initializer field".into(), format!("{index}")),
                    )
                })?;
                collect_leaves(field_type, field, offset + field_offset, leaves)?;
            }
        }
        _ => leaves.push(Leaf {
            offset,
            ty,
            initializer,
        }),
    }
    Ok(())
}

fn flat_leaves<'i>(
    ty: &'i LMIRType,
    initializer: &'i LMIRGlobalInitializer,
) -> LLVMResult<Vec<Leaf<'i>>> {
    let mut leaves = Vec::new();
    collect_leaves(ty, initializer, 0, &mut leaves)?;
    leaves.sort_by_key(|leaf| leaf.offset);
    Ok(leaves)
}

/// Lays `leaves` out as the fields of a packed struct of `size` bytes, padding the gaps.
fn flat_fields<'i, T>(
    size: usize,
    leaves: &[Leaf<'i>],
    mut padding: impl FnMut(u32) -> T,
    mut scalar: impl FnMut(&Leaf<'i>) -> LLVMResult<T>,
) -> LLVMResult<Vec<T>> {
    let mut fields = Vec::new();
    let mut end = 0;
    for leaf in leaves {
        if leaf.offset < end {
            continue;
        }
        if leaf.offset > end {
            fields.push(padding((leaf.offset - end) as u32));
        }
        fields.push(scalar(leaf)?);
        end = leaf.offset + usize::from(leaf.ty.size());
    }
    if end < size {
        fields.push(padding((size - end) as u32));
    }
    Ok(fields)
}

fn leaf_type<'ctx>(state: &GlobalState<'ctx>, leaf: &Leaf<'_>) -> LLVMResult<BasicTypeEnum<'ctx>> {
    any_to_basic_type(bc_llvm_type(state.context, leaf.ty)?)
}

/// The type of a global whose initializer overlays a value on an opaque type. A global is only
/// reached through its address, so its type is free to follow the initializer: a packed struct of
/// the scalars at their offsets, which can hold any member of a union.
fn flat_type<'ctx>(
    state: &GlobalState<'ctx>,
    ty: &LMIRType,
    initializer: &LMIRGlobalInitializer,
) -> LLVMResult<BasicTypeEnum<'ctx>> {
    let leaves = flat_leaves(ty, initializer)?;
    let fields = flat_fields(
        usize::from(ty.size()),
        &leaves,
        |bytes| state.context.i8_type().array_type(bytes).into(),
        |leaf| leaf_type(state, leaf),
    )?;
    Ok(state.context.struct_type(&fields, true).into())
}

fn flat_initializer<'ctx>(
    state: &GlobalState<'ctx>,
    ty: &LMIRType,
    initializer: &LMIRGlobalInitializer,
) -> LLVMResult<BasicValueEnum<'ctx>> {
    let leaves = flat_leaves(ty, initializer)?;
    let fields = flat_fields(
        usize::from(ty.size()),
        &leaves,
        |bytes| state.context.i8_type().array_type(bytes).const_zero().into(),
        |leaf| global_initializer(state, leaf_type(state, leaf)?, leaf.initializer),
    )?;
    Ok(state.context.const_struct(&fields, true).into())
}

/// Positions each field initializer at its index (the first initializer wins for duplicates), so
/// building a large constant aggregate stays linear in its length
fn initializers_by_index(
    fields: &[(usize, LMIRGlobalInitializer)],
    len: usize,
) -> Vec<Option<&LMIRGlobalInitializer>> {
    let mut initializers = vec![None; len];
    for (index, initializer) in fields {
        if let Some(slot @ None) = initializers.get_mut(*index) {
            *slot = Some(initializer);
        }
    }
    initializers
}
