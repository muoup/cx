use crate::{routines::convert_linkage, GlobalState};
use cranelift_module::{DataDescription, Linkage, Module};
use cx_lmir::types::{LMIRType, LMIRTypeKind};
use cx_lmir::{LMIRGlobalInitializer, LMIRGlobalState, LMIRGlobalType, LMIRGlobalValue};
use cx_log::CXResult;

pub(crate) fn generate_global(state: &mut GlobalState, variable: &LMIRGlobalValue) -> CXResult<()> {
    let id = match &variable._type {
        LMIRGlobalType::StringLiteral(str) => {
            let id = state
                .object_module
                .declare_anonymous_data(false, false)
                .unwrap();

            let mut str_data = str.to_owned().into_bytes();
            str_data.push(b'\0');

            let mut data = DataDescription::new();
            data.define(str_data.into_boxed_slice());

            state.object_module.define_data(id, &data).unwrap();
            state.object_module.declare_data_in_data(id, &mut data);
            id
        }

        LMIRGlobalType::Variable {
            _type,
            state: global_state,
        } => {
            let linkage = match global_state {
                LMIRGlobalState::External => Linkage::Import,
                LMIRGlobalState::ZeroInitialized | LMIRGlobalState::Initialized(_) => {
                    match variable.linkage {
                        cx_lmir::LinkageType::External => Linkage::Export,
                        linkage => convert_linkage(linkage),
                    }
                }
            };
            let id = state
                .object_module
                .declare_data(variable.name.as_str(), linkage, true, false)
                .unwrap();

            if linkage == Linkage::Import {
                id
            } else {
                let mut data = DataDescription::new();

                match global_state {
                    LMIRGlobalState::ZeroInitialized => {
                        data.define_zeroinit(usize::from(_type.size()));
                    }
                    LMIRGlobalState::Initialized(initializer) => {
                        data.define(initializer_bytes(initializer, _type).into_boxed_slice());
                        write_initializer_relocations(state, &mut data, initializer, _type, 0);
                    }
                    LMIRGlobalState::External => unreachable!(),
                }
                state.object_module.define_data(id, &data).expect("");
                state.object_module.declare_data_in_data(id, &mut data);

                id
            }
        }
    };

    state.global_ids.push(id);

    Ok(())
}

fn initializer_bytes(
    initializer: &LMIRGlobalInitializer,
    ty: &cx_lmir::types::LMIRType,
) -> Vec<u8> {
    let bytes = match initializer {
        LMIRGlobalInitializer::Integer { value, .. } => value.to_ne_bytes().to_vec(),
        LMIRGlobalInitializer::Float { value, _type } => match _type {
            cx_lmir::types::LMIRFloatType::F32 => {
                let value: f32 = value.into();
                value.to_ne_bytes().to_vec()
            }
            cx_lmir::types::LMIRFloatType::F64 => {
                let value: f64 = value.into();
                value.to_ne_bytes().to_vec()
            }
        },
        LMIRGlobalInitializer::Aggregate { fields } => {
            let mut bytes = vec![0; usize::from(ty.size())];
            for (index, initializer) in fields {
                let Some((field_type, offset)) = aggregate_field(ty, *index) else {
                    panic!("invalid aggregate global initializer field {index} for {ty:?}");
                };
                let field_bytes = initializer_bytes(initializer, &field_type);
                let end = (offset + field_bytes.len()).min(bytes.len());
                if offset < end {
                    bytes[offset..end].copy_from_slice(&field_bytes[..end - offset]);
                }
            }
            bytes
        }
        LMIRGlobalInitializer::Global(_)
        | LMIRGlobalInitializer::GlobalOffset { .. }
        | LMIRGlobalInitializer::Function(_) => {
            vec![0; usize::from(ty.size())]
        }
        LMIRGlobalInitializer::Null => vec![0; usize::from(ty.size())],
    };
    fit_bytes(bytes, usize::from(ty.size()))
}

fn write_initializer_relocations(
    state: &mut GlobalState,
    data: &mut DataDescription,
    initializer: &LMIRGlobalInitializer,
    ty: &LMIRType,
    offset: usize,
) {
    match initializer {
        LMIRGlobalInitializer::Global(global) => {
            let target = *state
                .global_ids
                .get(*global as usize)
                .unwrap_or_else(|| panic!("invalid global initializer reference {global}"));
            let target = state.object_module.declare_data_in_data(target, data);
            data.write_data_addr(offset as u32, target, 0);
        }
        LMIRGlobalInitializer::GlobalOffset {
            global,
            offset: addend,
        } => {
            let target = *state
                .global_ids
                .get(*global as usize)
                .unwrap_or_else(|| panic!("invalid global initializer reference {global}"));
            let target = state.object_module.declare_data_in_data(target, data);
            data.write_data_addr(offset as u32, target, *addend);
        }
        LMIRGlobalInitializer::Function(function) => {
            let target = *state
                .function_ids
                .get(function)
                .unwrap_or_else(|| panic!("invalid function initializer reference {function}"));
            let target = state.object_module.declare_func_in_data(target, data);
            data.write_function_addr(offset as u32, target);
        }
        LMIRGlobalInitializer::Aggregate { fields } => {
            for (index, field) in fields {
                let Some((field_type, field_offset)) = aggregate_field(ty, *index) else {
                    panic!("invalid aggregate global initializer field {index} for {ty:?}");
                };
                write_initializer_relocations(
                    state,
                    data,
                    field,
                    &field_type,
                    offset + field_offset,
                );
            }
        }
        _ => {}
    }
}

fn aggregate_field(ty: &LMIRType, index: usize) -> Option<(LMIRType, usize)> {
    match &ty.kind {
        LMIRTypeKind::Array { element, size } if index < *size => Some((
            element.as_ref().clone(),
            index * usize::from(element.size()),
        )),
        LMIRTypeKind::Struct { fields, .. } if index < fields.len() => {
            let mut offset = 0usize;
            for (field_index, (_, field_type)) in fields.iter().enumerate() {
                let alignment = usize::from(field_type.alignment());
                if !offset.is_multiple_of(alignment) {
                    offset += alignment - offset % alignment;
                }
                if field_index == index {
                    return Some((field_type.clone(), offset));
                }
                offset += usize::from(field_type.size());
            }
            None
        }
        LMIRTypeKind::Opaque { .. } if index == 0 => Some((ty.clone(), 0)),
        _ => None,
    }
}

fn fit_bytes(mut bytes: Vec<u8>, size: usize) -> Vec<u8> {
    if bytes.len() < size {
        bytes.resize(size, 0);
        return bytes;
    }
    if bytes.len() > size {
        if cfg!(target_endian = "little") {
            bytes.truncate(size);
        } else {
            bytes = bytes.split_off(bytes.len() - size);
        }
    }
    bytes
}
