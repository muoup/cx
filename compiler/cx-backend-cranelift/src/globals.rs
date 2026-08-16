use crate::{routines::convert_linkage, GlobalState};
use cranelift_module::{DataDescription, Linkage, Module};
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
                    }
                    LMIRGlobalState::External => unreachable!(),
                }
                state.object_module.define_data(id, &data).expect("");

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
        LMIRGlobalInitializer::Null => vec![0; usize::from(ty.size())],
    };
    fit_bytes(bytes, usize::from(ty.size()))
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
