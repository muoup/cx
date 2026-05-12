use crate::routines::convert_linkage;
use crate::GlobalState;
use cranelift_module::{DataDescription, Linkage, Module};
use cx_lmir::{LMIRGlobalType, LMIRGlobalValue};
use cx_util::CXResult;

pub(crate) fn generate_global(state: &mut GlobalState, variable: &LMIRGlobalValue) -> CXResult<()> {
    match &variable._type {
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
        }

        LMIRGlobalType::Variable {
            _type,
            initial_value,
        } => {
            let linkage = convert_linkage(variable.linkage);
            let id = state
                .object_module
                .declare_data(variable.name.as_str(), linkage, true, false)
                .unwrap();

            if linkage == Linkage::Import {
                return Ok(());
            }

            let mut data = DataDescription::new();

            if let Some(initial_value) = initial_value {
                let bytes: [u8; 8] = i64::to_ne_bytes(*initial_value);
                let type_size = _type.size();

                // Little Endian:
                // 1111 2222 3333 4444
                // ~~~~~~~~~ = i16
                // 
                // Big Endian:
                // 4444 3333 2222 1111
                //     i16 = ~~~~~~~~~
                let relevant_data = if cfg!(target_endian = "little") {
                    bytes.iter().take(type_size).cloned().collect::<Vec<_>>()
                } else {
                    bytes
                        .iter()
                        .skip(8 - type_size)
                        .cloned()
                        .collect::<Vec<_>>()
                };

                data.define(relevant_data.into_boxed_slice());
                state.object_module.define_data(id, &data).expect("");
            } else {
                let size = _type.size();
                data.define_zeroinit(size);
                state.object_module.define_data(id, &data).expect("");
            }
        }
    }

    Ok(())
}
