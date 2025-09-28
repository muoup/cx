use cranelift_module::{DataDescription, Linkage, Module};
use cx_data_mir::{MIRGlobalType, MIRGlobalValue};
use crate::GlobalState;
use crate::routines::convert_linkage;

pub(crate) fn generate_global(state: &mut GlobalState, variable: &MIRGlobalValue) -> Option<()> {
    match &variable._type {
        MIRGlobalType::StringLiteral(str) => {
            let id = state.object_module.declare_anonymous_data(
                false,
                false
            ).unwrap();

            let mut str_data = str.to_owned().into_bytes();
            str_data.push(b'\0');

            let mut data = DataDescription::new();
            data.define(str_data.into_boxed_slice());

            state.object_module.define_data(id, &data).unwrap();
            state.object_module.declare_data_in_data(id, &mut data);
        },

        MIRGlobalType::Variable { _type, initial_value } => {
            let linkage = convert_linkage(variable.linkage);
            let id = state.object_module.declare_data(
                variable.name.as_str(), linkage,
                true, false
            ).unwrap();

            if linkage == Linkage::Import {
                return Some(());
            }

            let mut data = DataDescription::new();

            if let Some(initial_value) = initial_value {
                let bytes : [u8; 8] =
                    unsafe { i64::to_ne_bytes(*initial_value) };
                let type_size = _type.fixed_size();
                let relevant_data = bytes.iter()
                    .skip(8 - type_size)
                    .cloned()
                    .collect::<Vec<_>>();

                data.define(relevant_data.into_boxed_slice());
                state.object_module.define_data(id, &data).expect("");
            } else {
                let size = _type.fixed_size();
                data.define_zeroinit(size);
                state.object_module.define_data(id, &data).expect("");
            }
        },
    }

    Some(())
}