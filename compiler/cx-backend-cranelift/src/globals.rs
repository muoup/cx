use cranelift_module::{DataDescription, Module};
use cx_data_bytecode::{BCGlobalType, BCGlobalValue};
use crate::GlobalState;

pub(crate) fn generate_global(state: &mut GlobalState, variable: &BCGlobalValue) -> Option<()> {
    match &variable._type {
        BCGlobalType::StringLiteral(str) => {
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

        BCGlobalType::Variable { _type, initial_value } => {
            let id = state.object_module.declare_data(
                variable.name.as_str(),
                cranelift_module::Linkage::Export,
                true,
                false
            ).unwrap();

            let mut data = DataDescription::new();

            if let Some(initial_value) = initial_value {
                let bytes : [u8; 8] =
                    unsafe { std::mem::transmute(*initial_value) };
                let type_size = _type.fixed_size();
                let relevant_data = bytes.iter()
                    .skip(8 - type_size)
                    .cloned()
                    .collect::<Vec<_>>();

                data.define(relevant_data.into_boxed_slice());
            } else {
                let size = _type.fixed_size();
                data.define_zeroinit(size);
            }

            state.object_module.define_data(id, &data).unwrap();
            state.object_module.declare_data_in_data(id, &mut data);
        },
    }

    Some(())
}