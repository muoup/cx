use std::sync::atomic::AtomicUsize;
use inkwell::module::Linkage;
use cx_data_bytecode::{BCGlobalType, BCGlobalValue};
use crate::GlobalState;
use crate::typing::{any_to_basic_type, bc_llvm_type};

fn string_literal_name() -> String {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    let id = COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    format!(".str_{id}")
}

pub(crate) fn generate_global_variable(state: &mut GlobalState, variable: &BCGlobalValue) -> Option<()> {
    match &variable._type {
        BCGlobalType::StringLiteral(str) => {
            let val = state
                .context
                .const_string(str.as_bytes(), true);

            let global = state.module.add_global(
                val.get_type(),
                None,
                string_literal_name().as_str()
            );

            global.set_linkage(Linkage::Private);
            global.set_initializer(&val);
            global.set_unnamed_addr(true);
            global.set_constant(true);

            state.globals.push(global);
        },

        BCGlobalType::Variable { _type, initial_value } => {
            let llvm_type = bc_llvm_type(state, _type)?;
            let basic_type = any_to_basic_type(llvm_type)
                .unwrap_or_else(|| panic!("Unsupported global variable type"));

            let global = state.module.add_global(basic_type, None, variable.name.as_str());

            if let Some(initial_value) = initial_value {
                let init_val = llvm_type.into_int_type()
                    .const_int(*initial_value as u64, true);
                global.set_initializer(&init_val);
            } else {
                global.set_initializer(&llvm_type.into_int_type().const_zero());
            }

            state.globals.push(global);
        },
    }

    Some(())
}