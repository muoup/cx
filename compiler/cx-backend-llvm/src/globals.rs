use crate::GlobalState;
use crate::typing::{any_to_basic_type, bc_llvm_type};
use cx_lmir::{LMIRGlobalInitializer, LMIRGlobalState, LMIRGlobalType, LMIRGlobalValue};
use inkwell::module::Linkage;
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
            let llvm_type = bc_llvm_type(state.context, _type)?;
            let basic_type = any_to_basic_type(llvm_type)
                .unwrap_or_else(|| panic!("Unsupported global variable type"));

            let global = state
                .module
                .add_global(basic_type, None, variable.name.as_str());

            if matches!(global_state, LMIRGlobalState::External) {
                global.set_linkage(Linkage::External);
            } else {
                let initializer = match global_state {
                    LMIRGlobalState::ZeroInitialized => basic_type.const_zero(),
                    LMIRGlobalState::Initialized(initializer) => {
                        global_initializer(basic_type, initializer)
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

fn global_initializer<'ctx>(
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
        LMIRGlobalInitializer::Null => match basic_type {
            inkwell::types::BasicTypeEnum::PointerType(pointer) => pointer.const_null().into(),
            _ => basic_type.const_zero(),
        },
    }
}
