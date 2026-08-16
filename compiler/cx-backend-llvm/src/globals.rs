use crate::GlobalState;
use crate::typing::{any_to_basic_type, bc_llvm_type};
use cx_lmir::{LMIRGlobalInitializer, LMIRGlobalState, LMIRGlobalType, LMIRGlobalValue};
use inkwell::module::Linkage;
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
        LMIRGlobalInitializer::Null => match basic_type {
            inkwell::types::BasicTypeEnum::PointerType(pointer) => pointer.const_null().into(),
            _ => basic_type.const_zero(),
        },
    }
}
