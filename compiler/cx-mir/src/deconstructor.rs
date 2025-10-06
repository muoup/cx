use crate::aux_routines::get_cx_struct_field_by_index;
use crate::builder::MIRBuilder;
use crate::cx_maps::convert_cx_prototype;
use cx_mir_data::types::{MIRType, MIRTypeKind};
use cx_mir_data::{
    MIRFunctionPrototype, MIRValue, VirtualInstruction,
};
use cx_typechecker_data::cx_types::{CXFunctionPrototype, CXParameter, CXType, CXTypeKind};
use cx_typechecker_data::function_map::CXFunctionKind;

const STANDARD_FREE: &str = "__stdfree";
const STANDARD_FREE_ARRAY: &str = "__stdfreearray";
const STANDARD_FREE_ARRAY_NOOP: &str = "__stdfreearray_destructor_noop";

pub(crate) fn deconstructor_prototype(type_: &CXType) -> Option<MIRFunctionPrototype> {
    let name = type_.get_identifier()?;
    
    let mut prototype = CXFunctionPrototype {
        name: CXFunctionKind::Deconstructor { base_type: name.clone() }.into(),
        return_type: CXType::unit(),
        params: vec![CXParameter {
            name: None,
            _type: type_.clone().pointer_to()
        }],
        var_args: false
    };
    
    if type_.was_template_instantiated() {
        prototype.apply_template_mangling();
    }
    
    convert_cx_prototype(&prototype)
}

pub fn deconstruct_variable(
    builder: &mut MIRBuilder,
    var: &MIRValue,
    _type: &CXType,
) -> Option<()> {
    match &_type.kind {
        CXTypeKind::Structured { .. } => {
            // FIXME: Generate deconstructor here if type needs one
            
            if let Some(deconstructor) = builder.get_deconstructor(_type) {
                builder.call(&deconstructor, vec![var.clone()])?;
            }
        }

        CXTypeKind::StrongPointer {
            inner_type,
            is_array,
        } => {
            let deconstructor = builder.get_deconstructor(inner_type);
            let inner_val = builder.load_value(var.clone(), MIRType::default_pointer())?;

            match (deconstructor, is_array) {
                (Some(prototype), true) => {
                    // Array of objects with deconstructor
                    let deconstructor = builder.fn_ref(&prototype)?;
                    let ptr_to = builder.add_instruction(
                        VirtualInstruction::GetFunctionAddr {
                            func: deconstructor,
                        },
                        MIRType::default_pointer(),
                    )?;
                    let type_size = builder.convert_cx_type(inner_type.as_ref())?.fixed_size();
                    let size_imm = MIRValue::IntImmediate {
                        val: type_size as i64,
                        type_: MIRTypeKind::Unsigned { bytes: 8 }.into(),
                    };

                    builder.call(STANDARD_FREE_ARRAY, vec![inner_val, size_imm, ptr_to])?;
                }

                (None, true) => {
                    // Array of objects without deconstructor
                    builder.call(STANDARD_FREE_ARRAY_NOOP, vec![inner_val])?;
                }

                (Some(prototype), false) => {
                    // Single object with deconstructor
                    builder.call(&prototype, vec![inner_val.clone()])?;
                    builder.call(STANDARD_FREE, vec![inner_val])?;
                }

                (None, false) => {
                    // Single object without deconstructor
                    builder.call(STANDARD_FREE, vec![inner_val])?;
                }
            }
        }

        _ => (),
    }

    Some(())
}

pub fn generate_deconstructor(builder: &mut MIRBuilder, _type: &CXType) -> Option<()> {
    let deconstructor_prototype = deconstructor_prototype(_type)?;

    builder.new_function(deconstructor_prototype.clone());

    let self_val = MIRValue::ParameterRef(0);
    let as_bc = builder.convert_cx_type(_type)?;

    match &_type.kind {
        CXTypeKind::Structured { fields, .. } => {
            for (i, (_, field_type)) in fields.iter().enumerate() {
                let struct_access = get_cx_struct_field_by_index(builder, &as_bc, i)?;
                let ptr = builder.add_instruction(
                    VirtualInstruction::StructAccess {
                        struct_: self_val.clone(),
                        struct_type: as_bc.clone(),

                        field_index: i,
                        field_offset: struct_access.offset,
                    },
                    MIRType::default_pointer(),
                )?;

                deconstruct_variable(builder, &ptr, field_type)?;
            }

            if let Some(destructor) = builder.get_destructor(_type) {
                builder.call(&destructor, vec![self_val])?;
            }
        }

        _ => panic!("PANIC: Deconstructor generation for type kind {_type} not implemented"),
    }

    builder.finish_function();
    Some(())
}
