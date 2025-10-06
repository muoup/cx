use crate::builder::MIRBuilder;
use crate::cx_maps::convert_cx_prototype;
use cx_mir_data::types::{MIRType, MIRTypeKind};
use cx_mir_data::{MIRFunctionPrototype, MIRValue, VirtualInstruction};
use cx_typechecker_data::cx_types::{CXFunctionPrototype, CXParameter, CXType, CXTypeKind};
use cx_typechecker_data::function_map::CXFunctionKind;

const STANDARD_FREE: &str = "__stdfree";
const STANDARD_FREE_ARRAY: &str = "__stdfreearray";
const STANDARD_FREE_ARRAY_NOOP: &str = "__stdfreearray_destructor_noop";

pub(crate) fn deconstructor_prototype(type_: &CXType) -> Option<MIRFunctionPrototype> {
    let name = type_.get_identifier()?;

    let mut prototype = CXFunctionPrototype {
        name: CXFunctionKind::Deconstructor {
            base_type: name.clone(),
        }
        .into(),
        return_type: CXType::unit(),
        params: vec![CXParameter {
            name: None,
            _type: type_.clone().pointer_to(),
        }],
        var_args: false,
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
            if let Some(deconstructor) = struct_deconstruction(builder, _type) {
                builder.call_proto(deconstructor, vec![var.clone()])?;
            }
        }

        CXTypeKind::StrongPointer {
            inner_type,
            is_array,
        } => {
            let deconstructor = struct_deconstruction(builder, inner_type);
            let inner_val = builder.load_value(var.clone(), MIRType::default_pointer())?;

            match (deconstructor, is_array) {
                (Some(prototype), true) => {
                    // Array of objects with deconstructor
                    let ptr_to = builder.add_instruction(
                        VirtualInstruction::GetFunctionAddr {
                            func: prototype.name,
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
                    builder.call_proto(prototype, vec![inner_val.clone()])?;
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

pub(crate) fn generate_deconstructor<'a>(
    builder: &mut MIRBuilder,
    _type: &CXType,
) -> Option<()> {
    let CXTypeKind::Structured { fields, .. } = &_type.kind else {
        return None;
    };
    
    let prototype = deconstructor_prototype(_type)?;
    
    builder.fn_map.insert(prototype.name.clone(), prototype.clone());
    builder.new_function(prototype);
    
    for (index, (_, field_type)) in fields.iter().enumerate() {
        let mir_type = builder.convert_cx_type(_type)?;
        
        let access = builder.struct_access(MIRValue::ParameterRef(0), &mir_type, index)
            .unwrap();
        
        deconstruct_variable(builder, &access, field_type);
    }
    
    if let Some(destructor) = builder.get_destructor(_type) {
        builder.call(&destructor, vec![MIRValue::ParameterRef(0)]);
    }
    
    builder.finish_function();
    Some(())
}

// Lazily generate struct deconstructor -- i.e. visit all fields, including
// the inner types of strong pointers, and ensure that any types needing implicit
// deconstruction will be in the builder's set.
fn struct_deconstruction(builder: &mut MIRBuilder, _type: &CXType) -> Option<MIRFunctionPrototype> {
    if let Some(deconstructor) = builder.get_deconstructor(_type) {
        return Some(deconstructor);
    }

    let CXTypeKind::Structured { fields, .. } = &_type.kind else {
        return None;
    };
    
    let destructor = builder.get_destructor(_type);
    let mut deconstructor_needed = destructor.is_some();

    for (_, field) in fields.iter() {
        match &field.kind {
            CXTypeKind::StrongPointer { inner_type, .. } => {
                struct_deconstruction(builder, inner_type);
                deconstructor_needed = true;
            },
            
            CXTypeKind::Structured { .. } => {
                deconstructor_needed |= struct_deconstruction(builder, field).is_some();
            },
            
            _ => {}
        }
    }
    
    if !deconstructor_needed {
        return None;
    }
    
    builder.defined_deconstructors.insert(_type.clone());
    return deconstructor_prototype(_type);
}
