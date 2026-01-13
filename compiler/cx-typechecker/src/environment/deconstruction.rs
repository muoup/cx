use cx_parsing_data::data::CXFunctionContract;
use cx_typechecker_data::mir::{
    expression::{MIRInstruction, MIRValue},
    name_mangling::base_mangle_deconstructor,
    program::MIRBaseMappings,
    types::{MIRFunctionPrototype, MIRParameter, MIRType, MIRTypeKind},
};
use cx_util::{CXResult, identifier::CXIdent};

use crate::{environment::{MIRFunctionGenRequest, TypeEnvironment}, type_checking::binary_ops::struct_field};

pub(crate) fn generate_deconstructor(env: &mut TypeEnvironment, base_data: &MIRBaseMappings, _type: MIRType) -> CXResult<()> {
    let Some(func) = env.get_deconstructor(&_type) else {
        unreachable!("Deconstructor prototype should be inserted before generation request")
    };

    env.builder.start_function(func);
    let this =
        MIRValue::Parameter {
            name: CXIdent::from("this"),
            _type: _type.clone().pointer_to(),
        };

    env.insert_stack_symbol("this".to_owned(), this.clone());

    match &_type.kind {
        MIRTypeKind::Structured { fields, .. } => {
            for (field_name, field_type) in fields {
                let struct_field = struct_field(&_type, field_name).unwrap();
                
                if let Some(func) = env.get_deconstructor(field_type) {
                    let pointer = env.builder.new_register();
                    env.builder.add_instruction(
                        MIRInstruction::StructGet {
                            result: pointer.clone(),
                            source: this.clone(),
                            field_index: struct_field.index,
                            field_offset: struct_field.offset,
                            struct_type: struct_field.field_type
                        }
                    );
                    
                    env.builder.add_instruction(
                        MIRInstruction::CallFunction {
                            result: None,
                            function: MIRValue::FunctionReference { prototype: func, implicit_variables: vec![] },
                            arguments: vec![MIRValue::Register {
                                register: pointer,
                                _type: field_type.clone().pointer_to(),
                            }],
                        }
                    );
                }
            }
        },

        _ => todo!("Deconstructor logic for: {}", _type),
    }
    
    if let Some(destructor) = env.get_destructor(base_data, &_type) {
        env.builder.add_instruction(
            MIRInstruction::CallFunction {
                result: None,
                function: MIRValue::FunctionReference { prototype: destructor, implicit_variables: vec![] },
                arguments: vec![this],
            }
        );
    }
    
    env.builder.add_return(None);
    env.builder.finish_function();
    Ok(())
}

fn push_deconstructor_request(env: &mut TypeEnvironment, _type: MIRType) {
    let deconstructor_name = base_mangle_deconstructor(&_type);
    let deconstructor = MIRFunctionPrototype {
        name: CXIdent::from(deconstructor_name.clone()),
        params: vec![MIRParameter {
            name: Some(CXIdent::from("this")),
            _type: _type.clone().pointer_to(),
        }],
        return_type: MIRType::unit(),
        var_args: false,
        contract: CXFunctionContract::default(),
    };

    env.realized_fns.insert(deconstructor_name, deconstructor);
    env.requests
        .push(MIRFunctionGenRequest::Deconstruction { _type });
}

pub fn process_new_type(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    _type: MIRType,
) {
    if env.get_destructor(base_data, &_type).is_some() {
        push_deconstructor_request(env, _type);
        return;
    }
 
    println!("FN MAP: {}", env.realized_fns.keys().cloned().collect::<Vec<_>>().join(", "));

    match &_type.kind {
        MIRTypeKind::Structured { fields, .. } => {
            // Even if we do not have a destructor, if any of our fields have destructors,
            // we need to generate a deconstructor for this type as well.
            for (_field_name, field_type) in fields.iter() {
                if env.get_deconstructor(field_type).is_some() {
                    push_deconstructor_request(env, _type.clone());
                    break;
                }
            }
        }

        // TODO: Tagged Union deconstruction
        _ => {}
    }
}
