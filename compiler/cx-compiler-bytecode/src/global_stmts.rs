use cx_data_ast::parse::ast::{CXExpr, CXFunctionPrototype};
use cx_data_ast::parse::value_type::CXType;
use cx_util::mangling::mangle_destructor;
use cx_data_bytecode::types::{BCType, BCTypeKind};
use cx_data_bytecode::{BCFunctionPrototype, BCParameter, LinkageType, VirtualInstruction};
use crate::aux_routines::allocate_variable;
use crate::builder::BytecodeBuilder;
use crate::instruction_gen::generate_instruction;

pub(crate) fn generate_function(
    builder: &mut BytecodeBuilder,
    prototype: &CXFunctionPrototype,
    body: &CXExpr
) -> Option<()> {
    builder.symbol_table.push_scope();

    let bc_prototype = builder.convert_cx_prototype(prototype).unwrap();
    builder.new_function(bc_prototype.clone());

    generate_params(builder, prototype)?;

    let Some(_) = generate_instruction(builder, body) else {
        panic!("Failed to generate body for function: {}", prototype.name);
    };

    builder.symbol_table.pop_scope();
    builder.finish_function(false);

    Some(())
}

pub(crate) fn generate_destructor(
    builder: &mut BytecodeBuilder,
    type_name: &str,
    body: &CXExpr
) -> Option<()> {
    let destructor_name = mangle_destructor(type_name);
    let prototype = BCFunctionPrototype {
        return_type: BCType::unit(),
        name: destructor_name,
        params: vec![BCParameter {
            name: Some("this".to_string()),
            _type: BCType::from(BCTypeKind::Pointer {
                nullable: false,
                dereferenceable: 0,
            }),
        }],
        var_args: false,
        linkage: LinkageType::Public
    };

    builder.symbol_table.push_scope();
    builder.new_function(prototype);

    let this = builder.add_instruction_bt(
        VirtualInstruction::FunctionParameter {
            param_index: 0,
        },
        BCType::default_pointer()
    )?;

    builder.symbol_table.insert("this".to_string(), this);

    let Some(_) = generate_instruction(builder, body) else {
        panic!("Failed to generate body for destructor: {type_name}");
    };

    builder.symbol_table.pop_scope();
    builder.finish_function(false);

    Some(())
}

fn generate_params(
    builder: &mut BytecodeBuilder,
    prototype: &CXFunctionPrototype
) -> Option<()> {
    let is_structured_return = prototype.return_type.is_structured();
    
    for (mut i, arg) in prototype.params.iter().enumerate() {
        let bc_type = builder.convert_cx_type(&arg._type)?;
        
        let memory = allocate_variable(
            &arg.name.as_ref().map(|n| n.to_string()).unwrap_or_else(|| format!("_fn_arg_{i}")),
            builder,
            &arg._type
        )?;

        if is_structured_return { i += 1; };

        let value = builder.add_instruction_bt(
            VirtualInstruction::FunctionParameter {
                param_index: i as u32,
            },
            bc_type.clone()
        )?;

        builder.add_instruction(
            VirtualInstruction::Store {
                value,
                memory,
                type_: bc_type
            },
            CXType::unit()
        )?;
    }
    
    Some(())
}