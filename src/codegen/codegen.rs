use crate::codegen::expression::codegen_expression;
use crate::codegen::routines::allocate_variable;
use crate::codegen::scope::VariableTable;
use crate::codegen::value_type::{get_cranelift_abi_type, get_cranelift_type};
use crate::parse::ast::{ValueType, GlobalStatement, Expression, FunctionParameter};
use cranelift::codegen::ir::{Function, UserFuncName};
use cranelift::codegen::{settings, Context};
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift::prelude::Signature;
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::HashMap;
use crate::parse::verify::context::FunctionPrototype;
use crate::parse::verify::VerifiedAST;

pub(crate) struct FunctionState<'a> {
    pub(crate) object_module: &'a mut ObjectModule,
    pub(crate) functions: &'a HashMap<String, FuncId>,

    pub(crate) builder: FunctionBuilder<'a>,
    pub(crate) variable_table: VariableTable,

    pub(crate) merge_block_id: Option<u32>,
    pub(crate) loop_block_id: Option<u32>,
    pub(crate) current_block_exited: bool,
}

pub(crate) struct GlobalState {
    pub(crate) context: Context,
    pub(crate) object_module: ObjectModule,

    pub(crate) functions: HashMap<String, FuncId>
}

pub fn ast_codegen(ast: &VerifiedAST) {
    let settings_builder = settings::builder();
    let flags = settings::Flags::new(settings_builder);

    let native_builder = cranelift_native::builder().unwrap();
    let isa = native_builder.finish(flags).unwrap();

    let mut global_state = GlobalState {
        context: Context::new(),
        object_module: ObjectModule::new(
            ObjectBuilder::new(
                isa.clone(),
                "test.o",
                cranelift_module::default_libcall_names(),
            ).unwrap()
        ),
        functions: HashMap::new()
    };

    for global_stmt in &ast.global_statements {
        codegen_global_statement(global_stmt, &mut global_state);
    }

    let obj = global_state.object_module.finish();
    std::fs::write("test.o", obj.emit().unwrap()).expect("Failed to write object file");
}

pub fn codegen_global_statement(global_stmt: &GlobalStatement, global_state: &mut GlobalState) {
    match global_stmt {
        GlobalStatement::Function { prototype, body } => {
            codegen_function(global_state, prototype, body);
        },
        GlobalStatement::TypeDeclaration { name, type_ } => {

        },
        _ => {
            println!("Unsupported global statement: {:?}", global_stmt);
        }
    }
}

fn codegen_function(global_state: &mut GlobalState, prototype: &FunctionPrototype, body: &Option<Vec<Expression>>) {
    let sig = Signature::new(
        global_state.object_module.target_config().default_call_conv
    );
    let mut func = Function::with_name_signature(
        UserFuncName::user(0, global_state.functions.len() as u32),
        sig
    );

    for FunctionParameter { type_, .. } in prototype.args.iter() {
        func.signature.params.push(get_cranelift_abi_type(type_));
    }

    match &prototype.return_type {
        ValueType::Unit => {},
        type_ => {
            func.signature.returns.push(get_cranelift_abi_type(type_));
        }
    }

    let id = global_state.object_module
        .declare_function(prototype.name.as_str(), Linkage::Export, &func.signature)
        .unwrap();

    global_state.functions.insert(prototype.name.clone(), id);

    let Some(body) = body else { return; };

    let mut binding = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut func, &mut binding);

    let block = builder.create_block();
    builder.switch_to_block(block);
    let mut var_table = VariableTable::new();
    var_table.push_scope();

    for FunctionParameter { name, type_ } in prototype.args.iter() {
        let param_type = get_cranelift_type(type_);
        let param = builder.append_block_param(block, param_type);

        allocate_variable(
            &mut builder, &mut var_table,
            name.as_str(), param_type,
            Some(param)
        ).expect("Failed to allocate variable");
    }

    let mut context = FunctionState {
        object_module: &mut global_state.object_module,
        functions: &global_state.functions,

        builder,
        variable_table: var_table,

        merge_block_id: None,
        loop_block_id: None,
        current_block_exited: false
    };

    for expr in body {
        codegen_expression(&mut context, expr);
    }

    context.builder.seal_all_blocks();
    context.builder.finalize();

    println!("{:?}", func);

    let GlobalState { object_module, context, .. } = global_state;

    context.func = func;
    object_module
        .define_function(id, context)
        .expect("Failed to define function");

    object_module.clear_context(context);
}