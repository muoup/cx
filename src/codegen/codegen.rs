use crate::codegen::expression::codegen_expression;
use crate::codegen::scope::VariableTable;
use crate::codegen::value_type::{get_cranelift_abi_type, get_cranelift_type};
use crate::parse::ast::{GlobalStatement, AST};
use cranelift::codegen::ir::{Function, UserFuncName};
use cranelift::codegen::isa::CallConv;
use cranelift::codegen::{settings, Context};
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift::prelude::Signature;
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::HashMap;

pub(crate) struct FunctionState<'a> {
    pub(crate) object_module: &'a mut ObjectModule,
    pub(crate) functions: &'a HashMap<String, FuncId>,

    pub(crate) builder: FunctionBuilder<'a>,
    pub(crate) variable_table: VariableTable,
    pub(crate) current_block_exited: bool
}

pub(crate) struct GlobalState {
    pub(crate) context: Context,
    pub(crate) object_module: ObjectModule,

    pub(crate) functions: HashMap<String, FuncId>
}

pub fn ast_codegen(ast: &AST) {
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

    for fn_decl in &ast.root.fn_declarations {
        codegen_function(fn_decl, &mut global_state);
    }

    let obj = global_state.object_module.finish();
    std::fs::write("test.o", obj.emit().unwrap()).expect("Failed to write object file");
}

pub fn codegen_function(global_stmt: &GlobalStatement, global_state: &mut GlobalState) {
    let GlobalStatement::Function { name, arguments, return_type, body } = global_stmt;

    let sig = Signature::new(CallConv::WindowsFastcall);
    let mut func = Function::with_name_signature(
        UserFuncName::user(0, global_state.functions.len() as u32),
        sig
    );

    for (_, type_) in arguments {
        func.signature.params.push(get_cranelift_abi_type(global_state, type_));
    }

    func.signature.returns.push(get_cranelift_abi_type(global_state, return_type));

    let id = global_state.object_module
        .declare_function(name, Linkage::Export, &func.signature)
        .unwrap();

    global_state.functions.insert(name.clone(), id);

    let Some(body) = body else {
        println!("{:?}", func);
        return;
    };

    let mut binding = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut func, &mut binding);

    let block = builder.create_block();
    builder.switch_to_block(block);
    let mut var_table = VariableTable::new();
    var_table.push_scope();

    for (name, type_) in arguments {
        var_table.insert(
            name.clone(),
            builder.append_block_param(block, get_cranelift_type(global_state, type_))
        );
    }

    {
        let mut context = FunctionState {
            object_module: &mut global_state.object_module,
            functions: &global_state.functions,

            builder,
            variable_table: var_table,
            current_block_exited: false
        };

        for expr in body {
            codegen_expression(&mut context, expr);
        }

        context.builder.seal_all_blocks();
        context.builder.finalize();

        id
    };

    println!("{:?}", func);

    let GlobalState { object_module, context, .. } = global_state;

    context.func = func;
    object_module
        .define_function(id, context)
        .expect("Failed to define function");

    object_module.clear_context(context);
}