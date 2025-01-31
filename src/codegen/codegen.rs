use crate::codegen::expression::codegen_expression;
use crate::codegen::scope::VariableTable;
use crate::parse::ast::{Expression, FunctionDeclaration, AST};
use cranelift::codegen::ir::{Function, UserFuncName};
use cranelift::codegen::isa::CallConv;
use cranelift::codegen::{ir, settings, Context};
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift::prelude::Signature;
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

pub(crate) struct CodegenContext<'a> {
    pub(crate) context: &'a mut Context,
    pub(crate) object_module: &'a mut ObjectModule,
    pub(crate) builder: FunctionBuilder<'a>,
    pub(crate) variable_table: VariableTable,
    pub(crate) current_block_exited: bool
}

pub fn ast_codegen(ast: &AST) {
    let settings_builder = settings::builder();
    let flags = settings::Flags::new(settings_builder);

    let native_builder = cranelift_native::builder().unwrap();
    let isa = native_builder.finish(flags).unwrap();

    let object_builder = ObjectBuilder::new(
        isa.clone(),
        "test.o",
        cranelift_module::default_libcall_names(),
    ).unwrap();

    let mut object_module = ObjectModule::new(object_builder);
    let mut context = object_module.make_context();

    for fn_decl in &ast.root.fn_declarations {
        codegen_function(fn_decl, &mut context, &mut object_module);
    }

    let obj = object_module.finish();
    std::fs::write("test.o", obj.emit().unwrap()).unwrap();
}

pub fn codegen_function(fn_decl: &FunctionDeclaration, context: &mut Context, module: &mut ObjectModule) {
    let sig = Signature::new(CallConv::WindowsFastcall);
    let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);

    let mut binding = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut func, &mut binding);

    let block = builder.create_block();
    builder.switch_to_block(block);
    let mut var_table = VariableTable::new();
    var_table.push_scope();

    for arg in &fn_decl.arguments {
        let Expression::VariableDeclaration { type_, name } = arg else {
            panic!("Expected variable declaration");
        };

        builder.func.signature.params.push(ir::AbiParam::new(ir::types::I64));
        builder.func.signature.returns.push(ir::AbiParam::new(ir::types::I64));
        var_table.insert(
            name.clone(),
            builder.append_block_param(block, ir::types::I64)
        );
    }

    let gen_id = ||
    {
        let mut context = CodegenContext {
            context,
            object_module: module,
            builder,
            variable_table: var_table,
            current_block_exited: false
        };

        let id = context.object_module
            .declare_function(&fn_decl.name, Linkage::Export, &context.builder.func.signature)
            .unwrap();

        for expr in &fn_decl.body {
            codegen_expression(&mut context, expr);
        }

        context.builder.seal_all_blocks();
        context.builder.finalize();

        id
    };

    let id = gen_id();

    println!("{:?}", func);

    context.func = func;
    module
        .define_function(id, context)
        .expect("Failed to define function");

    module.clear_context(context);
}