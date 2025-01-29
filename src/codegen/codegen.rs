use crate::codegen::codegen::ir::GlobalValue;
use crate::parse::ast::{Expression, FunctionDeclaration, AST};
use cranelift::codegen::ir::{Function, UserFuncName};
use cranelift::codegen::isa::CallConv;
use cranelift::codegen::{ir, settings, Context};
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift::prelude::{AbiParam, InstBuilder, Signature, Value};
use cranelift_module::{DataDescription, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

pub(crate) struct CodegenContext {
    pub(crate) str_literal_count: usize,
    pub(crate) context: Context,
    pub(crate) object_module: ObjectModule,
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

    let object_module = ObjectModule::new(object_builder);
    let context = object_module.make_context();

    let mut context = CodegenContext {
        str_literal_count: 0,
        context,
        object_module,
    };

    for fn_decl in &ast.root.fn_declarations {
        let func = codegen_function(fn_decl, &mut context);

        println!("{:?}", func);

        let id = context.object_module
            .declare_function(&fn_decl.name, Linkage::Export, &func.signature)
            .unwrap();

        context.context.func = func;
        context.object_module
            .define_function(id, &mut context.context)
            .unwrap();

        context.object_module.clear_context(&mut context.context);
    }

    let obj = context.object_module.finish();
    std::fs::write("test.o", obj.emit().unwrap()).unwrap();
}

pub fn codegen_function(fn_decl: &FunctionDeclaration, context: &mut CodegenContext) -> Function {
    let sig = Signature::new(CallConv::SystemV);
    let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);

    let mut binding = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut func, &mut binding);

    let block = builder.create_block();

    builder.switch_to_block(block);

    for expr in &fn_decl.body {
        codegen_expression(context, &mut builder, expr);
    }

    builder.seal_all_blocks();
    builder.finalize();

    func
}

fn codegen_expression(context: &mut CodegenContext, builder: &mut FunctionBuilder, expr: &Expression) -> Option<Value> {
    match expr {
        Expression::FunctionCall(name, args) => {
            let Expression::Identifier(fn_name) = name.as_ref() else { return None; };

            let mut sig = context.object_module.make_signature();
            sig.params.push(AbiParam::new(ir::types::I64));

            let call = context.object_module
                .declare_function(&fn_name, Linkage::Import, &sig)
                .unwrap();
            let local_call = context.object_module.declare_func_in_func(call, builder.func);

            let arguments = args.iter()
                .filter_map(|arg| codegen_expression(context, builder, arg))
                .collect::<Vec<_>>();

            Value::with_number(
                builder.ins().call(
                    local_call,
                    arguments.as_slice()
                ).as_u32()
            )
        }
        Expression::Return(expr) => {
            match codegen_expression(context, builder, expr) {
                Some(val) => {
                    builder.ins().return_(&[val]);
                },
                None => {
                    builder.ins().return_(&[]);
                }
            }
            None
        },
        Expression::IntLiteral(val) => {
            Some(builder.ins().iconst(ir::types::I64, *val))
        },
        Expression::StringLiteral(str) => {
            let literal = string_literal(context, builder, str.as_ref());

            Some(builder.ins().global_value(ir::types::I64, literal))
        },
        Expression::Unit => None,
        _ => unimplemented!("Expression not implemented: {:?}", expr)
    }
}

fn string_literal(context: &mut CodegenContext, builder: &mut FunctionBuilder, str: &str) -> GlobalValue {
    let strlit_name = format!("strlit_{}", context.str_literal_count);
    context.str_literal_count += 1;

    let id = context.object_module.declare_anonymous_data(
        false,
        false
    ).unwrap();

    let mut data = DataDescription::new();
    data.define(str.as_bytes().to_vec().into_boxed_slice());

    context.object_module.define_data(id, &data).unwrap();
    context.object_module.declare_data_in_func(
        id,
        builder.func
    )
}