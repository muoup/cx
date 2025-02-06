use crate::parse::ast::{GlobalStatement, Root};
use crate::parse::verify::context::{FunctionPrototype, VerifyContext};

pub(crate) fn global_pass(context: &mut VerifyContext, root: &Root) {
    for fn_decl in &root.global_stmts {
        match fn_decl {
            GlobalStatement::Function { name, return_type, arguments, .. } =>
                context.insert_function(name, FunctionPrototype {
                    return_type: return_type.clone(),
                    args: arguments.iter()
                        .map(|arg| arg.1.clone())
                        .collect()
                }),
        };
    }
}