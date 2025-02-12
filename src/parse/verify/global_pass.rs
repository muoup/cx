use crate::parse::ast::{GlobalStatement, UnverifiedAST, UnverifiedGlobalStatement, ValueType};
use crate::parse::verify::context::{FunctionPrototype, VerifyContext};

pub(crate) fn global_pass(context: &mut VerifyContext, root: &UnverifiedAST) {
    for global_stmt in &root.statements {
        // match global_stmt {
        //     UnverifiedGlobalStatement { name, return_type, arguments, .. } =>
        //         context.insert_function(name, FunctionPrototype {
        //             return_type: ValueTypeRef::new(ValueType::Unit),
        //             args: vec![]
        //         }),
        //
        //     _ => {}
        // };
    }
}