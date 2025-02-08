use crate::parse::ast::{GlobalStatement, Root, ValueType};
use crate::parse::verify::context::{FunctionPrototype, VerifyContext};
use crate::parse::verify::ValueTypeRef;

pub(crate) fn global_pass(context: &mut VerifyContext, root: &Root) {
    for global_stmt in &root.global_stmts {
        match global_stmt {
            GlobalStatement::Function { name, return_type, arguments, .. } =>
                context.insert_function(name, FunctionPrototype {
                    return_type: ValueTypeRef::new(ValueType::Unit),
                    args: vec![]
                }),

            _ => {}
        };
    }
}