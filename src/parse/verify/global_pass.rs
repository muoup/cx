use std::rc::Rc;
use log::warn;
use crate::parse::ast::{Expression, FunctionParameter, GlobalStatement, LValueExpression, UnverifiedAST, UnverifiedGlobalStatement, ValueType};
use crate::parse::verify::context::{FunctionPrototype, VerifyContext};
use crate::parse::verify::local_pass::{verify_lvalue, VerifyResult};
use crate::parse::verify::typing::{format_lvalue, verify_compound_pair};

pub(crate) fn global_pass(context: &mut VerifyContext, stmts: Vec<UnverifiedGlobalStatement>) -> VerifyResult<Vec<GlobalStatement>> {
    let mut global_statements = Vec::new();

    for global_stmt in stmts.into_iter() {
        global_statements.push(examine_global_statement(context, global_stmt)?);
    }

    Some(global_statements)
}

pub(crate) fn examine_global_statement(context: &mut VerifyContext, stmt: UnverifiedGlobalStatement) -> VerifyResult<GlobalStatement> {
    match stmt {
        UnverifiedGlobalStatement::Function {
            mut return_type, mut name_header,
            mut params, mut body
        } => {
            let Some((return_type, name)) =
                verify_compound_pair(context, &mut return_type, &mut name_header) else {
                println!("Failed to verify function header, found {:#?}", name_header);
                return None
            };

            let mut formatted_parameters = Vec::new();

            for mut param in params.into_iter() {
                format_lvalue(context, &mut param);

                let Expression::LValue(LValueExpression::Alloca { type_, name }) = param else {
                    println!("Function parameter must be an l-value alloca, found {:#?}", param);
                    return None
                };

                formatted_parameters.push(FunctionParameter {
                    name,
                    type_,
                });
            }

            let prototype = FunctionPrototype {
                name: name.clone(),
                return_type: return_type.clone(),
                args: formatted_parameters
            };
            context.insert_function(name.as_str(), prototype)?;

            Some(
                GlobalStatement::Function {
                    prototype: context.get_function(name.as_str()).unwrap(),
                    body
                }
            )
        },

        UnverifiedGlobalStatement::StructDeclaration {
            name, field_declarations
        } => {
            let mut fields = Vec::new();

            for mut field in field_declarations.into_iter() {
                verify_lvalue(context, &mut field)?;

                let Expression::LValue(LValueExpression::Alloca { type_, name }) = field else {
                    println!("Struct field must be an l-value alloca, found {:#?}", field);
                    return None
                };

                fields.push((name, type_));
            }

            let struct_type = ValueType::Structured {
                fields: fields.into_boxed_slice().into()
            };

            context.insert_type(name.as_str(), struct_type.clone());

            Some(
                GlobalStatement::TypeDeclaration {
                    name: Some(name),
                    type_: struct_type
                }
            )
        }

        _ => unimplemented!("Unimplemented global statement: {:#?}", stmt),
    }
}