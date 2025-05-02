// Handles Converting Abstract Expression Shapes to Concrete Meaning

use std::any::Any;
use std::clone;
use crate::lex::token::OperatorType;
use crate::log_error;
use crate::mangling::namespace_mangle;
use crate::parse::pass_unverified::{UVExpr, UVGlobalStmt, UVIdent, UVOp, UVAST};
use crate::parse::pass_molded::{CXExpr, CXFunctionPrototype, CXGlobalStmt, CXParameter, CXAST};
use crate::parse::pass_molded::expr_molding::{mold_expr_stack, mold_expression, mold_type, split_initialization};
use crate::parse::pass_molded::pattern_molding::{mold_delimited, PseudoUVExpr};
use crate::parse::value_type::CXValType;

pub(crate) fn mold_globals(ast: &UVAST, cx_ast: &mut CXAST) -> Option<()> {
    for stmt in &ast.stmts {
        match stmt {
            UVGlobalStmt::TypeDeclaration { name, type_ } => {
                cx_ast.type_map.insert(name.clone(), type_.clone());
            },

            UVGlobalStmt::Import(import) =>
                cx_ast.imports.push(import.clone()),

            UVGlobalStmt::HandledInternally => (),

            UVGlobalStmt::Function { return_type, name, params, body }
                => cx_ast.global_stmts.push(mold_function(return_type, name, params, body)?),

            _ => todo!()
        }
    }

    Some(())
}

fn mold_function(return_type: &CXValType, name: &UVIdent, params: &Vec<CXParameter>, body: &Option<UVExpr>) -> Option<CXGlobalStmt> {
    let prototype = match name {
        UVIdent::Identifier(name) => {
            CXFunctionPrototype {
                name: name.clone(),
                return_type: return_type.clone(),
                parameters: params.clone()
            }
        },

        UVIdent::ScopedIdentifier(names) => {
            let mut params = params.clone();

            if let Some(first_param) = params.get_mut(0) {
                if first_param.type_ == CXValType::Identifier("this".to_string()) &&
                    first_param.name.is_none() {

                    *first_param = CXParameter {
                        name: Some("this".to_string()),
                        type_: CXValType::PointerTo(
                            Box::new(CXValType::Identifier(names.get(0)?.clone()))
                        )
                    }
                }
            }

            CXFunctionPrototype {
                name: namespace_mangle(names),
                return_type: return_type.clone(),
                parameters: params
            }
        },
    };

    Some(
        match body {
            Some(body) =>
                CXGlobalStmt::FunctionDefinition {
                    prototype: prototype.clone(),
                    body: mold_expression(body)?
                },
            None =>
                CXGlobalStmt::FunctionForward {
                    prototype: prototype.clone()
                }
        }
    )
}
