// Handles Converting Abstract Expression Shapes to Concrete Meaning

use std::any::Any;
use std::clone;
use crate::log_error;
use crate::mangling::member_function_mangle;
use crate::parse::pass_unverified::{UVBinOp, UVExpr, UVGlobalStmt, UVAST};
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

            UVGlobalStmt::BodiedExpression { header, body } => {
                cx_ast.global_stmts.push(
                    mold_global_bodied(header, body)?
                );
            },

            UVGlobalStmt::SingleExpression { expression} =>
                cx_ast.global_stmts.push(
                    mold_global_single(expression)?
                ),

            UVGlobalStmt::Import(import) =>
                cx_ast.imports.push(import.clone()),

            UVGlobalStmt::HandledInternally => (),
        }
    }

    Some(())
}

pub(crate) fn mold_global_bodied(header: &UVExpr, body: &UVExpr) -> Option<CXGlobalStmt> {
    // For now just assume this is a function definition
    let Some(prototype) = mold_function_header(header) else {
        log_error!("Failed to mold function header: {}", header);
    };

    let Some(body) = mold_expression(body) else {
        log_error!("Failed to mold function body: {}", body);
    };

    Some(
        CXGlobalStmt::FunctionDefinition {
            prototype, body
        }
    )
}

pub(crate) fn mold_global_single(expression: &UVExpr) -> Option<CXGlobalStmt> {
    match expression {
        UVExpr::Compound { .. } => {
            let Some(header) = mold_function_header(expression) else {
                log_error!("Failed to mold function header: {}", expression);
            };

            Some(
                CXGlobalStmt::FunctionForward {
                    prototype: CXFunctionPrototype {
                        name: header.name,
                        return_type: header.return_type,
                        parameters: header.parameters
                    }
                }
            )
        },

        _ => log_error!("Failed to parse single expression: {}", expression)
    }
}

pub(crate) fn mold_function_header(expr: &UVExpr) -> Option<CXFunctionPrototype> {
    let Some((type_, Some(fn_call))) = split_initialization(expr) else {
        log_error!("Failed to split initialization: {}", expr);
    };

    match fn_call {
        UVExpr::Compound { left, right }
            => mold_function(type_, left.as_ref(), right.as_ref()),
        UVExpr::Complex { expr_stack, op_stack}
            => mold_member_function(type_, expr_stack, op_stack),

        _ => log_error!("Failed to parse function name: {} {}", type_, fn_call),
    }
}

fn mold_function(type_: CXValType, name: &UVExpr, parenthesized: &UVExpr) -> Option<CXFunctionPrototype> {
    let UVExpr::Identifier(name) = name else {
        log_error!("Failed to parse function name: {:?}", name);
    };

    let UVExpr::Parenthesized(parenthesized) = parenthesized else {
        log_error!("Failed to parse function name: {:?}", name);
    };

    let param_expr =
        match parenthesized {
            Some(expr) => Some(expr.as_ref()),
            None => None,
        };
    let Some(params) = mold_parameters(param_expr) else {
        log_error!("Failed to parse function parameters: {:?}", parenthesized);
    };

    Some(
        CXFunctionPrototype {
            name: name.clone(),
            return_type: type_,
            parameters: params,
        }
    )
}

fn mold_member_function(type_: CXValType, expr_stack: &Vec<UVExpr>, op_stack: &Vec<UVBinOp>) -> Option<CXFunctionPrototype> {
    let molded_expr_stack = mold_expr_stack(expr_stack, op_stack)?;

    let PseudoUVExpr::BinOp {
        left, right,
        op: UVBinOp::ScopeRes
    } = molded_expr_stack else  {
        log_error!("Failed to parse function name: {:?}", molded_expr_stack)
    };

    let PseudoUVExpr::ID(UVExpr::Identifier(class_name)) = left.as_ref() else {
        log_error!("Failed to parse class name: {:?}", left);
    };

    let (fn_name, paren) = {
        let PseudoUVExpr::ID(UVExpr::Compound { left, right }) = right.as_ref() else {
            log_error!("Failed to parse function name: {:?}", right);
        };

        let UVExpr::Identifier(fn_name) = left.as_ref() else {
            log_error!("Failed to parse function name: {:?}", left);
        };

        let UVExpr::Parenthesized(paren) = right.as_ref() else {
            log_error!("Failed to parse function name: {:?}", right);
        };

        (fn_name.as_str(), paren)
    };

    let name = member_function_mangle(
        class_name,
        fn_name
    );

    let mut params = mold_parameters(
        match paren {
            Some(expr) => Some(expr.as_ref()),
            None => None,
        }
    )?;

    let Some(first_param) = params.first_mut() else {
        log_error!("Missing \"this\" parameter in struct method, found empty param set");
    };

    let CXParameter { name: None, type_: ref this } = first_param else {
        log_error!("Missing \"this\" parameter in struct method, found")
    };

    let CXValType::Identifier(ref this) = this else {
        log_error!("Missing \"this\" parameter in struct method")
    };

    if this != "this" {
        log_error!("Expected \"this\" parameter in struct method")
    }

    *first_param = CXParameter {
        name: Some("this".to_string()),
        type_: CXValType::PointerTo(Box::new(CXValType::Identifier(class_name.clone())))
    };

    Some(
        CXFunctionPrototype {
            name,
            return_type: type_,
            parameters: params,
        }
    )
}

pub(crate) fn mold_parameters(expr: Option<&UVExpr>) -> Option<Vec<CXParameter>> {
    match expr {
        None => Some(vec![]),
        Some(expr) => {
            let Some(delimited) = mold_delimited(expr, UVBinOp::Comma) else {
                log_error!("Failed to mold parameters: {}", expr);
            };

            delimited
                .iter()
                .map(mold_param)
                .collect::<Option<Vec<_>>>()
        }
    }
}

pub(crate) fn mold_param(expr: &PseudoUVExpr) -> Option<CXParameter> {
    match expr {
        PseudoUVExpr::ID(expr) => {
            let Some((type_, name)) = split_initialization(expr) else {
                log_error!("Failed to split initialization: {}", expr);
            };

            match name {
                Some(UVExpr::Identifier(name)) =>
                    Some(CXParameter { type_, name: Some(name.clone()) }),

                None => Some(CXParameter { type_, name: None }),

                _ => log_error!("Failed to parse function parameter: {}", expr),
            }
        },

        PseudoUVExpr::BinOp { left, right, op: UVBinOp::Multiply } => {
            let PseudoUVExpr::ID(left) = *left.as_ref() else {
                log_error!("Failed to parse function name: {:?}", left);
            };

            let PseudoUVExpr::ID(UVExpr::Identifier(right)) = *right.as_ref() else {
                log_error!("Failed to parse function name: {:?}", right);
            };

            Some(
                CXParameter {
                    name: Some(right.clone()),
                    type_: CXValType::PointerTo(Box::new(mold_type(left)?))
                }
            )
        },

        PseudoUVExpr::BinOp { .. } =>
            log_error!("Failed to parse parameter name: {:?}", expr),
    }
}
