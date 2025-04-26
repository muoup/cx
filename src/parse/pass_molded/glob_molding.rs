// Handles Converting Abstract Expression Shapes to Concrete Meaning

use std::any::Any;
use std::clone;
use crate::log_error;
use crate::parse::pass_unverified::{UVBinOp, UVExpr, UVGlobalStmt, UVAST};
use crate::parse::pass_molded::{CXExpr, CXFunctionPrototype, CXGlobalStmt, CXParameter, CXAST};
use crate::parse::pass_molded::expr_molding::{mold_expression, mold_type, split_initialization};
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
    let Some((type_, fn_call)) = split_initialization(expr) else {
        log_error!("Failed to split initialization: {}", expr);
    };

    let UVExpr::Compound { left, right } = fn_call else {
        log_error!("Failed to parse function: {}", expr);
    };

    let UVExpr::Identifier(name) = left.as_ref() else {
        log_error!("Failed to parse function name: {}", expr);
    };

    let UVExpr::Parenthesized(expr) = right.as_ref() else {
        log_error!("Failed to parse function parameters: {}", expr);
    };

    let params = match expr {
        None => vec![],
        Some(expr) => {
            let Some(expr) = mold_parameters(expr.as_ref()) else {
                log_error!("Failed to parse function parameters: {}", expr);
            };

            expr
        }
    };

    Some(
        CXFunctionPrototype {
            name: name.clone(),
            return_type: type_,
            parameters: params,
        }
    )
}

pub(crate) fn mold_parameters(expr: &UVExpr) -> Option<Vec<CXParameter>> {
    let Some(delimited) = mold_delimited(expr, UVBinOp::Comma) else {
        log_error!("Failed to mold parameters: {}", expr);
    };

    delimited
        .iter()
        .map(mold_param)
        .collect::<Option<Vec<_>>>()
}

pub(crate) fn mold_param(expr: &PseudoUVExpr) -> Option<CXParameter> {
    match expr {
        PseudoUVExpr::ID(expr) => {
            let Some((type_, name)) = split_initialization(expr) else {
                log_error!("Failed to split initialization: {}", expr);
            };

            let UVExpr::Identifier(name) = name else {
                log_error!("Failed to parse function name: {}", expr);
            };

            Some(CXParameter {
                name: Some(name.clone()),
                type_,
            })
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
