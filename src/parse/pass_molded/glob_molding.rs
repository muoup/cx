// Handles Converting Abstract Expression Shapes to Concrete Meaning

use std::any::Any;
use crate::log_error;
use crate::parse::expression::parse_expression;
use crate::parse::pass_unverified::{UVBinOp, UVExpr, UVGlobalStmt, UVAST};
use crate::parse::pass_molded::{CXExpr, CXGlobalStmt, CXParameter, CXAST};
use crate::parse::pass_molded::expr_molding::{mold_expression, mold_type, split_initialization};
use crate::parse::pass_molded::pattern_molding::{mold_delimited, PseudoUVExpr};

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

            UVGlobalStmt::SingleExpression { expression} => {
                cx_ast.global_stmts.push(
                    mold_global_single(expression)?
                );
            },

            UVGlobalStmt::Import(import) =>
                cx_ast.imports.push(import.clone()),

            UVGlobalStmt::HandledInternally => (),
        }
    }

    Some(())
}

pub(crate) fn mold_global_bodied(header: &UVExpr, body: &UVExpr) -> Option<CXGlobalStmt> {
    // For now just assume this is a function definition
    let Some((type_, fn_call)) = split_initialization(header) else {
        log_error!("Failed to split initialization: {}", header);
    };

    let Some(body) = mold_expression(body) else {
        log_error!("Failed to parse body: {}", body);
    };

    let UVExpr::Compound { left, right } = fn_call else {
         log_error!("Failed to parse function: {}", header);
    };

    match (left.as_ref(), right.as_ref()) {
        (UVExpr::Identifier(name), UVExpr::Parenthesized(expr)) => {
            let parameters = expr
                .iter()
                .map(|expr| mold_param(expr))
                .collect::<Option<Vec<_>>>()?;

            Some(CXGlobalStmt::FunctionDefinition {
                name: name.clone(),
                return_type: type_,
                parameters,
                body,
            })
        },

        _ => todo!()
    }
}

pub(crate) fn mold_parameters(expr: &UVExpr) -> Option<Vec<CXParameter>> {
    mold_delimited(expr, &UVBinOp::Comma)?
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

            let UVExpr::Identifier(name) = name.as_ref() else {
                log_error!("Failed to parse function name: {}", expr);
            };

            Some(CXParameter {
                name: Some(name.clone()),
                type_,
            })
        },

        PseudoUVExpr::BinOp { left, right, op: UVBinOp::Multiply } => {
            let PseudoUVExpr::ID(left) = *left.as_ref() else {
                log_error!("Failed to parse function name: {}", left);
            };

            let PseudoUVExpr::ID(right) = *right.as_ref() else {
                log_error!("Failed to parse function name: {}", right);
            };

            todo!()
        }
    }
}

pub(crate) fn mold_global_single(expression: &UVExpr) -> Option<CXGlobalStmt> {
    match expression {
        UVExpr::Compound { .. } => {
            let Some((type_, fn_call)) = split_initialization(expression) else {
                log_error!("Failed to split initialization: {}", expression);
            };

            let UVExpr::Compound { left, right } = fn_call else {
                log_error!("Failed to parse function: {}", expression);
            };

            let UVExpr::Identifier(name) = left.as_ref() else {
                log_error!("Failed to parse function name: {}", expression);
            };

            let UVExpr::Parenthesized(expr) = right.as_ref() else {
                log_error!("Failed to parse function parameters: {}", expression);
            };

            let params = match expr {
                None => vec![],
                Some(expr) => {
                    mold_parameters(expr.as_ref())?
                }
            }

            todo!()
        },

        _ => log_error!("Failed to parse single expression: {}", expression)
    };
}