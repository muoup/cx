// Handles Converting Abstract Expression Shapes to Concrete Meaning

use std::any::Any;
use crate::log_error;
use crate::parse::expression::parse_expression;
use crate::parse::pass_unverified::{UVExpr, UVGlobalStmt, UVAST};
use crate::parse::pass_molded::{CXExpr, CXGlobalStmt, CXParameter, CXAST};
use crate::parse::pass_molded::expr_molding::{mold_expression, split_initialization};

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
    if let UVExpr::Compound { .. } = expr {
        return Some(vec![mold_param(expr)?]);
    }

    let UVExpr::Complex { .. } = expr else {
        log_error!("Failed to parse parameters: {}", expr);
    };

    todo!()
}

pub(crate) fn mold_param(expr: &UVExpr) -> Option<CXParameter> {
    let Some((left, right)) = split_initialization(expr) else {
        log_error!("Failed to split initialization: {}", expr);
    };

    let UVExpr::Identifier(ident) = right else {
        log_error!("Failed to parse parameter name: {}", expr);
    };

    Some(
        CXParameter {
            name: Some(ident.clone()),
            type_: left,
        }
    )
}

pub(crate) fn mold_global_single(expression: &UVExpr) -> Option<CXGlobalStmt> {
    todo!()
}