use std::{collections::HashMap, path::Path};

use cx_mir::mir::types::MIRFunctionPrototype;
use cx_safe_ir::{
    ast::{FMIRNode, FMIRNodeBody},
    intrinsic::FMIRIntrinsicKind,
};
use cx_util::CXResult;

use crate::{
    AnalysisDiagnosticContext,
    traversal::{VisitControl, walk_pre_order},
};

pub mod binary;
pub mod unary;

#[derive(Clone, Debug)]
pub enum ConstValue {
    Bool(bool),
    Int(i64),
    Float(f64),
    Unit,
}

pub fn int_to_bool(value: i64) -> bool {
    value != 0
}

pub fn bool_to_int(value: bool) -> i64 {
    if value { 1 } else { 0 }
}

pub fn evaluate_const(
    node: &FMIRNode,
    scoped_variables: &HashMap<String, ConstValue>,
) -> Option<ConstValue> {
    match &node.body {
        FMIRNodeBody::IntegerLiteral(value) => Some(ConstValue::Int(*value)),
        FMIRNodeBody::FloatLiteral(value) => Some(ConstValue::Float(*value)),
        FMIRNodeBody::BooleanLiteral(value) => Some(ConstValue::Bool(*value)),
        FMIRNodeBody::Unit => Some(ConstValue::Unit),
        FMIRNodeBody::VariableAlias { name } => scoped_variables.get(name).cloned(),
        FMIRNodeBody::CReturn { value } => evaluate_const(value, scoped_variables),
        FMIRNodeBody::Then { second, .. } => evaluate_const(second, scoped_variables),
        FMIRNodeBody::If {
            condition,
            then_branch,
            else_branch,
        } => match evaluate_const(condition, scoped_variables) {
            Some(ConstValue::Bool(true)) => evaluate_const(then_branch, scoped_variables),
            Some(ConstValue::Bool(false)) => evaluate_const(else_branch, scoped_variables),
            Some(ConstValue::Int(value)) if int_to_bool(value) => {
                evaluate_const(then_branch, scoped_variables)
            }
            Some(ConstValue::Int(value)) if !int_to_bool(value) => {
                evaluate_const(else_branch, scoped_variables)
            }
            _ => None,
        },
        FMIRNodeBody::Bind {
            monad,
            capture,
            function,
        } => {
            let value = evaluate_const(monad, scoped_variables)?;
            let mut scoped = scoped_variables.clone();
            scoped.insert(capture.as_string(), value);
            evaluate_const(function, &scoped)
        }
        FMIRNodeBody::Application { function, argument } => {
            let argument_value = evaluate_const(argument, scoped_variables)?;

            if let FMIRNodeBody::IntrinsicFunction(intrinsic) = &function.body {
                match &intrinsic.kind {
                    FMIRIntrinsicKind::Unary(op) => return unary::eval_unary(op, argument_value),
                    FMIRIntrinsicKind::Cast(op) => return unary::eval_cast(op, argument_value),
                    FMIRIntrinsicKind::Binary(_) => {}
                };
            }

            let FMIRNodeBody::Application {
                function: nested_function,
                argument: left_argument,
            } = &function.body
            else {
                return None;
            };

            let FMIRNodeBody::IntrinsicFunction(intrinsic) = &nested_function.body else {
                return None;
            };
            let FMIRIntrinsicKind::Binary(binary_intrinsic) = &intrinsic.kind else {
                return None;
            };

            let left_value = evaluate_const(left_argument, scoped_variables)?;
            binary::eval_binop(binary_intrinsic, left_value, argument_value)
        }
        _ => None,
    }
}

pub fn assert_proven_conditions(
    function_prototype: &MIRFunctionPrototype,
    root: &FMIRNode,
    compilation_unit: &Path,
) -> CXResult<()> {
    let diagnostics = AnalysisDiagnosticContext::new(function_prototype, compilation_unit);
    let mut visit = |node: &FMIRNode| -> CXResult<VisitControl> {
        let FMIRNodeBody::CompilerAssert { condition, message } = &node.body else {
            return Ok(VisitControl::Continue);
        };

        match evaluate_const(condition, &HashMap::new()) {
            Some(ConstValue::Bool(false)) | Some(ConstValue::Int(0)) => {
                diagnostics.fail_proven_false(message, node, condition)
            }
            _ => Ok(VisitControl::Continue),
        }
    };

    walk_pre_order(root, &mut visit)
}
