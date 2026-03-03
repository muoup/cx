use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use cx_mir::mir::program::{MIRFunction, MIRUnit};
use cx_safe_ir::ast::{FMIRFunction, FMIRNode, FMIRNodeBody};
use cx_util::{CXError, CXResult};

use crate::mir_conversion::{convert_mir, environment::FMIREnvironment};

pub(crate) mod mir_conversion;

pub type FMIRAnalysisPass<'a> = &'a dyn Fn(&FMIRContext, FMIRFunction) -> CXResult<FMIRFunction>;

pub struct FMIRContext {
    env: FMIREnvironment,
    functions: HashMap<String, FMIRFunction>,
}

#[derive(Clone, Debug)]
enum ConstValue {
    Bool(bool),
    Int(i64),
    Float(f64),
    Unit,
}

impl FMIRContext {
    pub fn new() -> Self {
        FMIRContext {
            env: FMIREnvironment::new(),
            functions: HashMap::new(),
        }
    }

    pub fn new_from(mir: &MIRUnit) -> CXResult<Self> {
        let mut context = FMIRContext::new();

        for function in mir.functions.iter() {
            if !function.prototype.contract.safe {
                continue;
            }

            context.consume_mir_function(function)?;
        }

        Ok(context)
    }

    pub fn consume_mir_function(&mut self, mir_function: &MIRFunction) -> CXResult<()> {
        let fmir_function = convert_mir(&mut self.env, mir_function)?;

        self.functions
            .insert(mir_function.prototype.name.as_string(), fmir_function);

        Ok(())
    }

    pub fn apply_analysis_pass(&mut self, pass: FMIRAnalysisPass) -> CXResult<()> {
        let mut next_functions = HashMap::with_capacity(self.functions.len());
        for (name, function) in self.functions.iter() {
            next_functions.insert(name.clone(), pass(self, function.clone())?);
        }
        self.functions = next_functions;
        Ok(())
    }

    pub fn apply_standard_analysis_passes(&mut self) -> CXResult<()> {
        for (name, function) in self.functions.iter() {
            assert_proven_conditions(name, &function.body)?;
        }

        Ok(())
    }

    pub fn drain_functions(&mut self) -> Vec<(String, FMIRFunction)> {
        self.functions.drain().collect()
    }
}

fn int_to_bool(value: i64) -> bool {
    value != 0
}

fn bool_to_int(value: bool) -> i64 {
    if value { 1 } else { 0 }
}

fn eval_unary_operator(name: &str, arg: ConstValue) -> Option<ConstValue> {
    match (name, arg) {
        ("__op_neg", value) | ("__op_ineg", value) => match value {
            ConstValue::Int(inner) => Some(ConstValue::Int(-inner)),
            _ => None,
        },
        ("__op_fneg", ConstValue::Float(inner)) => Some(ConstValue::Float(-inner)),
        ("__op_bnot", ConstValue::Int(inner)) => Some(ConstValue::Int(!inner)),
        ("__op_lnot", ConstValue::Bool(inner)) => Some(ConstValue::Bool(!inner)),
        ("__op_lnot", ConstValue::Int(inner)) => Some(ConstValue::Bool(!int_to_bool(inner))),
        (op, ConstValue::Int(inner)) if op.starts_with("__op_preinc_") => {
            let increment = op.rsplit_once('_')?.1.parse::<i64>().ok()?;
            Some(ConstValue::Int(inner + increment))
        }
        (op, ConstValue::Int(inner)) if op.starts_with("__op_postinc_") => {
            let _ = op.rsplit_once('_')?.1.parse::<i64>().ok()?;
            Some(ConstValue::Int(inner))
        }
        _ => None,
    }
}

fn eval_int_binary_operator(suffix: &str, left: i64, right: i64) -> Option<ConstValue> {
    match suffix {
        "add" => Some(ConstValue::Int(left + right)),
        "sub" => Some(ConstValue::Int(left - right)),
        "mul" | "imul" => Some(ConstValue::Int(left * right)),
        "div" | "idiv" => {
            if right == 0 {
                None
            } else {
                Some(ConstValue::Int(left / right))
            }
        }
        "mod" | "imod" => {
            if right == 0 {
                None
            } else {
                Some(ConstValue::Int(left % right))
            }
        }
        "eq" => Some(ConstValue::Bool(left == right)),
        "ne" => Some(ConstValue::Bool(left != right)),
        "lt" | "ilt" => Some(ConstValue::Bool(left < right)),
        "le" | "ile" => Some(ConstValue::Bool(left <= right)),
        "gt" | "igt" => Some(ConstValue::Bool(left > right)),
        "ge" | "ige" => Some(ConstValue::Bool(left >= right)),
        "land" => Some(ConstValue::Bool(int_to_bool(left) && int_to_bool(right))),
        "lor" => Some(ConstValue::Bool(int_to_bool(left) || int_to_bool(right))),
        "band" => Some(ConstValue::Int(left & right)),
        "bor" => Some(ConstValue::Int(left | right)),
        "bxor" => Some(ConstValue::Int(left ^ right)),
        _ => None,
    }
}

fn eval_float_binary_operator(suffix: &str, left: f64, right: f64) -> Option<ConstValue> {
    match suffix {
        "add" => Some(ConstValue::Float(left + right)),
        "sub" => Some(ConstValue::Float(left - right)),
        "mul" => Some(ConstValue::Float(left * right)),
        "div" => Some(ConstValue::Float(left / right)),
        "eq" => Some(ConstValue::Bool(left == right)),
        "ne" => Some(ConstValue::Bool(left != right)),
        "lt" => Some(ConstValue::Bool(left < right)),
        "le" => Some(ConstValue::Bool(left <= right)),
        "gt" => Some(ConstValue::Bool(left > right)),
        "ge" => Some(ConstValue::Bool(left >= right)),
        _ => None,
    }
}

fn eval_binary_operator(name: &str, left: ConstValue, right: ConstValue) -> Option<ConstValue> {
    if let Some(suffix) = name.strip_prefix("__op_i").and_then(|s| s.split_once('_').map(|v| v.1))
    {
        let left = match left {
            ConstValue::Int(value) => value,
            ConstValue::Bool(value) => bool_to_int(value),
            _ => return None,
        };
        let right = match right {
            ConstValue::Int(value) => value,
            ConstValue::Bool(value) => bool_to_int(value),
            _ => return None,
        };
        return eval_int_binary_operator(suffix, left, right);
    }

    if let Some(suffix) = name.strip_prefix("__op_f").and_then(|s| s.split_once('_').map(|v| v.1))
    {
        let left = match left {
            ConstValue::Float(value) => value,
            _ => return None,
        };
        let right = match right {
            ConstValue::Float(value) => value,
            _ => return None,
        };
        return eval_float_binary_operator(suffix, left, right);
    }

    if let Some(suffix) = name.strip_prefix("__op_ptr_") {
        let left = match left {
            ConstValue::Int(value) => value,
            _ => return None,
        };
        let right = match right {
            ConstValue::Int(value) => value,
            _ => return None,
        };
        return eval_int_binary_operator(suffix, left, right);
    }

    if let Some(suffix) = name.strip_prefix("__op_ptrdiff_") {
        let left = match left {
            ConstValue::Int(value) => value,
            _ => return None,
        };
        let right = match right {
            ConstValue::Int(value) => value,
            _ => return None,
        };
        return eval_int_binary_operator(suffix, left, right);
    }

    None
}

fn evaluate_const(
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
            Some(ConstValue::Int(_)) => evaluate_const(else_branch, scoped_variables),
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

            if let FMIRNodeBody::VariableAlias { name } = &function.body {
                return eval_unary_operator(name.as_str(), argument_value);
            }

            let FMIRNodeBody::Application {
                function: nested_function,
                argument: left_argument,
            } = &function.body
            else {
                return None;
            };

            let FMIRNodeBody::VariableAlias { name } = &nested_function.body else {
                return None;
            };

            let left_value = evaluate_const(left_argument, scoped_variables)?;
            eval_binary_operator(name, left_value, argument_value)
        }
        _ => None,
    }
}

fn assert_proven_conditions(function_name: &str, node: &FMIRNode) -> CXResult<()> {
    if let FMIRNodeBody::CompilerAssert { condition, message } = &node.body {
        match evaluate_const(condition, &HashMap::new()) {
            Some(ConstValue::Bool(false)) => {
                return CXError::create_result(format!(
                    "FMIR analysis error in safe function '{}': {} (condition proven false)",
                    function_name, message
                ));
            }
            Some(ConstValue::Int(0)) => {
                return CXError::create_result(format!(
                    "FMIR analysis error in safe function '{}': {} (condition proven false)",
                    function_name, message
                ));
            }
            _ => {}
        }
    }

    match &node.body {
        FMIRNodeBody::Application { function, argument } => {
            assert_proven_conditions(function_name, function)?;
            assert_proven_conditions(function_name, argument)?;
        }
        FMIRNodeBody::CompilerAssert { condition, .. } => {
            assert_proven_conditions(function_name, condition)?;
        }
        FMIRNodeBody::Bind {
            monad, function, ..
        } => {
            assert_proven_conditions(function_name, monad)?;
            assert_proven_conditions(function_name, function)?;
        }
        FMIRNodeBody::Then { first, second } => {
            assert_proven_conditions(function_name, first)?;
            assert_proven_conditions(function_name, second)?;
        }
        FMIRNodeBody::Load { pointer } => {
            assert_proven_conditions(function_name, pointer)?;
        }
        FMIRNodeBody::Store { pointer, value } => {
            assert_proven_conditions(function_name, pointer)?;
            assert_proven_conditions(function_name, value)?;
        }
        FMIRNodeBody::If {
            condition,
            then_branch,
            else_branch,
        } => {
            assert_proven_conditions(function_name, condition)?;
            assert_proven_conditions(function_name, then_branch)?;
            assert_proven_conditions(function_name, else_branch)?;
        }
        FMIRNodeBody::CLoop { condition, body } => {
            assert_proven_conditions(function_name, condition)?;
            assert_proven_conditions(function_name, body)?;
        }
        FMIRNodeBody::CReturn { value } => {
            assert_proven_conditions(function_name, value)?;
        }
        FMIRNodeBody::DeclareAccess { .. }
        | FMIRNodeBody::Pure
        | FMIRNodeBody::UnsafeBlock
        | FMIRNodeBody::Alloca
        | FMIRNodeBody::VariableAlias { .. }
        | FMIRNodeBody::IntegerLiteral(_)
        | FMIRNodeBody::FloatLiteral(_)
        | FMIRNodeBody::BooleanLiteral(_)
        | FMIRNodeBody::Unit => {}
    }

    Ok(())
}

impl Default for FMIRContext {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for FMIRContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut names = self.functions.keys().cloned().collect::<Vec<_>>();
        names.sort();

        writeln!(f, "FMIR Context:")?;
        for name in names {
            if let Some(function) = self.functions.get(&name) {
                writeln!(f, "{function}")?;
            }
        }

        Ok(())
    }
}
