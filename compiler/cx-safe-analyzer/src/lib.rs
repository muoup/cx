use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};

use cx_mir::mir::program::{MIRFunction, MIRUnit};
use cx_mir::mir::types::{MIRFunctionPrototype, MIRIntegerType, MIRType, MIRTypeKind};
use cx_safe_ir::ast::{FMIRFunction, FMIRNode, FMIRNodeBody, FMIRSourceRange};
use cx_safe_ir::intrinsic::{
    FMIRBinaryIntrinsic, FMIRCastIntrinsic, FMIRIntrinsicFBinOp,
    FMIRIntrinsicIBinOp, FMIRIntrinsicKind, FMIRPointerBinaryIntrinsicOp,
    FMIRPointerDiffBinaryIntrinsicOp, FMIRUnaryIntrinsic,
};
use cx_util::{CXError, CXErrorTrait, CXResult};

use crate::mir_conversion::{convert_mir, environment::FMIREnvironment};
use crate::traversal::{VisitControl, walk_pre_order};

pub(crate) mod mir_conversion;
pub(crate) mod traversal;

pub type FMIRAnalysisPass<'a> = &'a dyn Fn(&FMIRContext, FMIRFunction) -> CXResult<FMIRFunction>;

pub struct FMIRContext {
    env: FMIREnvironment,
    functions: HashMap<String, FMIRFunction>,
}

struct FMIRAnalysisError {
    message: String,
    compilation_unit: PathBuf,
    token_start: usize,
    token_end: usize,
}

impl CXErrorTrait for FMIRAnalysisError {
    fn pretty_print(&self) {
        cx_log::pretty_underline_error(
            &self.message,
            &self.compilation_unit,
            self.token_start,
            self.token_end,
        );
    }
}

#[derive(Clone, Debug)]
enum ConstValue {
    Bool(bool),
    Int(i64),
    Float(f64),
    Unit,
}

struct AnalysisDiagnosticContext {
    compilation_unit: PathBuf,
    file_contents: Option<String>,
    function_name: String,
    function_signature: String,
}

impl AnalysisDiagnosticContext {
    fn new(function_prototype: &MIRFunctionPrototype, compilation_unit: &Path) -> Self {
        Self {
            compilation_unit: compilation_unit.to_path_buf(),
            file_contents: std::fs::read_to_string(compilation_unit).ok(),
            function_name: function_prototype.name.as_string(),
            function_signature: format_function_signature(function_prototype),
        }
    }

    fn source_text_for_range(&self, range: &FMIRSourceRange) -> Option<String> {
        let file_contents = self.file_contents.as_ref()?;
        let tokens = cx_lexer::lex(file_contents)?;

        let start_token = tokens.get(range.start_token)?;
        let end_token = tokens.get(range.end_token.saturating_sub(1))?;
        if start_token.file_origin != end_token.file_origin {
            return None;
        }

        let source_slice = file_contents
            .get(start_token.start_index..end_token.end_index)?
            .trim();
        Some(source_slice.to_string())
    }

    fn failure_message(&self, message: &str, condition: &FMIRNode) -> String {
        if let Some(ret_name) = message.strip_prefix("postcondition failed:") {
            let post_condition_expr = condition
                .source_range
                .as_ref()
                .and_then(|range| self.source_text_for_range(range))
                .unwrap_or_else(|| "<unknown post-condition expression>".to_string());
            return format!(
                "In function `{}`, contract condition\n   post({}): ({})\n\nwill never be true at return site",
                self.function_signature, ret_name, post_condition_expr
            );
        }

        format!(
            "FMIR analysis error in safe function '{}': {} (condition proven false)",
            self.function_name, message
        )
    }

    fn fail_proven_false(
        &self,
        message: &str,
        node: &FMIRNode,
        condition: &FMIRNode,
    ) -> CXResult<VisitControl> {
        let resolved_message = self.failure_message(message, condition);

        if let Some(range) = node
            .source_range
            .as_ref()
            .or(condition.source_range.as_ref())
        {
            return Err(Box::new(FMIRAnalysisError {
                message: resolved_message,
                compilation_unit: self.compilation_unit.clone(),
                token_start: range.start_token,
                token_end: range.end_token,
            }) as Box<dyn CXErrorTrait>);
        }

        CXError::create_result(resolved_message)
    }
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

    pub fn apply_standard_analysis_passes(&mut self, compilation_unit: &Path) -> CXResult<()> {
        for function in self.functions.values() {
            assert_proven_conditions(&function.prototype, &function.body, compilation_unit)?;
        }

        Ok(())
    }

    pub fn drain_functions(&mut self) -> Vec<(String, FMIRFunction)> {
        self.functions.drain().collect()
    }
}

fn format_human_readable_type(mir_type: &MIRType) -> String {
    match &mir_type.kind {
        MIRTypeKind::Integer {
            _type: MIRIntegerType::I1,
            signed: false,
        } => "bool".to_string(),
        MIRTypeKind::Integer {
            _type: MIRIntegerType::I8,
            signed: true,
        } => "char".to_string(),
        MIRTypeKind::Integer {
            _type: MIRIntegerType::I8,
            signed: false,
        } => "unsigned char".to_string(),
        MIRTypeKind::Integer {
            _type: MIRIntegerType::I16,
            signed: true,
        } => "short".to_string(),
        MIRTypeKind::Integer {
            _type: MIRIntegerType::I16,
            signed: false,
        } => "unsigned short".to_string(),
        MIRTypeKind::Integer {
            _type: MIRIntegerType::I32,
            signed: true,
        } => "int".to_string(),
        MIRTypeKind::Integer {
            _type: MIRIntegerType::I32,
            signed: false,
        } => "unsigned int".to_string(),
        MIRTypeKind::Integer {
            _type: MIRIntegerType::I64,
            signed: true,
        } => "long".to_string(),
        MIRTypeKind::Integer {
            _type: MIRIntegerType::I64,
            signed: false,
        } => "unsigned long".to_string(),
        _ => format!("{mir_type}"),
    }
}

fn format_function_signature(prototype: &MIRFunctionPrototype) -> String {
    let mut signature = format!(
        "{} {}(",
        format_human_readable_type(&prototype.return_type),
        prototype.name
    );

    for (index, param) in prototype.params.iter().enumerate() {
        if index > 0 {
            signature.push_str(", ");
        }

        if let Some(name) = &param.name {
            signature.push_str(
                format!("{} {}", format_human_readable_type(&param._type), name).as_str(),
            );
        } else {
            signature.push_str(format_human_readable_type(&param._type).as_str());
        }
    }

    if prototype.var_args {
        if !prototype.params.is_empty() {
            signature.push_str(", ");
        }
        signature.push_str("...");
    }

    signature.push(')');
    signature
}

fn int_to_bool(value: i64) -> bool {
    value != 0
}

fn bool_to_int(value: bool) -> i64 {
    if value { 1 } else { 0 }
}

fn eval_unary_intrinsic(op: &FMIRUnaryIntrinsic, arg: ConstValue) -> Option<ConstValue> {
    match (op, arg) {
        (FMIRUnaryIntrinsic::Neg, ConstValue::Int(inner))
        | (FMIRUnaryIntrinsic::INeg, ConstValue::Int(inner)) => Some(ConstValue::Int(-inner)),
        (FMIRUnaryIntrinsic::FNeg, ConstValue::Float(inner)) => Some(ConstValue::Float(-inner)),
        (FMIRUnaryIntrinsic::BNot, ConstValue::Int(inner)) => Some(ConstValue::Int(!inner)),
        (FMIRUnaryIntrinsic::LNot, ConstValue::Bool(inner)) => Some(ConstValue::Bool(!inner)),
        (FMIRUnaryIntrinsic::LNot, ConstValue::Int(inner)) => {
            Some(ConstValue::Bool(!int_to_bool(inner)))
        }
        _ => None,
    }
}

fn eval_int_binary_operator(
    op: &FMIRIntrinsicIBinOp,
    left: i64,
    right: i64,
) -> Option<ConstValue> {
    match op {
        FMIRIntrinsicIBinOp::Add => Some(ConstValue::Int(left + right)),
        FMIRIntrinsicIBinOp::Sub => Some(ConstValue::Int(left - right)),
        FMIRIntrinsicIBinOp::Mul | FMIRIntrinsicIBinOp::IMul => {
            Some(ConstValue::Int(left * right))
        }
        FMIRIntrinsicIBinOp::Div | FMIRIntrinsicIBinOp::IDiv => {
            if right == 0 {
                None
            } else {
                Some(ConstValue::Int(left / right))
            }
        }
        FMIRIntrinsicIBinOp::Mod | FMIRIntrinsicIBinOp::IMod => {
            if right == 0 {
                None
            } else {
                Some(ConstValue::Int(left % right))
            }
        }
        FMIRIntrinsicIBinOp::Eq => Some(ConstValue::Bool(left == right)),
        FMIRIntrinsicIBinOp::Ne => Some(ConstValue::Bool(left != right)),
        FMIRIntrinsicIBinOp::Lt | FMIRIntrinsicIBinOp::ILt => {
            Some(ConstValue::Bool(left < right))
        }
        FMIRIntrinsicIBinOp::Le | FMIRIntrinsicIBinOp::ILe => {
            Some(ConstValue::Bool(left <= right))
        }
        FMIRIntrinsicIBinOp::Gt | FMIRIntrinsicIBinOp::IGt => {
            Some(ConstValue::Bool(left > right))
        }
        FMIRIntrinsicIBinOp::Ge | FMIRIntrinsicIBinOp::IGe => {
            Some(ConstValue::Bool(left >= right))
        }
        FMIRIntrinsicIBinOp::LAnd => {
            Some(ConstValue::Bool(int_to_bool(left) && int_to_bool(right)))
        }
        FMIRIntrinsicIBinOp::LOr => {
            Some(ConstValue::Bool(int_to_bool(left) || int_to_bool(right)))
        }
        FMIRIntrinsicIBinOp::BAnd => Some(ConstValue::Int(left & right)),
        FMIRIntrinsicIBinOp::BOr => Some(ConstValue::Int(left | right)),
        FMIRIntrinsicIBinOp::BXor => Some(ConstValue::Int(left ^ right)),
    }
}

fn eval_float_binary_operator(
    op: &FMIRIntrinsicFBinOp,
    left: f64,
    right: f64,
) -> Option<ConstValue> {
    match op {
        FMIRIntrinsicFBinOp::FADD => Some(ConstValue::Float(left + right)),
        FMIRIntrinsicFBinOp::FSUB => Some(ConstValue::Float(left - right)),
        FMIRIntrinsicFBinOp::FMUL => Some(ConstValue::Float(left * right)),
        FMIRIntrinsicFBinOp::FDIV => Some(ConstValue::Float(left / right)),
        FMIRIntrinsicFBinOp::EQ => Some(ConstValue::Bool(left == right)),
        FMIRIntrinsicFBinOp::FNE => Some(ConstValue::Bool(left != right)),
        FMIRIntrinsicFBinOp::FLT => Some(ConstValue::Bool(left < right)),
        FMIRIntrinsicFBinOp::FLE => Some(ConstValue::Bool(left <= right)),
        FMIRIntrinsicFBinOp::FGT => Some(ConstValue::Bool(left > right)),
        FMIRIntrinsicFBinOp::FGE => Some(ConstValue::Bool(left >= right)),
    }
}

fn eval_binary_intrinsic(
    intrinsic: &FMIRBinaryIntrinsic,
    left: ConstValue,
    right: ConstValue,
) -> Option<ConstValue> {
    match intrinsic {
        FMIRBinaryIntrinsic::Integer { op, .. } => {
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
            eval_int_binary_operator(op, left, right)
        }
        FMIRBinaryIntrinsic::Float { op, .. } => {
            let left = match left {
                ConstValue::Float(value) => value,
                _ => return None,
            };
            let right = match right {
                ConstValue::Float(value) => value,
                _ => return None,
            };
            eval_float_binary_operator(op, left, right)
        }
        FMIRBinaryIntrinsic::Pointer { op } => {
            let op = match op {
                FMIRPointerBinaryIntrinsicOp::Eq => FMIRIntrinsicIBinOp::Eq,
                FMIRPointerBinaryIntrinsicOp::Ne => FMIRIntrinsicIBinOp::Ne,
                FMIRPointerBinaryIntrinsicOp::Lt => FMIRIntrinsicIBinOp::Lt,
                FMIRPointerBinaryIntrinsicOp::Gt => FMIRIntrinsicIBinOp::Gt,
                FMIRPointerBinaryIntrinsicOp::Le => FMIRIntrinsicIBinOp::Le,
                FMIRPointerBinaryIntrinsicOp::Ge => FMIRIntrinsicIBinOp::Ge,
            };

            let left = match left {
                ConstValue::Int(value) => value,
                _ => return None,
            };
            let right = match right {
                ConstValue::Int(value) => value,
                _ => return None,
            };
            eval_int_binary_operator(&op, left, right)
        }
        FMIRBinaryIntrinsic::PointerDiff { op } => {
            let op = match op {
                FMIRPointerDiffBinaryIntrinsicOp::Add => FMIRIntrinsicIBinOp::Add,
                FMIRPointerDiffBinaryIntrinsicOp::Sub => FMIRIntrinsicIBinOp::Sub,
            };

            let left = match left {
                ConstValue::Int(value) => value,
                _ => return None,
            };
            let right = match right {
                ConstValue::Int(value) => value,
                _ => return None,
            };
            eval_int_binary_operator(&op, left, right)
        }
    }
}

fn eval_cast_intrinsic(op: &FMIRCastIntrinsic, value: ConstValue) -> Option<ConstValue> {
    match (op, value) {
        (FMIRCastIntrinsic::IntToBool, ConstValue::Bool(value)) => Some(ConstValue::Bool(value)),
        (FMIRCastIntrinsic::IntToBool, ConstValue::Int(value)) => {
            Some(ConstValue::Bool(int_to_bool(value)))
        }
        _ => None,
    }
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

            if let FMIRNodeBody::IntrinsicFunction(intrinsic) = &function.body {
                return match &intrinsic.kind {
                    FMIRIntrinsicKind::Unary(op) => eval_unary_intrinsic(op, argument_value),
                    FMIRIntrinsicKind::Cast(op) => eval_cast_intrinsic(op, argument_value),
                    FMIRIntrinsicKind::Binary(_) => None,
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
            eval_binary_intrinsic(binary_intrinsic, left_value, argument_value)
        }
        _ => None,
    }
}

fn assert_proven_conditions(
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
