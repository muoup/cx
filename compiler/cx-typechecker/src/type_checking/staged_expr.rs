use cx_hir::ast::expression::{HIRBlockKind, HIRExprKind, HIRExpression};
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_thir::thir::{
    comptime::{THIRStagedExpr, THIRStagedParameter},
    data::{THIRComptimeValueType, THIRType},
    expression::{
        THIRExpression, THIRExpressionKind, THIRFnContract, THIRLocalID, THIRPostcondition,
    },
    pattern::THIRPattern,
};
use cx_tokens::TokenRange;
use std::collections::HashSet;

use crate::{
    environment::TypeEnvironment,
    type_checking::{
        coercion::implicit::implicit_cast,
        result::{DeferredStagedExpr, TypecheckResult},
        typechecker::typecheck_expr,
    },
};

pub fn typecheck_staged_expr(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    inner: &HIRExpression,
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    let body = external_yield_block(inner, expected_type.is_some_and(THIRType::is_void));
    let inner = body.as_ref().unwrap_or(inner);
    let body = env.in_runtime_emit(|env| {
        env.in_staged(|env| {
            let result = typecheck_expr(env, namespace, inner, expected_type)?;
            let result = if let Some(expected_type) = expected_type {
                result.apply_expected_type(env, namespace, expected_type)?
            } else {
                result
            };
            result.standard_ready_coerce(env, inner.token_range())
        })
    })?;

    Ok(TypecheckResult::staged_literal(stage_expression(body)))
}

pub(crate) fn stage_expression(body: THIRExpression) -> THIRStagedExpr {
    let mut staged = THIRStagedExpr::new(Box::new(body));
    let captures = collect_captures(&staged);
    staged.set_captures(captures);
    staged
}

pub fn complete_staged_expr(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    deferred: DeferredStagedExpr,
    value_type: &THIRComptimeValueType,
) -> CXResult<THIRStagedExpr> {
    if deferred.params.len() != value_type.params.len() {
        return env.log_error(
            deferred.body.token_range(),
            &catalogue::ARGUMENT_COUNT,
            (
                "Staged expression".into(),
                value_type.params.len(),
                deferred.params.len(),
                false,
            ),
        );
    }

    env.symbols.push_local_scope();

    let mut params = Vec::with_capacity(deferred.params.len());
    for (name, ty) in deferred.params.into_iter().zip(&value_type.params) {
        let local_id = THIRLocalID::fresh();
        env.symbols.insert_local_value(
            QualifiedName::new_raw(name.clone()),
            THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::Variable {
                    name: name.clone(),
                    local_id,
                },
                _type: ty.clone(),
            },
        );
        params.push(THIRStagedParameter {
            name,
            local_id,
            ty: ty.clone(),
        });
    }

    let rewritten = external_yield_block(&deferred.body, value_type._type.is_void());
    let body_expression = rewritten.as_ref().unwrap_or(&deferred.body);
    let body = env.in_staged(|env| {
        let expected = (!value_type._type.is_void() && !value_type._type.is_unreachable())
            .then_some(&value_type._type);
        let result = typecheck_expr(env, namespace, body_expression, expected)?;
        let result = if let Some(expected) = expected {
            result.apply_expected_type(env, namespace, expected)?
        } else {
            result
        };
        let body = result.standard_ready_coerce(env, deferred.body.token_range())?;
        if let Some(expected) = expected {
            implicit_cast(env, body, expected)
        } else {
            Ok(body)
        }
    });
    env.symbols.pop_local_scope();

    let body = body?;
    let mut staged = THIRStagedExpr::new(Box::new(body));
    staged.add_params(params);
    let captures = collect_captures(&staged);
    staged.set_captures(captures);
    Ok(staged)
}

fn external_yield_block(expression: &HIRExpression, external_yield: bool) -> Option<HIRExpression> {
    if !external_yield {
        return None;
    }
    let HIRExprKind::Block {
        kind: HIRBlockKind::Expression,
        ..
    } = &expression.kind else {
        return None;
    };
    let mut block = expression.clone();
    if let HIRExprKind::Block { kind, .. } = &mut block.kind {
        *kind = HIRBlockKind::Statement;
    }
    Some(block)
}

fn collect_captures(staged: &THIRStagedExpr) -> Vec<THIRLocalID> {
    let mut references = Vec::new();
    let mut bindings = HashSet::new();
    for parameter in staged.params() {
        bindings.insert(parameter.local_id);
    }
    collect_expression_locals(staged.expr(), &mut references, &mut bindings);

    let mut seen = HashSet::new();
    references
        .into_iter()
        .filter(|local_id| !bindings.contains(local_id) && seen.insert(*local_id))
        .collect()
}

fn collect_expression_locals(
    expression: &THIRExpression,
    references: &mut Vec<THIRLocalID>,
    bindings: &mut HashSet<THIRLocalID>,
) {
    match &expression.kind {
        THIRExpressionKind::Variable { local_id, .. }
        | THIRExpressionKind::Move { local_id, .. } => references.push(*local_id),

        THIRExpressionKind::BoolLiteral(_)
        | THIRExpressionKind::IntLiteral(_)
        | THIRExpressionKind::FloatLiteral(_)
        | THIRExpressionKind::StringLiteral { .. }
        | THIRExpressionKind::Unit
        | THIRExpressionKind::GlobalVariable { .. }
        | THIRExpressionKind::ContractVariable { .. }
        | THIRExpressionKind::FunctionReference { .. }
        | THIRExpressionKind::SizeOf { .. }
        | THIRExpressionKind::AlignOf { .. }
        | THIRExpressionKind::Break
        | THIRExpressionKind::Continue
        | THIRExpressionKind::Goto { .. }
        | THIRExpressionKind::Unreachable => {}

        THIRExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            collect_expression_locals(lhs, references, bindings);
            collect_expression_locals(rhs, references, bindings);
        }
        THIRExpressionKind::UnaryOperation { operand, .. }
        | THIRExpressionKind::AddressOf { operand }
        | THIRExpressionKind::Copy { source: operand }
        | THIRExpressionKind::Typechange(operand)
        | THIRExpressionKind::MemberAccess { base: operand, .. }
        | THIRExpressionKind::TaggedUnionTag { value: operand, .. }
        | THIRExpressionKind::TaggedUnionGet { value: operand, .. }
        | THIRExpressionKind::TaggedUnionInitializer { value: operand, .. }
        | THIRExpressionKind::Leak {
            expression: operand,
        }
        | THIRExpressionKind::Unsafe {
            expression: operand,
        }
        | THIRExpressionKind::VaEnd { list: operand }
        | THIRExpressionKind::VaArg { list: operand, .. } => {
            collect_expression_locals(operand, references, bindings);
        }
        THIRExpressionKind::CreateLocalVariable {
            local_id,
            initial_value,
            ..
        } => {
            bindings.insert(*local_id);
            if let Some(initial_value) = initial_value {
                collect_expression_locals(initial_value, references, bindings);
            }
        }
        THIRExpressionKind::AdoptRegion {
            local_id,
            initial_value,
            ..
        } => {
            bindings.insert(*local_id);
            collect_expression_locals(initial_value, references, bindings);
        }
        THIRExpressionKind::Assign { target, value } => {
            collect_expression_locals(target, references, bindings);
            collect_expression_locals(value, references, bindings);
        }
        THIRExpressionKind::ArrayAccess { array, index, .. } => {
            collect_expression_locals(array, references, bindings);
            collect_expression_locals(index, references, bindings);
        }
        THIRExpressionKind::PatternIs { lhs, pattern } => {
            collect_expression_locals(lhs, references, bindings);
            collect_pattern_bindings(pattern, bindings);
        }
        THIRExpressionKind::Unpack {
            value,
            bindings: unpack_bindings,
        } => {
            collect_expression_locals(value, references, bindings);
            bindings.extend(
                unpack_bindings
                    .iter()
                    .map(|unpack_binding| unpack_binding.binding_local_id),
            );
        }
        THIRExpressionKind::TaggedUnionSet {
            target,
            inner_value,
            ..
        } => {
            collect_expression_locals(target, references, bindings);
            collect_expression_locals(inner_value, references, bindings);
        }
        THIRExpressionKind::ArrayInitializer { elements, .. } => {
            for element in elements {
                collect_expression_locals(element, references, bindings);
            }
        }
        THIRExpressionKind::StructInitializer {
            initializations, ..
        } => {
            for initialization in initializations {
                collect_expression_locals(&initialization.value, references, bindings);
            }
        }
        THIRExpressionKind::Label { statement, .. }
        | THIRExpressionKind::Defer {
            expression: statement,
        } => collect_expression_locals(statement, references, bindings),
        THIRExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_expression_locals(condition, references, bindings);
            collect_expression_locals(then_branch, references, bindings);
            if let Some(else_branch) = else_branch {
                collect_expression_locals(else_branch, references, bindings);
            }
        }
        THIRExpressionKind::While {
            condition, body, ..
        } => {
            collect_expression_locals(condition, references, bindings);
            collect_expression_locals(body, references, bindings);
        }
        THIRExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            collect_expression_locals(init, references, bindings);
            collect_expression_locals(condition, references, bindings);
            collect_expression_locals(increment, references, bindings);
            collect_expression_locals(body, references, bindings);
        }
        THIRExpressionKind::CSwitch {
            condition,
            cases,
            default,
        } => {
            collect_expression_locals(condition, references, bindings);
            for (case, body) in cases {
                collect_expression_locals(case, references, bindings);
                collect_expression_locals(body, references, bindings);
            }
            if let Some(default) = default {
                collect_expression_locals(default, references, bindings);
            }
        }
        THIRExpressionKind::Match {
            condition,
            subject,
            arms,
        } => {
            collect_expression_locals(condition, references, bindings);
            bindings.insert(*subject);
            for (pattern, body) in arms {
                collect_pattern_bindings(pattern, bindings);
                collect_expression_locals(body, references, bindings);
            }
        }
        THIRExpressionKind::Return {
            postcondition,
            value,
        } => {
            if let Some(value) = value {
                collect_expression_locals(value, references, bindings);
            }
            if let Some(postcondition) = postcondition {
                collect_postcondition(postcondition, references, bindings);
            }
        }
        THIRExpressionKind::Yield { value } => {
            if let Some(value) = value {
                collect_expression_locals(value, references, bindings);
            }
        }
        THIRExpressionKind::Assert { condition, .. } => {
            collect_expression_locals(condition, references, bindings);
        }
        THIRExpressionKind::Block { statements, .. } => {
            for statement in statements {
                collect_expression_locals(statement, references, bindings);
            }
        }
        THIRExpressionKind::CallFunction {
            function,
            arguments,
            contract,
        } => {
            collect_expression_locals(function, references, bindings);
            for argument in arguments {
                collect_expression_locals(argument, references, bindings);
            }
            collect_contract(contract, references, bindings);
        }
        THIRExpressionKind::VaStart { list, last } => {
            collect_expression_locals(list, references, bindings);
            collect_expression_locals(last, references, bindings);
        }
        THIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => {
            if let cx_thir::thir::expression::THIRCoercion::ReferenceBounding(local_ids) =
                conversion
            {
                references.extend(local_ids.iter().copied());
            }
            collect_expression_locals(operand, references, bindings);
        }
        THIRExpressionKind::StagedExpression(nested) => {
            references.extend(nested.captures().iter().copied());
            bindings.extend(nested.params().iter().map(|parameter| parameter.local_id));
            collect_expression_locals(nested.expr(), references, bindings);
        }
        THIRExpressionKind::Materialize { expr, with_params } => {
            collect_expression_locals(expr, references, bindings);
            for parameter in with_params {
                collect_expression_locals(parameter, references, bindings);
            }
        }
    }
}

fn collect_pattern_bindings(pattern: &THIRPattern, bindings: &mut HashSet<THIRLocalID>) {
    match pattern {
        THIRPattern::Binding { local_id, .. } => {
            bindings.insert(*local_id);
        }
        THIRPattern::TaggedUnionVariant {
            inner_local_id: Some(local_id),
            ..
        } => {
            bindings.insert(*local_id);
        }
        THIRPattern::Integer(_)
        | THIRPattern::Float(_, _)
        | THIRPattern::TaggedUnionVariant {
            inner_local_id: None,
            ..
        } => {}
    }
}

fn collect_postcondition(
    postcondition: &THIRPostcondition,
    references: &mut Vec<THIRLocalID>,
    bindings: &mut HashSet<THIRLocalID>,
) {
    collect_expression_locals(&postcondition.condition, references, bindings);
}

fn collect_contract(
    contract: &THIRFnContract,
    references: &mut Vec<THIRLocalID>,
    bindings: &mut HashSet<THIRLocalID>,
) {
    if let Some(precondition) = &contract.precondition {
        collect_expression_locals(precondition, references, bindings);
    }
    if let Some(postcondition) = &contract.postcondition {
        collect_postcondition(postcondition, references, bindings);
    }
}

pub fn into_expression(staged: THIRStagedExpr) -> THIRExpression {
    THIRExpression {
        _type: staged.expr()._type.clone(),
        token_range: staged.expr().token_range.clone(),
        kind: THIRExpressionKind::StagedExpression(staged),
    }
}
