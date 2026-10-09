use cx_hmir::{
    HMIRAggregateKind, HMIRBlockKind, HMIRControlOp, HMIRExprID, HMIRExprKind, HMIRLocalID,
    HMIRPattern,
};
use cx_log::catalogue::{mir, typecheck};
use cx_mir::{
    MIRBasicBlockID, MIRBlockTarget, MIRConstant, MIRInstructionKind, MIRScopeID, MIRValue,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    function::{
        Control, ControlKind, Expect, FunctionLowering, LowerResult, Merge, MergeParam, Operand,
        PatternBinding, Stop, SwitchLabels,
        aggregate::{lower_bind_pattern, lower_pattern_subject, lower_sum_index},
        coerce::{lower_convert, lower_truthy},
        expr::{lower_expr, lower_inferred_type},
        inspect, lower_cleanup_to, lower_eval, lower_return, lower_type_hint,
        operand::{lower_copy, lower_read, lower_spill, lower_value},
    },
    ty::HMIRTypeID,
    value::{arithmetic_type, normalize_int, promote_integer_type},
};

pub(crate) fn lower_block(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    kind: HMIRBlockKind,
    statements: &[HMIRExprID],
    tail: Option<HMIRExprID>,
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    match kind {
        HMIRBlockKind::Sequence => lower_sequence(cx, frame, statements, tail, expect),
        HMIRBlockKind::Scope => lower_scope(cx, span, |this| {
            let value = lower_sequence(this, frame, statements, tail, expect)?;
            settle_scope_value(this, value, expect, span)
        }),
        HMIRBlockKind::Yield => {
            let merge = lower_open_merge(cx, "block.yield", expect, span)?;
            cx.push_control(ControlKind::Yield { merge });
            let expect = cx.merge_expect(merge);
            let result = lower_scope(cx, span, |this| {
                let value = lower_sequence(this, frame, statements, tail, expect)?;
                settle_scope_value(this, value, expect, span)
            });
            cx.controls.pop();
            match result {
                Ok(value) => lower_merge_edge(cx, merge, tail.map(|_| value), span)?,
                Err(Stop::Diverged) => {}
                Err(error) => return Err(error),
            }
            lower_close_merge(cx, merge, span)
        }
    }
}

fn lower_sequence(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    statements: &[HMIRExprID],
    tail: Option<HMIRExprID>,
    expect: Expect,
) -> LowerResult<Operand> {
    let mut live = true;
    let mut checked = false;
    for (index, statement) in statements
        .iter()
        .chain(tail.iter().filter(|_| expect == Expect::Discard))
        .enumerate()
    {
        // A case label is reached from its switch, whatever came before it
        let is_case = cx.is_case(frame, *statement);
        if is_case && !live {
            live = true;
            checked = false;
        }
        if live || cx.unevaluated {
            if cx.terminated() && !is_case {
                let block = cx.new_block("check.unreachable");
                cx.set_block(block);
            }
            match lower_expr(cx, frame, *statement, Expect::Discard) {
                Ok(_) => {}
                Err(Stop::Diverged) => live = false,
                Err(error) => return Err(error),
            }
        } else {
            if !checked {
                let dead = &statements[index.min(statements.len())..];
                let resumed = dead
                    .iter()
                    .position(|statement| cx.is_case(frame, *statement));
                inspect::check(
                    cx,
                    frame,
                    inspect::Check::Sequence {
                        statements: &dead[..resumed.unwrap_or(dead.len())],
                        tail: tail.filter(|_| resumed.is_none()),
                        expect,
                    },
                )?;
                checked = true;
            }
            live = lower_dead(cx, frame, *statement)?;
        }
    }
    if !live {
        if cx.unevaluated
            && expect != Expect::Discard
            && let Some(tail) = tail
        {
            lower_expr(cx, frame, tail, expect).or_else(|stop| match stop {
                Stop::Diverged => Ok(Operand::unit(cx.program.types_mut())),
                error => Err(error),
            })?;
        }
        return Err(Stop::Diverged);
    }
    match tail {
        Some(tail) if expect != Expect::Discard => lower_expr(cx, frame, tail, expect),
        _ => Ok(Operand::unit(cx.program.types_mut())),
    }
}

// Lowers the parts of unreachable code that a goto can still enter; true if control
// falls out of it
fn lower_dead(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    id: HMIRExprID,
) -> LowerResult<bool> {
    match cx.kind(frame, id) {
        HMIRExprKind::Label { name, body } => {
            let Some(target) = cx.labels.get(name.as_str()).copied() else {
                return Ok(false);
            };
            cx.set_block(target);
            match lower_expr(cx, frame, body, Expect::Discard) {
                Ok(_) => Ok(!cx.terminated()),
                Err(Stop::Diverged) => Ok(false),
                Err(error) => Err(error),
            }
        }
        HMIRExprKind::Case { value, body } => {
            let span = cx.span(frame, id);
            lower_case_label(cx, frame, value, &span)?;
            match lower_expr(cx, frame, body, Expect::Discard) {
                Ok(_) => Ok(!cx.terminated()),
                Err(Stop::Diverged) => Ok(false),
                Err(error) => Err(error),
            }
        }
        HMIRExprKind::Block {
            kind, statements, ..
        } => {
            let span = cx.span(frame, id);
            let scoped = kind == HMIRBlockKind::Scope;
            if scoped {
                cx.push_scope(&span);
            }
            let mut live = false;
            for statement in statements {
                if live {
                    match lower_expr(cx, frame, statement, Expect::Discard) {
                        Ok(_) => {}
                        Err(Stop::Diverged) => live = false,
                        Err(error) => return Err(error),
                    }
                } else {
                    live = lower_dead(cx, frame, statement)?;
                }
            }
            if scoped {
                cx.pop_scope(&span)?;
            }
            Ok(live)
        }
        HMIRExprKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            let span = cx.span(frame, id);
            let mut merge = None;
            for branch in std::iter::once(then_branch).chain(else_branch) {
                if lower_dead(cx, frame, branch)? {
                    let block = *merge.get_or_insert_with(|| cx.new_block("if.merge"));
                    cx.jump(block, Vec::new(), &span);
                }
            }
            if let Some(block) = merge {
                cx.set_block(block);
            }
            Ok(merge.is_some())
        }
        _ => Ok(false),
    }
}

// Lowers an untaken branch that a goto may still enter; true if control continues
fn lower_skip(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    id: HMIRExprID,
    live: bool,
    span: &TokenRange,
) -> LowerResult<bool> {
    let resume = cx.current;
    inspect::check(
        cx,
        frame,
        inspect::Check::Sequence {
            statements: &[id],
            tail: None,
            expect: Expect::Discard,
        },
    )?;
    let falls = lower_dead(cx, frame, id)?;
    if cx.current == resume {
        return Ok(live);
    }
    match (live, falls) {
        (false, falls) => Ok(falls),
        (true, false) => {
            cx.set_block(resume);
            Ok(true)
        }
        (true, true) => {
            let join = cx.new_block("if.join");
            cx.jump(join, Vec::new(), span);
            cx.set_block(resume);
            cx.jump(join, Vec::new(), span);
            cx.set_block(join);
            Ok(true)
        }
    }
}

pub(crate) fn lower_if(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    condition: HMIRExprID,
    then_branch: HMIRExprID,
    else_branch: Option<HMIRExprID>,
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let expect = match (else_branch, expect) {
        (None, _) => Expect::Discard,
        (Some(else_branch), Expect::Any) => {
            Expect::of(lower_branch_type(cx, frame, then_branch, else_branch))
        }
        (_, expect) => expect,
    };
    let mark = cx.pattern_bindings.len();
    let condition = lower_expr(cx, frame, condition, Expect::Any)?;
    let condition = lower_truthy(cx, condition, span)?;
    let bindings = cx.pattern_bindings.split_off(mark);

    if !cx.unevaluated
        && bindings.is_empty()
        && let Some(taken) = condition.as_static().and_then(|value| value.is_truthy())
    {
        let (chosen, skipped) = match taken {
            true => (Some(then_branch), else_branch),
            false => (else_branch, Some(then_branch)),
        };
        let result = match chosen {
            Some(branch) => lower_expr(cx, frame, branch, expect),
            None => Ok(Operand::unit(cx.program.types_mut())),
        };
        let Some(skipped) = skipped.filter(|_| expect == Expect::Discard) else {
            return result;
        };
        let live = match result {
            Ok(_) => true,
            Err(Stop::Diverged) => false,
            Err(error) => return Err(error),
        };
        return match lower_skip(cx, frame, skipped, live, span)? {
            true => Ok(Operand::unit(cx.program.types_mut())),
            false => Err(Stop::Diverged),
        };
    }

    let condition = lower_value(cx, condition, span)?;
    let then_block = cx.new_block("if.then");
    let else_block = else_branch.map(|_| cx.new_block("if.else"));
    let merge = lower_open_merge(cx, "if.merge", expect, span)?;
    let merge_block = cx.merges[merge].block;
    if else_block.is_none() {
        cx.merges[merge].reached = true;
    }
    cx.emit(
        MIRInstructionKind::Branch {
            cond: condition,
            true_target: MIRBlockTarget::new(then_block),
            false_target: MIRBlockTarget::new(else_block.unwrap_or(merge_block)),
        },
        span,
    );

    cx.set_block(then_block);
    lower_branch(cx, frame, then_branch, merge, bindings, span)?;
    if let (Some(else_branch), Some(else_block)) = (else_branch, else_block) {
        cx.set_block(else_block);
        lower_branch(cx, frame, else_branch, merge, Vec::new(), span)?;
    }
    lower_close_merge(cx, merge, span)
}

// The type both arms of a valued conditional are converted to, when it can be predicted
fn lower_branch_type(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
) -> Option<HMIRTypeID> {
    let lhs = lower_type_hint(cx, frame, lhs)?;
    let rhs = lower_type_hint(cx, frame, rhs)?;
    let types = cx.program.types_mut();
    let (lhs, rhs) = (types.decayed(lhs), types.decayed(rhs));
    if lhs == rhs || types.is_unreachable(rhs) {
        return Some(lhs);
    }
    if types.is_unreachable(lhs) {
        return Some(rhs);
    }
    if let Some(common) = arithmetic_type(types, lhs, rhs) {
        return Some(common);
    }
    match (types.pointer_inner(lhs), types.pointer_inner(rhs)) {
        // A null pointer constant takes the type of the other arm
        (None, Some(_)) if types.int_info(lhs).is_some() => Some(rhs),
        (Some(then_inner), Some(else_inner)) => {
            let constant = types.is_const(then_inner) || types.is_const(else_inner);
            let unqualified = types.unqualified(else_inner);
            let pointee = match types.is_void(unqualified) {
                true => else_inner,
                false => then_inner,
            };
            let pointee = match constant {
                true => types.const_of(pointee),
                false => pointee,
            };
            Some(types.pointer_to(pointee))
        }
        _ => Some(lhs),
    }
}

// Lowers one arm into 'merge' within its own scope
fn lower_branch(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    body: HMIRExprID,
    merge: usize,
    bindings: Vec<PatternBinding>,
    span: &TokenRange,
) -> LowerResult<()> {
    let expect = cx.merge_expect(merge);
    let result = lower_scope(cx, span, |this| {
        for binding in bindings {
            lower_bind_pattern(this, binding, span)?;
        }
        let value = lower_expr(this, frame, body, expect)?;
        settle_scope_value(this, value, expect, span)
    });
    match result {
        Ok(value) => lower_merge_edge(cx, merge, Some(value), span),
        Err(Stop::Diverged) => Ok(()),
        Err(error) => Err(error),
    }
}

pub(crate) fn lower_while(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    condition: HMIRExprID,
    body: HMIRExprID,
    pre_eval: bool,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let condition_block = cx.new_block("while.condition");
    let body_block = cx.new_block("while.body");
    let exit = cx.new_block("while.exit");
    cx.jump(
        if pre_eval {
            condition_block
        } else {
            body_block
        },
        Vec::new(),
        span,
    );

    cx.set_block(condition_block);
    let mark = cx.pattern_bindings.len();
    let bindings = match lower_condition(cx, frame, condition, span) {
        Ok(condition) => {
            let bindings = cx.pattern_bindings.split_off(mark);
            cx.emit(
                MIRInstructionKind::Branch {
                    cond: condition,
                    true_target: MIRBlockTarget::new(body_block),
                    false_target: MIRBlockTarget::new(exit),
                },
                span,
            );
            bindings
        }
        Err(Stop::Diverged) => {
            lower_seal(cx, &[body_block, exit], span);
            return Err(Stop::Diverged);
        }
        Err(error) => return Err(error),
    };

    cx.set_block(body_block);
    lower_loop_body(cx, frame, body, bindings, exit, condition_block, span)?;
    cx.jump(condition_block, Vec::new(), span);
    cx.set_block(exit);
    Ok(Operand::unit(cx.program.types_mut()))
}

pub(crate) fn lower_for(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    init: HMIRExprID,
    condition: HMIRExprID,
    increment: HMIRExprID,
    body: HMIRExprID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    lower_scope(cx, span, |this| {
        lower_expr(this, frame, init, Expect::Discard)?;
        let condition_block = this.new_block("for.condition");
        let body_block = this.new_block("for.body");
        let increment_block = this.new_block("for.increment");
        let exit = this.new_block("for.exit");
        this.jump(condition_block, Vec::new(), span);

        this.set_block(condition_block);
        let mark = this.pattern_bindings.len();
        match lower_condition(this, frame, condition, span) {
            Ok(condition) => this.emit(
                MIRInstructionKind::Branch {
                    cond: condition,
                    true_target: MIRBlockTarget::new(body_block),
                    false_target: MIRBlockTarget::new(exit),
                },
                span,
            ),
            Err(Stop::Diverged) => {
                lower_seal(this, &[body_block, increment_block, exit], span);
                return Err(Stop::Diverged);
            }
            Err(error) => return Err(error),
        }
        let bindings = this.pattern_bindings.split_off(mark);

        this.set_block(body_block);
        lower_loop_body(this, frame, body, bindings, exit, increment_block, span)?;
        this.jump(increment_block, Vec::new(), span);

        this.set_block(increment_block);
        match lower_expr(this, frame, increment, Expect::Discard) {
            Ok(_) | Err(Stop::Diverged) => {}
            Err(error) => return Err(error),
        }
        this.jump(condition_block, Vec::new(), span);
        this.set_block(exit);
        Ok(Operand::unit(this.program.types_mut()))
    })
}

fn lower_condition(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    condition: HMIRExprID,
    span: &TokenRange,
) -> LowerResult<MIRValue> {
    let condition = lower_expr(cx, frame, condition, Expect::Any)?;
    let condition = lower_truthy(cx, condition, span)?;
    lower_value(cx, condition, span)
}

fn lower_loop_body(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    body: HMIRExprID,
    bindings: Vec<PatternBinding>,
    exit: MIRBasicBlockID,
    next: MIRBasicBlockID,
    span: &TokenRange,
) -> LowerResult<()> {
    cx.push_control(ControlKind::Loop { exit, next });
    let result = lower_scope(cx, span, |this| {
        for binding in bindings {
            lower_bind_pattern(this, binding, span)?;
        }
        lower_expr(this, frame, body, Expect::Discard)
    });
    cx.controls.pop();
    match result {
        Ok(_) | Err(Stop::Diverged) => Ok(()),
        Err(error) => Err(error),
    }
}

// Terminates blocks that control can no longer reach
fn lower_seal(cx: &mut FunctionLowering<'_, '_>, blocks: &[MIRBasicBlockID], span: &TokenRange) {
    for block in blocks {
        cx.set_block(*block);
        cx.emit(MIRInstructionKind::Unreachable, span);
    }
}

// The body is lowered once, in order, so that its labels fall through to one another. Only those
// labels lead into it, which is why the branch can wait until they have all been seen.
pub(crate) fn lower_switch(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    condition: HMIRExprID,
    body: HMIRExprID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let condition_span = cx.span(frame, condition);
    let condition = lower_expr(cx, frame, condition, Expect::Any)?;
    let found = lower_inferred_type(cx, condition.ty());
    let condition_ty = promote_integer_type(cx.program.types_mut(), found);
    if cx.program.types().int_info(condition_ty).is_none() {
        let types = cx.program.types();
        return cx.error(
            &condition_span,
            &typecheck::TYPE_MISMATCH,
            (
                "switch condition".into(),
                "integer type".into(),
                format!("'{}'", types.display(found)),
            ),
        );
    }
    let condition = lower_convert(cx, condition, condition_ty, &condition_span)?;
    let signed = cx.program.types().is_signed(condition_ty);
    let value = lower_value(cx, condition, &condition_span)?;
    let dispatch = cx.current;
    let exit = cx.new_block("switch.exit");

    let entry = cx.new_block("switch.body");
    cx.set_block(entry);
    cx.push_control(ControlKind::Switch { exit });
    cx.switches.push(SwitchLabels {
        condition: condition_ty,
        cases: Vec::new(),
        default: None,
    });
    let result = lower_expr(cx, frame, body, Expect::Discard);
    let labels = cx.switches.pop().expect("switch labels are balanced");
    cx.controls.pop();
    match result {
        Ok(_) => cx.jump(exit, Vec::new(), span),
        Err(Stop::Diverged) => {}
        Err(error) => return Err(error),
    }

    cx.set_block(dispatch);
    cx.emit(
        MIRInstructionKind::CaseBranch {
            value,
            signed,
            cases: labels.cases,
            default: Some(MIRBlockTarget::new(labels.default.unwrap_or(exit))),
        },
        &condition_span,
    );
    cx.set_block(exit);
    Ok(Operand::unit(cx.program.types_mut()))
}

pub(crate) fn lower_case(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    value: Option<HMIRExprID>,
    body: HMIRExprID,
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    lower_case_label(cx, frame, value, span)?;
    lower_expr(cx, frame, body, expect)
}

// Starts the block of a 'case' or 'default' label and records it with the enclosing switch
fn lower_case_label(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    value: Option<HMIRExprID>,
    span: &TokenRange,
) -> LowerResult<()> {
    let Some(condition) = cx.switches.last().map(|labels| labels.condition) else {
        return cx.error(
            span,
            &typecheck::REQUIRED_CONTEXT,
            ("case label".into(), "a switch".into()),
        );
    };
    let value = match value {
        Some(case) => {
            let case_span = cx.span(frame, case);
            let Some(value) = lower_eval(cx, frame, case, Expect::Type(condition))?.as_int()
            else {
                return cx.error(
                    &case_span,
                    &mir::EXPECTED_CONSTANT,
                    ("switch case".into(), "integer".into()),
                );
            };
            let value = normalize_int(value, condition, cx.program.types());
            let labels = cx.switches.last().expect("case label is in a switch");
            if labels.cases.iter().any(|(seen, _)| *seen == value) {
                return cx.error(
                    &case_span,
                    &typecheck::DUPLICATE_ITEM,
                    ("case".into(), "switch".into()),
                );
            }
            Some(value)
        }
        None => None,
    };

    let block = cx.new_block(match value {
        Some(_) => "switch.case",
        None => "switch.default",
    });
    let labels = cx.switches.last_mut().expect("case label is in a switch");
    match value {
        Some(value) => labels.cases.push((value, MIRBlockTarget::new(block))),
        None => labels.default = Some(block),
    }
    cx.jump(block, Vec::new(), span);
    cx.set_block(block);
    Ok(())
}

pub(crate) fn lower_match(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    scrutinee: HMIRExprID,
    subject: HMIRLocalID,
    arms: &[(HMIRPattern, HMIRExprID)],
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let value = lower_expr(cx, frame, scrutinee, Expect::Any)?;
    let value = lower_pattern_subject(cx, value, span)?;
    let owned = !value.is_lvalue();
    let value = lower_spill(cx, value, span)?;
    cx.bind(frame, subject, value.clone());

    let variants = cx
        .program
        .types()
        .nominal_of(value.ty())
        .is_some_and(|nominal| nominal.kind() == HMIRAggregateKind::TaggedUnion);
    let (dispatch, signed) = if variants {
        (lower_sum_index(cx, &value, span)?, false)
    } else {
        let signed = cx.program.types().is_signed(value.ty());
        (lower_copy(cx, &value, span)?, signed)
    };

    let merge = lower_open_merge(cx, "match.exit", expect, span)?;
    let blocks = arms
        .iter()
        .map(|_| cx.new_block("match.arm"))
        .collect::<Vec<_>>();
    let binding_block = arms.iter().zip(&blocks).find_map(|((pattern, _), block)| {
        matches!(pattern, HMIRPattern::Binding(_)).then_some(*block)
    });
    let default = binding_block.unwrap_or_else(|| cx.new_block("match.unreachable"));
    let mut evaluated = crate::function::lower_eval_frame(cx, frame);
    let planned = crate::pattern::match_cases(
        cx.program,
        &mut evaluated,
        value.ty(),
        arms.iter().map(|(pattern, _)| pattern),
        span,
    )?;
    let cases = planned
        .into_iter()
        .zip(&blocks)
        .filter_map(|(case, block)| case.map(|case| (case, MIRBlockTarget::new(*block))))
        .collect();
    cx.emit(
        MIRInstructionKind::CaseBranch {
            value: dispatch,
            signed,
            cases,
            default: Some(MIRBlockTarget::new(default)),
        },
        span,
    );

    for ((pattern, body), block) in arms.iter().zip(blocks) {
        cx.set_block(block);
        cx.push_control(ControlKind::Yield { merge });
        let binding = PatternBinding {
            frame,
            subject: value.clone(),
            owned,
            pattern: pattern.clone(),
        };
        let result = lower_branch(cx, frame, *body, merge, vec![binding], span);
        cx.controls.pop();
        result?;
    }
    if binding_block.is_none() {
        cx.set_block(default);
        cx.emit(MIRInstructionKind::Unreachable, span);
    }
    lower_close_merge(cx, merge, span)
}

pub(crate) fn lower_label(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    name: CXIdent,
    body: HMIRExprID,
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let target = cx.label_block(&name);
    cx.jump(target, Vec::new(), span);
    cx.set_block(target);
    lower_expr(cx, frame, body, expect)
}

pub(super) fn lower_control(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    op: HMIRControlOp,
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    match op {
        HMIRControlOp::Return(value) => {
            if value.is_some() && cx.check_return && cx.program.types().is_void(cx.ret) {
                return cx.error(span, &typecheck::VOID_RETURN_VALUE, ());
            }
            let value = match value {
                Some(value) => {
                    let expect = if cx.check_return {
                        Expect::Type(cx.ret)
                    } else {
                        Expect::Any
                    };
                    let value = lower_expr(cx, frame, value, expect)?;
                    Some(value)
                }
                None => None,
            };
            lower_return(cx, value, span)?;
            Err(Stop::Diverged)
        }
        HMIRControlOp::Yield(value) => {
            let Some((boundary, merge)) = cx.find_control(|kind| match kind {
                ControlKind::Yield { merge } => Some(merge),
                _ => None,
            }) else {
                return cx.error(
                    span,
                    &typecheck::REQUIRED_CONTEXT,
                    ("yield".into(), "a yielding block".into()),
                );
            };
            let value = match value {
                Some(value) => {
                    let expect = cx.merge_expect(merge);
                    let value = lower_expr(cx, frame, value, expect)?;
                    let value = settle_scope_value(cx, value, expect, span)?;
                    if let Expect::Type(expected) = expect {
                        let found = lower_inferred_type(cx, value.ty());
                        let promoted = promote_integer_type(cx.program.types_mut(), found);
                        let types = cx.program.types_mut();
                        if !types.same_unqualified(found, expected)
                            && !types.same_unqualified(promoted, expected)
                        {
                            let types = cx.program.types();
                            return cx.error(
                                span,
                                &typecheck::TYPE_MISMATCH,
                                (
                                    "yield".into(),
                                    format!("'{}'", types.display(expected)),
                                    format!("'{}'", types.display(found)),
                                ),
                            );
                        }
                    }
                    Some(value)
                }
                None => None,
            };
            lower_cleanup_to(cx, boundary, false, span)?;
            lower_merge_edge(cx, merge, value, span)?;
            Err(Stop::Diverged)
        }
        HMIRControlOp::Break => {
            let Some((boundary, exit)) = cx.find_control(|kind| match kind {
                ControlKind::Loop { exit, .. } | ControlKind::Switch { exit } => Some(exit),
                ControlKind::Yield { .. } => None,
            }) else {
                return cx.error(
                    span,
                    &typecheck::REQUIRED_CONTEXT,
                    ("break".into(), "a loop or switch".into()),
                );
            };
            lower_cleanup_to(cx, boundary, false, span)?;
            cx.jump(exit, Vec::new(), span);
            Err(Stop::Diverged)
        }
        HMIRControlOp::Continue => {
            let Some((boundary, next)) = cx.find_control(|kind| match kind {
                ControlKind::Loop { next, .. } => Some(next),
                _ => None,
            }) else {
                return cx.error(
                    span,
                    &typecheck::REQUIRED_CONTEXT,
                    ("continue".into(), "a loop".into()),
                );
            };
            lower_cleanup_to(cx, boundary, false, span)?;
            cx.jump(next, Vec::new(), span);
            Err(Stop::Diverged)
        }
        HMIRControlOp::Goto(name) => {
            if cx.defer_boundary.is_some() {
                return cx.error(span, &typecheck::DEFER_JUMP, "jump".into());
            }
            let target = cx.label_block(&name);
            cx.jump(target, Vec::new(), span);
            Err(Stop::Diverged)
        }
        HMIRControlOp::IndirectGoto(target) => {
            if cx.defer_boundary.is_some() {
                return cx.error(span, &typecheck::DEFER_JUMP, "jump".into());
            }
            let types = cx.program.types_mut();
            let void = types.void();
            let pointee = types.const_of(void);
            let address_ty = types.pointer_to(pointee);
            let address = lower_expr(cx, frame, target, Expect::Type(address_ty))?;
            let address = lower_convert(cx, address, address_ty, span)?;
            let address = lower_value(cx, address, span)?;
            let targets = cx
                .body
                .address_labels()
                .iter()
                .map(|(_, block)| MIRBlockTarget::new(*block))
                .collect();
            cx.emit(MIRInstructionKind::IndirectJump { address, targets }, span);
            Err(Stop::Diverged)
        }
        HMIRControlOp::LabelAddress { name, .. } => {
            let Some(function) = cx.function else {
                return cx.error(
                    span,
                    &typecheck::REQUIRED_CONTEXT,
                    ("label address".into(), "a function".into()),
                );
            };
            if cx.body.address_label(name.as_str()).is_none() {
                let block = cx.label_block(&name);
                cx.body.add_address_label(name.as_str(), block);
            }
            let types = cx.program.types_mut();
            let void = types.void();
            let ty = types.pointer_to(void);
            let address = MIRConstant::BlockAddress {
                function,
                label: name.to_string(),
            };
            Ok(Operand::value(MIRValue::Constant(address), ty))
        }
        HMIRControlOp::Defer(body) => {
            inspect::check(cx, frame, inspect::Check::Deferred(body))?;
            cx.scopes
                .last_mut()
                .expect("function has a scope")
                .defers
                .push((frame, body));
            Ok(Operand::unit(cx.program.types_mut()))
        }
        HMIRControlOp::Unsafe(body) => {
            cx.unsafe_depth += 1;
            let result = lower_expr(cx, frame, body, expect);
            cx.unsafe_depth -= 1;
            result
        }
        HMIRControlOp::Unreachable => {
            cx.emit(MIRInstructionKind::Unreachable, span);
            Err(Stop::Diverged)
        }
    }
}

fn lower_open_merge(
    cx: &mut FunctionLowering<'_, '_>,
    name: &str,
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<usize> {
    let block = cx.new_block(name);
    let param = match expect {
        Expect::Discard => MergeParam::Valueless,
        Expect::Any => MergeParam::Undecided,
        Expect::Type(ty) => lower_merge_param(cx, block, ty, span)?,
    };
    cx.merges.push(Merge {
        block,
        param,
        reached: false,
    });
    Ok(cx.merges.len() - 1)
}

fn lower_merge_param(
    cx: &mut FunctionLowering<'_, '_>,
    block: MIRBasicBlockID,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> LowerResult<MergeParam> {
    let types = cx.program.types();
    if types.is_void(ty) || types.is_unreachable(ty) {
        return Ok(MergeParam::Valueless);
    }
    let mir = cx.mir(ty, span)?;
    Ok(MergeParam::Value(
        cx.body.add_block_param(block, mir, None),
        ty,
    ))
}

// Jumps from the current block into 'merge', passing 'value' when the merge takes one
fn lower_merge_edge(
    cx: &mut FunctionLowering<'_, '_>,
    merge: usize,
    value: Option<Operand>,
    span: &TokenRange,
) -> LowerResult<()> {
    if cx.terminated() {
        return Ok(());
    }
    let block = cx.merges[merge].block;
    if let (MergeParam::Undecided, Some(value)) = (cx.merges[merge].param, &value) {
        let ty = lower_inferred_type(cx, value.ty());
        cx.merges[merge].param = lower_merge_param(cx, block, ty, span)?;
    }
    match (cx.merges[merge].param, value) {
        (MergeParam::Value(_, ty), Some(value)) => {
            let value = lower_convert(cx, value, ty, span)?;
            let value = lower_value(cx, value, span)?;
            cx.jump(block, vec![value], span);
        }
        (MergeParam::Value(_, ty), None) => {
            return cx.error(
                span,
                &typecheck::TYPE_MISMATCH,
                (
                    "expression".into(),
                    format!("'{}'", cx.program.types().display(ty)),
                    "no value".into(),
                ),
            );
        }
        (MergeParam::Undecided, None) => {
            cx.merges[merge].param = MergeParam::Valueless;
            cx.jump(block, Vec::new(), span);
        }
        (MergeParam::Valueless | MergeParam::Undecided, _) => {
            cx.jump(block, Vec::new(), span);
        }
    }
    cx.merges[merge].reached = true;
    Ok(())
}

fn lower_close_merge(
    cx: &mut FunctionLowering<'_, '_>,
    merge: usize,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let Merge {
        block,
        param,
        reached,
    } = cx.merges[merge];
    cx.set_block(block);
    if !reached {
        cx.emit(MIRInstructionKind::Unreachable, span);
        return Err(Stop::Diverged);
    }
    Ok(match param {
        MergeParam::Value(register, ty) => Operand::register(register, ty),
        _ => Operand::unit(cx.program.types_mut()),
    })
}

pub(super) fn lower_scope<T>(
    cx: &mut FunctionLowering<'_, '_>,
    span: &TokenRange,
    body: impl FnOnce(&mut FunctionLowering<'_, '_>) -> LowerResult<T>,
) -> LowerResult<T> {
    cx.push_scope(span);
    let result = body(cx);
    let popped = cx.pop_scope(span);
    let value = result?;
    popped?;
    Ok(value)
}

fn settle_scope_value(
    cx: &mut FunctionLowering<'_, '_>,
    value: Operand,
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    if expect == Expect::Discard || !value.is_lvalue() {
        return Ok(value);
    }
    if cx.program.types().is_reference(value.ty()) {
        return Ok(value);
    }
    lower_read(cx, value, span)
}

impl FunctionLowering<'_, '_> {
    fn push_control(&mut self, kind: ControlKind) {
        let boundary = self.current_scope();
        self.controls.push(Control { boundary, kind });
    }

    fn find_control<T>(
        &self,
        select: impl Fn(ControlKind) -> Option<T>,
    ) -> Option<(MIRScopeID, T)> {
        self.controls
            .get(self.defer_boundary.unwrap_or(0)..)?
            .iter()
            .rev()
            .find_map(|control| select(control.kind).map(|found| (control.boundary, found)))
    }

    fn merge_expect(&self, merge: usize) -> Expect {
        match self.merges[merge].param {
            MergeParam::Undecided => Expect::Any,
            MergeParam::Valueless => Expect::Discard,
            MergeParam::Value(_, ty) => Expect::Type(ty),
        }
    }

    fn is_case(&self, frame: usize, id: HMIRExprID) -> bool {
        matches!(
            self.frames[frame].body().expr(id).kind(),
            HMIRExprKind::Case { .. }
        )
    }

    // Blocks whose address a static of the function holds have to exist before the body does
    pub(super) fn declare_address_labels(&mut self, labels: &[CXIdent]) {
        for label in labels {
            let block = self.label_block(label);
            self.body.add_address_label(label.as_str(), block);
        }
    }

    fn label_block(&mut self, name: &CXIdent) -> MIRBasicBlockID {
        if let Some(block) = self.labels.get(name.as_str()) {
            return *block;
        }
        let block = self.new_block(name.as_str());
        self.labels.insert(name.as_str().to_string(), block);
        block
    }
}
