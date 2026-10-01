use cx_hmir::{HMIRBlockKind, HMIRControlOp, HMIRExprID, HMIRExprKind, HMIRLocalID, HMIRPattern};
use cx_mir::{MIRBasicBlockID, MIRBlockTarget, MIRInstructionKind, MIRValue};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    function::{
        Control, ControlKind, Expect, FunctionLowering, Lower, Merge, MergeParam, Operand,
        PatternBinding, Stop,
    },
    ty::{TypeID, TypeKind},
    value::arithmetic_type,
};

impl FunctionLowering<'_, '_> {
    pub(crate) fn block(
        &mut self,
        frame: usize,
        kind: HMIRBlockKind,
        statements: &[HMIRExprID],
        tail: Option<HMIRExprID>,
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        match kind {
            HMIRBlockKind::Sequence => self.sequence(frame, statements, tail, expect),
            HMIRBlockKind::Scope => self.scoped(span, expect, |this| {
                this.sequence(frame, statements, tail, expect)
            }),
            HMIRBlockKind::Yield => {
                let merge = self.open_merge("block.yield", expect, span)?;
                self.push_control(ControlKind::Yield { merge });
                let result = self.scoped(span, Expect::Discard, |this| {
                    let value = this.sequence(frame, statements, tail, this.merge_expect(merge))?;
                    if tail.is_some() {
                        this.yield_to(merge, Some(value), span)?;
                    }
                    Ok(())
                });
                self.controls.pop();
                match result {
                    Ok(()) | Err(Stop::Diverged) => {}
                    Err(error) => return Err(error),
                }
                self.emit(MIRInstructionKind::Unreachable, span);
                self.close_merge(merge, span)
            }
        }
    }

    // Lowers 'body' in a fresh scope; a value it produces is read before the scope ends
    pub(crate) fn scoped<T: ScopedResult>(
        &mut self,
        span: &TokenRange,
        expect: Expect,
        body: impl FnOnce(&mut Self) -> Lower<T>,
    ) -> Lower<T> {
        self.push_scope(span);
        let result = body(self).and_then(|value| value.settle(self, expect, span));
        let popped = self.pop_scope(span);
        let value = result?;
        popped?;
        Ok(value)
    }

    fn sequence(
        &mut self,
        frame: usize,
        statements: &[HMIRExprID],
        tail: Option<HMIRExprID>,
        expect: Expect,
    ) -> Lower<Operand> {
        let mut live = true;
        for statement in statements
            .iter()
            .chain(tail.iter().filter(|_| expect == Expect::Discard))
        {
            if live {
                match self.expr(frame, *statement, Expect::Discard) {
                    Ok(_) => {}
                    Err(Stop::Diverged) => live = false,
                    Err(error) => return Err(error),
                }
            } else {
                live = self.dead(frame, *statement)?;
            }
        }
        if !live {
            return Err(Stop::Diverged);
        }
        match tail {
            Some(tail) if expect != Expect::Discard => self.expr(frame, tail, expect),
            _ => Ok(Operand::unit(self.program.types_mut())),
        }
    }

    // Lowers the parts of unreachable code that a goto can still enter; true if control
    // falls out of it
    fn dead(&mut self, frame: usize, id: HMIRExprID) -> Lower<bool> {
        match self.kind(frame, id) {
            HMIRExprKind::Label { name, body } => {
                let Some(target) = self.labels.get(name.as_str()).copied() else {
                    return Ok(false);
                };
                self.set_block(target);
                match self.expr(frame, body, Expect::Discard) {
                    Ok(_) => Ok(!self.terminated()),
                    Err(Stop::Diverged) => Ok(false),
                    Err(error) => Err(error),
                }
            }
            HMIRExprKind::Block {
                kind, statements, ..
            } => {
                let span = self.span(frame, id);
                let scoped = kind == HMIRBlockKind::Scope;
                if scoped {
                    self.push_scope(&span);
                }
                let mut live = false;
                for statement in statements {
                    if live {
                        match self.expr(frame, statement, Expect::Discard) {
                            Ok(_) => {}
                            Err(Stop::Diverged) => live = false,
                            Err(error) => return Err(error),
                        }
                    } else {
                        live = self.dead(frame, statement)?;
                    }
                }
                if scoped {
                    self.pop_scope(&span)?;
                }
                Ok(live)
            }
            HMIRExprKind::If {
                then_branch,
                else_branch,
                ..
            } => {
                let span = self.span(frame, id);
                let mut merge = None;
                for branch in std::iter::once(then_branch).chain(else_branch) {
                    if self.dead(frame, branch)? {
                        let block = *merge.get_or_insert_with(|| self.new_block("if.merge"));
                        self.jump(block, Vec::new(), &span);
                    }
                }
                if let Some(block) = merge {
                    self.set_block(block);
                }
                Ok(merge.is_some())
            }
            _ => Ok(false),
        }
    }

    // Lowers an untaken branch that a goto may still enter; true if control continues
    fn skip(&mut self, frame: usize, id: HMIRExprID, live: bool, span: &TokenRange) -> Lower<bool> {
        let resume = self.current;
        let falls = self.dead(frame, id)?;
        if self.current == resume {
            return Ok(live);
        }
        match (live, falls) {
            (false, falls) => Ok(falls),
            (true, false) => {
                self.set_block(resume);
                Ok(true)
            }
            (true, true) => {
                let join = self.new_block("if.join");
                self.jump(join, Vec::new(), span);
                self.set_block(resume);
                self.jump(join, Vec::new(), span);
                self.set_block(join);
                Ok(true)
            }
        }
    }

    pub(crate) fn lower_if(
        &mut self,
        frame: usize,
        condition: HMIRExprID,
        then_branch: HMIRExprID,
        else_branch: Option<HMIRExprID>,
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let expect = match (else_branch, expect) {
            (None, _) => Expect::Discard,
            (Some(else_branch), Expect::Any) => {
                Expect::of(self.branch_type(frame, then_branch, else_branch))
            }
            (_, expect) => expect,
        };
        let mark = self.pattern_bindings.len();
        let condition = self.expr(frame, condition, Expect::Any)?;
        let condition = self.truthy(condition, span)?;
        let bindings = self.pattern_bindings.split_off(mark);

        if !self.unevaluated
            && bindings.is_empty()
            && let Some(taken) = condition.as_static().and_then(|value| value.is_truthy())
        {
            let (chosen, skipped) = match taken {
                true => (Some(then_branch), else_branch),
                false => (else_branch, Some(then_branch)),
            };
            let result = match chosen {
                Some(branch) => self.expr(frame, branch, expect),
                None => Ok(Operand::unit(self.program.types_mut())),
            };
            let Some(skipped) = skipped.filter(|_| expect == Expect::Discard) else {
                return result;
            };
            let live = match result {
                Ok(_) => true,
                Err(Stop::Diverged) => false,
                Err(error) => return Err(error),
            };
            return match self.skip(frame, skipped, live, span)? {
                true => Ok(Operand::unit(self.program.types_mut())),
                false => Err(Stop::Diverged),
            };
        }

        let condition = self.value(condition, span)?;
        let then_block = self.new_block("if.then");
        let else_block = else_branch.map(|_| self.new_block("if.else"));
        let merge = self.open_merge("if.merge", expect, span)?;
        let merge_block = self.merges[merge].block;
        if else_block.is_none() {
            self.merges[merge].reached = true;
        }
        self.emit(
            MIRInstructionKind::Branch {
                cond: condition,
                true_target: MIRBlockTarget::new(then_block),
                false_target: MIRBlockTarget::new(else_block.unwrap_or(merge_block)),
            },
            span,
        );

        self.set_block(then_block);
        self.branch(frame, then_branch, merge, bindings, span)?;
        if let (Some(else_branch), Some(else_block)) = (else_branch, else_block) {
            self.set_block(else_block);
            self.branch(frame, else_branch, merge, Vec::new(), span)?;
        }
        self.close_merge(merge, span)
    }

    // The type both arms of a valued conditional agree on, when they can be predicted
    fn branch_type(&mut self, frame: usize, lhs: HMIRExprID, rhs: HMIRExprID) -> Option<TypeID> {
        let lhs = self.type_hint(frame, lhs)?;
        let rhs = self.type_hint(frame, rhs)?;
        if lhs == rhs {
            return Some(lhs);
        }
        arithmetic_type(self.program.types_mut(), lhs, rhs).or(Some(lhs))
    }

    // Lowers one arm into 'merge' within its own scope
    fn branch(
        &mut self,
        frame: usize,
        body: HMIRExprID,
        merge: usize,
        bindings: Vec<PatternBinding>,
        span: &TokenRange,
    ) -> Lower<()> {
        let expect = self.merge_expect(merge);
        let result = self.scoped(span, expect, |this| {
            for binding in bindings {
                this.bind_pattern(binding, span)?;
            }
            this.expr(frame, body, expect)
        });
        match result {
            Ok(value) => self.merge_edge(merge, Some(value), span),
            Err(Stop::Diverged) => Ok(()),
            Err(error) => Err(error),
        }
    }

    pub(crate) fn lower_while(
        &mut self,
        frame: usize,
        condition: HMIRExprID,
        body: HMIRExprID,
        pre_eval: bool,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let condition_block = self.new_block("while.condition");
        let body_block = self.new_block("while.body");
        let exit = self.new_block("while.exit");
        self.jump(
            if pre_eval {
                condition_block
            } else {
                body_block
            },
            Vec::new(),
            span,
        );

        self.set_block(condition_block);
        let mark = self.pattern_bindings.len();
        let bindings = match self.condition(frame, condition, span) {
            Ok(condition) => {
                let bindings = self.pattern_bindings.split_off(mark);
                self.emit(
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
                self.seal(&[body_block, exit], span);
                return Err(Stop::Diverged);
            }
            Err(error) => return Err(error),
        };

        self.set_block(body_block);
        self.loop_body(frame, body, bindings, exit, condition_block, span)?;
        self.jump(condition_block, Vec::new(), span);
        self.set_block(exit);
        Ok(Operand::unit(self.program.types_mut()))
    }

    pub(crate) fn lower_for(
        &mut self,
        frame: usize,
        init: HMIRExprID,
        condition: HMIRExprID,
        increment: HMIRExprID,
        body: HMIRExprID,
        span: &TokenRange,
    ) -> Lower<Operand> {
        self.scoped(span, Expect::Discard, |this| {
            this.expr(frame, init, Expect::Discard)?;
            let condition_block = this.new_block("for.condition");
            let body_block = this.new_block("for.body");
            let increment_block = this.new_block("for.increment");
            let exit = this.new_block("for.exit");
            this.jump(condition_block, Vec::new(), span);

            this.set_block(condition_block);
            let mark = this.pattern_bindings.len();
            match this.condition(frame, condition, span) {
                Ok(condition) => this.emit(
                    MIRInstructionKind::Branch {
                        cond: condition,
                        true_target: MIRBlockTarget::new(body_block),
                        false_target: MIRBlockTarget::new(exit),
                    },
                    span,
                ),
                Err(Stop::Diverged) => {
                    this.seal(&[body_block, increment_block, exit], span);
                    return Err(Stop::Diverged);
                }
                Err(error) => return Err(error),
            }
            let bindings = this.pattern_bindings.split_off(mark);

            this.set_block(body_block);
            this.loop_body(frame, body, bindings, exit, increment_block, span)?;
            this.jump(increment_block, Vec::new(), span);

            this.set_block(increment_block);
            match this.expr(frame, increment, Expect::Discard) {
                Ok(_) | Err(Stop::Diverged) => {}
                Err(error) => return Err(error),
            }
            this.jump(condition_block, Vec::new(), span);
            this.set_block(exit);
            Ok(Operand::unit(this.program.types_mut()))
        })
    }

    fn condition(
        &mut self,
        frame: usize,
        condition: HMIRExprID,
        span: &TokenRange,
    ) -> Lower<MIRValue> {
        let condition = self.expr(frame, condition, Expect::Any)?;
        let condition = self.truthy(condition, span)?;
        self.value(condition, span)
    }

    fn loop_body(
        &mut self,
        frame: usize,
        body: HMIRExprID,
        bindings: Vec<PatternBinding>,
        exit: MIRBasicBlockID,
        next: MIRBasicBlockID,
        span: &TokenRange,
    ) -> Lower<()> {
        self.push_control(ControlKind::Loop { exit, next });
        let result = self.scoped(span, Expect::Discard, |this| {
            for binding in bindings {
                this.bind_pattern(binding, span)?;
            }
            this.expr(frame, body, Expect::Discard)
        });
        self.controls.pop();
        match result {
            Ok(_) | Err(Stop::Diverged) => Ok(()),
            Err(error) => Err(error),
        }
    }

    // Terminates blocks that control can no longer reach
    fn seal(&mut self, blocks: &[MIRBasicBlockID], span: &TokenRange) {
        for block in blocks {
            self.set_block(*block);
            self.emit(MIRInstructionKind::Unreachable, span);
        }
    }

    // Cases fall through in order; the last falls through to the default, if any
    pub(crate) fn lower_switch(
        &mut self,
        frame: usize,
        condition: HMIRExprID,
        cases: &[(HMIRExprID, HMIRExprID)],
        default: Option<HMIRExprID>,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let condition = self.expr(frame, condition, Expect::Any)?;
        let signed = self.program.types().is_signed(condition.ty());
        let condition_ty = condition.ty();
        let value = self.value(condition, span)?;
        let exit = self.new_block("switch.exit");
        let default_block = default.map(|_| self.new_block("switch.default"));

        let mut targets = Vec::with_capacity(cases.len());
        let mut blocks = Vec::with_capacity(cases.len());
        for (case, _) in cases {
            let case_span = self.span(frame, *case);
            let case_value = self.eval(frame, *case, Expect::Type(condition_ty))?;
            let Some(case_value) = case_value.as_int() else {
                return self.error(&case_span, "switch case is not an integer constant");
            };
            let block = self.new_block("switch.case");
            targets.push((case_value, MIRBlockTarget::new(block)));
            blocks.push(block);
        }
        self.emit(
            MIRInstructionKind::CaseBranch {
                value,
                signed,
                cases: targets,
                default: Some(MIRBlockTarget::new(default_block.unwrap_or(exit))),
            },
            span,
        );

        let segments = cases
            .iter()
            .map(|(_, body)| *body)
            .zip(blocks.iter().copied())
            .chain(default.zip(default_block))
            .collect::<Vec<_>>();
        for (index, (body, block)) in segments.iter().enumerate() {
            let next = segments
                .get(index + 1)
                .map(|(_, block)| *block)
                .unwrap_or(exit);
            self.set_block(*block);
            self.push_control(ControlKind::Switch { exit });
            let result = self.scoped(span, Expect::Discard, |this| {
                this.expr(frame, *body, Expect::Discard)
            });
            self.controls.pop();
            match result {
                Ok(_) => self.jump(next, Vec::new(), span),
                Err(Stop::Diverged) => {}
                Err(error) => return Err(error),
            }
        }
        self.set_block(exit);
        Ok(Operand::unit(self.program.types_mut()))
    }

    pub(crate) fn lower_match(
        &mut self,
        frame: usize,
        scrutinee: HMIRExprID,
        subject: HMIRLocalID,
        arms: &[(HMIRPattern, HMIRExprID)],
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let value = self.expr(frame, scrutinee, Expect::Any)?;
        let owned = !value.is_lvalue();
        let value = self.spill(value, span)?;
        self.bind(frame, subject, value.clone());

        let variants = self
            .program
            .types()
            .nominal_of(value.ty())
            .is_some_and(|nominal| nominal.kind() == cx_hmir::HMIRAggregateKind::TaggedUnion);
        let (dispatch, signed) = if variants {
            (self.sum_index(&value, span)?, false)
        } else {
            let signed = self.program.types().is_signed(value.ty());
            (self.copy(&value, span)?, signed)
        };

        let merge = self.open_merge("match.exit", expect, span)?;
        let blocks = arms
            .iter()
            .map(|_| self.new_block("match.arm"))
            .collect::<Vec<_>>();
        let binding_block = arms.iter().zip(&blocks).find_map(|((pattern, _), block)| {
            matches!(pattern, HMIRPattern::Binding(_)).then_some(*block)
        });
        let default = binding_block.unwrap_or_else(|| self.new_block("match.unreachable"));
        let mut cases = Vec::with_capacity(arms.len());
        for ((pattern, _), block) in arms.iter().zip(&blocks) {
            let case = match pattern {
                HMIRPattern::Binding(_) => continue,
                HMIRPattern::Integer(value) if !variants => *value as i128,
                HMIRPattern::Variant { index, .. } if variants => *index as i128,
                HMIRPattern::Float(_) => {
                    return self.error(span, "floating patterns cannot be matched by cases");
                }
                _ => return self.error(span, "pattern does not fit the matched value"),
            };
            cases.push((case, MIRBlockTarget::new(*block)));
        }
        self.emit(
            MIRInstructionKind::CaseBranch {
                value: dispatch,
                signed,
                cases,
                default: Some(MIRBlockTarget::new(default)),
            },
            span,
        );

        for ((pattern, body), block) in arms.iter().zip(blocks) {
            self.set_block(block);
            self.push_control(ControlKind::Yield { merge });
            let binding = PatternBinding {
                frame,
                subject: value.clone(),
                owned,
                pattern: pattern.clone(),
            };
            let result = self.branch(frame, *body, merge, vec![binding], span);
            self.controls.pop();
            result?;
        }
        if binding_block.is_none() {
            self.set_block(default);
            self.emit(MIRInstructionKind::Unreachable, span);
        }
        self.close_merge(merge, span)
    }

    pub(crate) fn lower_label(
        &mut self,
        frame: usize,
        name: CXIdent,
        body: HMIRExprID,
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let target = self.label(&name);
        self.jump(target, Vec::new(), span);
        self.set_block(target);
        self.expr(frame, body, expect)
    }

    fn label(&mut self, name: &CXIdent) -> MIRBasicBlockID {
        if let Some(block) = self.labels.get(name.as_str()) {
            return *block;
        }
        let block = self.new_block(name.as_str());
        self.labels.insert(name.as_str().to_string(), block);
        block
    }

    pub(crate) fn control(
        &mut self,
        frame: usize,
        op: HMIRControlOp,
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        match op {
            HMIRControlOp::Return(value) => {
                let value = match value {
                    Some(value) => {
                        let value = self.expr(frame, value, Expect::Type(self.ret))?;
                        if self.program.types().is_void(self.ret) {
                            None
                        } else {
                            Some(value)
                        }
                    }
                    None => None,
                };
                self.emit_return(value, span)?;
                Err(Stop::Diverged)
            }
            HMIRControlOp::Yield(value) => {
                let Some((boundary, merge)) = self.find_control(|kind| match kind {
                    ControlKind::Yield { merge } => Some(merge),
                    _ => None,
                }) else {
                    return self.error(span, "yield outside of a yielding block");
                };
                let value = match value {
                    Some(value) => {
                        let expect = self.merge_expect(merge);
                        let value = self.expr(frame, value, expect)?;
                        Some(value.settle(self, expect, span)?)
                    }
                    None => None,
                };
                self.cleanup_to(boundary, false, span)?;
                self.yield_to(merge, value, span)?;
                Err(Stop::Diverged)
            }
            HMIRControlOp::Break => {
                let Some((boundary, exit)) = self.find_control(|kind| match kind {
                    ControlKind::Loop { exit, .. } | ControlKind::Switch { exit } => Some(exit),
                    ControlKind::Yield { .. } => None,
                }) else {
                    return self.error(span, "break outside of a loop or switch");
                };
                self.cleanup_to(boundary, false, span)?;
                self.jump(exit, Vec::new(), span);
                Err(Stop::Diverged)
            }
            HMIRControlOp::Continue => {
                let Some((boundary, next)) = self.find_control(|kind| match kind {
                    ControlKind::Loop { next, .. } => Some(next),
                    _ => None,
                }) else {
                    return self.error(span, "continue outside of a loop");
                };
                self.cleanup_to(boundary, false, span)?;
                self.jump(next, Vec::new(), span);
                Err(Stop::Diverged)
            }
            HMIRControlOp::Goto(name) => {
                let target = self.label(&name);
                self.jump(target, Vec::new(), span);
                Err(Stop::Diverged)
            }
            HMIRControlOp::Defer(body) => {
                self.scopes
                    .last_mut()
                    .expect("function has a scope")
                    .defers
                    .push((frame, body));
                Ok(Operand::unit(self.program.types_mut()))
            }
            HMIRControlOp::Unsafe(body) => self.expr(frame, body, expect),
            HMIRControlOp::Unreachable => {
                self.emit(MIRInstructionKind::Unreachable, span);
                Err(Stop::Diverged)
            }
        }
    }

    fn push_control(&mut self, kind: ControlKind) {
        let boundary = self.current_scope();
        self.controls.push(Control { boundary, kind });
    }

    fn find_control<T>(
        &self,
        select: impl Fn(ControlKind) -> Option<T>,
    ) -> Option<(cx_mir::MIRScopeID, T)> {
        self.controls
            .iter()
            .rev()
            .find_map(|control| select(control.kind).map(|found| (control.boundary, found)))
    }

    fn open_merge(&mut self, name: &str, expect: Expect, span: &TokenRange) -> Lower<usize> {
        let block = self.new_block(name);
        let param = match expect {
            Expect::Discard => MergeParam::Valueless,
            Expect::Any => MergeParam::Undecided,
            Expect::Type(ty) => self.merge_param(block, ty, span)?,
        };
        self.merges.push(Merge {
            block,
            param,
            reached: false,
        });
        Ok(self.merges.len() - 1)
    }

    fn merge_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: TypeID,
        span: &TokenRange,
    ) -> Lower<MergeParam> {
        let types = self.program.types();
        if types.is_void(ty) || types.is_unreachable(ty) {
            return Ok(MergeParam::Valueless);
        }
        let mir = self.mir(ty, span)?;
        Ok(MergeParam::Value(
            self.body.add_block_param(block, mir, None),
            ty,
        ))
    }

    fn merge_expect(&self, merge: usize) -> Expect {
        match self.merges[merge].param {
            MergeParam::Undecided => Expect::Any,
            MergeParam::Valueless => Expect::Discard,
            MergeParam::Value(_, ty) => Expect::Type(ty),
        }
    }

    // Jumps from the current block into 'merge', passing 'value' when the merge takes one
    fn merge_edge(&mut self, merge: usize, value: Option<Operand>, span: &TokenRange) -> Lower<()> {
        if self.terminated() {
            return Ok(());
        }
        let block = self.merges[merge].block;
        if let (MergeParam::Undecided, Some(value)) = (self.merges[merge].param, &value) {
            let ty = self.inferred_type(value.ty());
            self.merges[merge].param = self.merge_param(block, ty, span)?;
        }
        match (self.merges[merge].param, value) {
            (MergeParam::Value(_, ty), Some(value)) => {
                let value = self.convert(value, ty, span)?;
                let value = self.value(value, span)?;
                self.jump(block, vec![value], span);
            }
            (MergeParam::Value(..), None) => {
                self.emit(MIRInstructionKind::Unreachable, span);
                return Ok(());
            }
            (MergeParam::Undecided, None) => {
                self.merges[merge].param = MergeParam::Valueless;
                self.jump(block, Vec::new(), span);
            }
            (MergeParam::Valueless | MergeParam::Undecided, _) => {
                self.jump(block, Vec::new(), span);
            }
        }
        self.merges[merge].reached = true;
        Ok(())
    }

    fn yield_to(&mut self, merge: usize, value: Option<Operand>, span: &TokenRange) -> Lower<()> {
        self.merge_edge(merge, value, span)
    }

    fn close_merge(&mut self, merge: usize, span: &TokenRange) -> Lower<Operand> {
        let Merge {
            block,
            param,
            reached,
        } = self.merges[merge];
        self.set_block(block);
        if !reached {
            self.emit(MIRInstructionKind::Unreachable, span);
            return Err(Stop::Diverged);
        }
        Ok(match param {
            MergeParam::Value(register, ty) => Operand::register(register, ty),
            _ => Operand::unit(self.program.types_mut()),
        })
    }
}

// What a scoped body produces; values outliving the scope are read before it ends
pub(crate) trait ScopedResult: Sized {
    fn settle(
        self,
        lowering: &mut FunctionLowering,
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Self>;
}

impl ScopedResult for () {
    fn settle(self, _: &mut FunctionLowering, _: Expect, _: &TokenRange) -> Lower<Self> {
        Ok(())
    }
}

impl ScopedResult for Operand {
    fn settle(
        self,
        lowering: &mut FunctionLowering,
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Self> {
        if expect == Expect::Discard || !self.is_lvalue() {
            return Ok(self);
        }
        let is_reference = matches!(
            lowering.program.types().kind(self.ty()),
            TypeKind::Reference(_)
        );
        if is_reference {
            return Ok(self);
        }
        lowering.read(self, span)
    }
}
