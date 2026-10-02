mod aggregate;
pub(crate) mod call;
mod coerce;
mod contract;
pub(crate) mod control;
pub(crate) mod expr;
pub(crate) mod inspect;
mod operand;
mod ops;
mod promote;

use std::{collections::HashMap, rc::Rc};

use cx_hmir::{
    HMIRBody, HMIRDefKind, HMIRExprID, HMIRExprKind, HMIRLocalID, HMIRPattern, HMIRUnit,
};
use cx_log::{
    CXResult,
    catalogue::mir,
    error::{CXError, context::from_token_range},
};
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRBody, MIRConstant, MIRFnPrototype,
    MIRFunctionID, MIRInstruction, MIRInstructionKind, MIRIntType, MIRIntrinsic, MIRPlaceID,
    MIRRegisterID, MIRScopeID, MIRTypeID, MIRValue,
    expr::{instruction::MIRInvalidationKind, visit},
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    eval::{
        EvalFrame, RuntimeView, eval, eval_frame_for, eval_signature, eval_type, eval_type_hint,
        type_hint,
    },
    function::{coerce::lower_convert, expr::lower_expr, operand::lower_value},
    program::{DefKey, Instance, Program, def_body},
    staging_error,
    ty::TypeID,
    value::{FrameRef, StaticValue},
};

pub(crate) use operand::{Operand, OperandKind};

pub(crate) enum Stop {
    Diverged,
    Error(CXError),
}

pub(crate) type LowerResult<T> = Result<T, Stop>;

impl From<CXError> for Stop {
    fn from(error: CXError) -> Self {
        Stop::Error(error)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Expect {
    Discard,
    Any,
    Type(TypeID),
}

// A lexical instantiation of some def's body; quotes spliced at runtime get their own frame
pub(crate) struct Frame {
    unit: Rc<HMIRUnit>,
    pub(crate) def: DefKey,
    owner: Rc<Instance>,
    statics: HashMap<HMIRLocalID, StaticValue>,
    // Frame whose runtime bindings a spliced quote refers to
    origin: Option<usize>,
}

struct Scope {
    id: MIRScopeID,
    defers: Vec<(usize, HMIRExprID)>,
}

#[derive(Clone, Copy)]
enum ControlKind {
    Loop {
        exit: MIRBasicBlockID,
        next: MIRBasicBlockID,
    },
    Switch {
        exit: MIRBasicBlockID,
    },
    Yield {
        merge: usize,
    },
}

#[derive(Clone, Copy)]
struct Control {
    boundary: MIRScopeID,
    kind: ControlKind,
}

// A join point; with no expected type, the first value reaching it decides its parameter
struct Merge {
    block: MIRBasicBlockID,
    param: MergeParam,
    reached: bool,
}

#[derive(Clone, Copy)]
enum MergeParam {
    Undecided,
    Valueless,
    Value(MIRRegisterID, TypeID),
}

// A pattern tested in a condition whose bindings take effect in the branch it guards
pub(crate) struct PatternBinding {
    frame: usize,
    subject: Operand,
    owned: bool,
    pattern: HMIRPattern,
}

pub(crate) struct FunctionLowering<'p, 'l> {
    pub(crate) program: &'p mut Program<'l>,
    serial: u64,
    body: MIRBody,
    current: MIRBasicBlockID,
    pub(crate) frames: Vec<Frame>,
    bindings: HashMap<(usize, HMIRLocalID), Operand>,
    scopes: Vec<Scope>,
    controls: Vec<Control>,
    merges: Vec<Merge>,
    labels: HashMap<String, MIRBasicBlockID>,
    pattern_bindings: Vec<PatternBinding>,
    ret: TypeID,
    pub(crate) unevaluated: bool,
    // A safe function may only perform unsafe operations inside '@unsafe'
    safe: bool,
    unsafe_depth: usize,
}

impl Expect {
    pub(crate) fn ty(self) -> Option<TypeID> {
        match self {
            Expect::Type(ty) => Some(ty),
            _ => None,
        }
    }

    pub(crate) fn of(ty: Option<TypeID>) -> Self {
        ty.map(Expect::Type).unwrap_or(Expect::Any)
    }
}

impl Frame {
    fn new(unit: Rc<HMIRUnit>, def: DefKey, owner: Rc<Instance>) -> Self {
        Self {
            unit,
            def,
            owner,
            statics: HashMap::new(),
            origin: None,
        }
    }

    fn body(&self) -> &HMIRBody {
        def_body(self.unit.def(self.def.def())).expect("lowered def has a body")
    }
}

pub(crate) fn lower_function(
    cx: &mut Program<'_>,
    instance: &Instance,
    id: MIRFunctionID,
) -> CXResult<MIRBody> {
    let unit = cx.unit(instance.0.unit());
    let def = unit.def(instance.0.def());
    let span = def.span().clone();
    let HMIRDefKind::Function(function) = def.kind() else {
        return Err(staging_error(&span, "lowered a non-function".into()));
    };
    let root = function.root().expect("queued functions have a body");
    let signature = eval_signature(cx, instance, &span)?;
    let prototype = cx.module().function(id).prototype().clone();
    let statics = eval_frame_for(cx, instance).locals().clone();
    let serial = cx.next_serial();

    let mut cx = FunctionLowering::new(cx, serial, signature.ret(), &span);
    cx.safe = function.signature().contract().is_safe();
    let mut frame = Frame::new(unit.clone(), instance.0, Rc::new(instance.clone()));
    frame.statics = statics;
    cx.frames.push(frame);

    let root_scope = cx.scopes[0].id;
    for (index, local) in signature.runtime().iter().enumerate() {
        let param = &prototype.signature.params()[index];
        let place = cx.body.add_parameter(param, root_scope);
        cx.initialize(place, &span);
        let ty = signature.params()[index].1;
        cx.bind(0, *local, Operand::place(place, ty));
    }

    lower_root(&mut cx, root, &prototype)?;
    Ok(cx.body)
}

fn lower_root(
    cx: &mut FunctionLowering<'_, '_>,
    root: HMIRExprID,
    prototype: &MIRFnPrototype,
) -> CXResult<()> {
    let span = cx.span(0, root);
    let is_block = matches!(
        cx.frames[0].body().expr(root).kind(),
        HMIRExprKind::Block { .. }
    );
    let result = if is_block {
        lower_expr(cx, 0, root, Expect::Discard).map(|_| None)
    } else {
        lower_expr(cx, 0, root, Expect::Type(cx.ret)).map(Some)
    };
    let value = match result {
        Ok(value) => value,
        Err(Stop::Diverged) => return Ok(()),
        Err(Stop::Error(error)) => return Err(error),
    };
    if cx.terminated() {
        return Ok(());
    }
    let outcome = (|| -> LowerResult<()> {
        if let Some(value) = value {
            return lower_return(cx, Some(value), &span);
        }
        if cx.program.types().is_void(cx.ret) {
            return lower_return(cx, None, &span);
        }
        if prototype.symbol_name.as_str() == "main" {
            let zero = Operand::value(
                MIRValue::Constant(MIRConstant::Integer {
                    ty: MIRIntType::I32,
                    value: 0,
                }),
                cx.ret,
            );
            return lower_return(cx, Some(zero), &span);
        }
        if cx.program.require_explicit_return()
            && !cx.program.types().is_unreachable(cx.ret)
            && cx.reachable()
        {
            return Err(Stop::Error(CXError::new(
                mir::FUNCTION_RETURN.bind(prototype.symbol_name.to_string()),
                from_token_range(&span),
            )));
        }
        cx.emit(MIRInstructionKind::Unreachable, &span);
        Ok(())
    })();
    match outcome {
        Ok(()) | Err(Stop::Diverged) => Ok(()),
        Err(Stop::Error(error)) => Err(error),
    }
}

impl<'p, 'l> FunctionLowering<'p, 'l> {
    fn new(cx: &'p mut Program<'l>, serial: u64, ret: TypeID, span: &TokenRange) -> Self {
        let mut body = MIRBody::new();
        let entry = body.add_block();
        let root = body.add_scope(span.clone());
        Self {
            program: cx,
            serial,
            body,
            current: entry,
            frames: Vec::new(),
            bindings: HashMap::new(),
            scopes: vec![Scope {
                id: root,
                defers: Vec::new(),
            }],
            controls: Vec::new(),
            merges: Vec::new(),
            labels: HashMap::new(),
            pattern_bindings: Vec::new(),
            ret,
            unevaluated: false,
            safe: false,
            unsafe_depth: 0,
        }
    }

    pub(crate) fn span(&self, frame: usize, id: HMIRExprID) -> TokenRange {
        self.frames[frame].body().expr(id).span().clone()
    }

    pub(crate) fn kind(&self, frame: usize, id: HMIRExprID) -> HMIRExprKind {
        self.frames[frame].body().expr(id).kind().clone()
    }

    fn error<T>(&self, span: &TokenRange, message: impl Into<String>) -> LowerResult<T> {
        Err(Stop::Error(staging_error(span, message.into())))
    }

    pub(crate) fn require_mutable(
        &self,
        ty: TypeID,
        action: &str,
        span: &TokenRange,
    ) -> LowerResult<()> {
        let types = self.program.types();
        if types.is_const(ty) {
            return self.error(
                span,
                format!("cannot {action} a value of type '{}'", types.display(ty)),
            );
        }
        Ok(())
    }

    pub(crate) fn require_unsafe(&self, operation: &str, span: &TokenRange) -> LowerResult<()> {
        if !self.safe || self.unsafe_depth > 0 {
            return Ok(());
        }
        self.error(
            span,
            format!(
                "{operation} is unsafe and so cannot be used in safe contexts, wrap this \
                 expression in an `@unsafe` block to bypass this restriction"
            ),
        )
    }

    pub(crate) fn emit(&mut self, kind: MIRInstructionKind, span: &TokenRange) {
        if !self.terminated() {
            self.body
                .push_instr_at(self.current, MIRInstruction::new(kind, span.clone()));
        }
    }

    pub(crate) fn initialize(&mut self, place: MIRPlaceID, span: &TokenRange) {
        self.emit(
            MIRInstructionKind::Initialize {
                place: MIRBindable::Place(place),
            },
            span,
        );
    }

    pub(crate) fn invalidate(
        &mut self,
        place: MIRBindable,
        kind: MIRInvalidationKind,
        span: &TokenRange,
    ) {
        self.emit(MIRInstructionKind::Invalidate { place, kind }, span);
    }

    pub(crate) fn intrinsic(&mut self, intrinsic: impl Into<MIRIntrinsic>, span: &TokenRange) {
        self.emit(MIRInstructionKind::IntrinsicOp(intrinsic.into()), span);
    }

    pub(crate) fn jump(&mut self, block: MIRBasicBlockID, args: Vec<MIRValue>, span: &TokenRange) {
        self.emit(
            MIRInstructionKind::Jump {
                target: MIRBlockTarget::with_args(block, args),
            },
            span,
        );
    }

    fn reachable(&self) -> bool {
        let mut visited = vec![false; self.body.blocks().len()];
        let mut pending = vec![self.body.entry()];
        while let Some(id) = pending.pop() {
            if id == self.current {
                return true;
            }
            if std::mem::replace(&mut visited[id.index()], true) {
                continue;
            }
            if let Some(instruction) = self
                .body
                .block(id)
                .and_then(|block| block.last_instruction())
            {
                pending.extend(
                    visit::successors(instruction)
                        .into_iter()
                        .map(|target| target.block),
                );
            }
        }
        false
    }

    pub(crate) fn terminated(&self) -> bool {
        self.body
            .block(self.current)
            .and_then(|block| block.last_instruction())
            .is_some_and(MIRInstruction::is_terminator)
    }

    pub(crate) fn new_block(&mut self, name: &str) -> MIRBasicBlockID {
        self.body.add_block_named(name)
    }

    pub(crate) fn set_block(&mut self, block: MIRBasicBlockID) {
        self.current = block;
    }

    pub(crate) fn mir(&mut self, ty: TypeID, span: &TokenRange) -> LowerResult<MIRTypeID> {
        Ok(self.program.types_mut().mir(ty, span)?)
    }

    pub(crate) fn register(&mut self, ty: TypeID, span: &TokenRange) -> LowerResult<MIRRegisterID> {
        let mir = self.mir(ty, span)?;
        Ok(self.body.add_register(mir, None))
    }

    pub(crate) fn place(
        &mut self,
        ty: TypeID,
        name: Option<CXIdent>,
        span: &TokenRange,
    ) -> LowerResult<MIRPlaceID> {
        if let Some(problem) = self.program.types().object_problem(ty) {
            return self.error(span, format!("variable has {problem}"));
        }
        let mir = self.mir(ty, span)?;
        let nodrop = self.program.types().is_nodrop(ty);
        let scope = self.scopes.last().expect("function has a scope").id;
        Ok(self.body.add_place(mir, name, nodrop, scope))
    }

    pub(crate) fn bind(&mut self, frame: usize, local: HMIRLocalID, operand: Operand) {
        self.bindings.insert((frame, local), operand);
    }

    fn frame_chain(&self, frame: usize) -> impl Iterator<Item = usize> + '_ {
        std::iter::successors(Some(frame), |index| self.frames[*index].origin)
    }

    pub(crate) fn binding(&self, frame: usize, local: HMIRLocalID) -> Option<Operand> {
        self.frame_chain(frame)
            .find_map(|index| self.bindings.get(&(index, local)).cloned())
    }

    pub(crate) fn static_binding(&self, frame: usize, local: HMIRLocalID) -> Option<StaticValue> {
        self.frame_chain(frame)
            .find_map(|index| self.frames[index].statics.get(&local).cloned())
    }

    pub(crate) fn push_scope(&mut self, span: &TokenRange) {
        let id = self.body.add_scope(span.clone());
        self.scopes.push(Scope {
            id,
            defers: Vec::new(),
        });
    }

    // Runs the innermost scope's defers and drops its places, then leaves it
    pub(crate) fn pop_scope(&mut self, span: &TokenRange) -> LowerResult<()> {
        let result = if self.terminated() {
            Ok(())
        } else {
            let index = self.scopes.len() - 1;
            lower_cleanup_scope(self, index, span)
        };
        self.scopes.pop();
        result
    }

    pub(crate) fn current_scope(&self) -> MIRScopeID {
        self.scopes.last().expect("function has a scope").id
    }
}

pub(crate) fn lower_eval(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    id: HMIRExprID,
    expect: Expect,
) -> LowerResult<StaticValue> {
    let mut eval_frame = lower_eval_frame(cx, frame);
    let value = eval(cx.program, &mut eval_frame, id, expect.ty())?;
    for (local, value) in eval_frame.locals() {
        if cx.frames[frame].statics.contains_key(local) {
            cx.frames[frame].statics.insert(*local, value.clone());
        }
    }
    Ok(value)
}

// An evaluator frame seeing this frame's comptime bindings and its runtime bindings' types
pub(crate) fn lower_eval_frame(cx: &FunctionLowering<'_, '_>, frame: usize) -> EvalFrame {
    let current = &cx.frames[frame];
    let mut types = HashMap::new();
    for index in cx.frame_chain(frame) {
        for ((owner, local), operand) in &cx.bindings {
            if *owner == index {
                types.entry(*local).or_insert(operand.ty());
            }
        }
    }
    let mut eval =
        EvalFrame::new(current.unit.clone(), current.def, current.owner.clone()).with_runtime(
            RuntimeView::new(Some(FrameRef::new(cx.serial, frame)), types),
        );
    for index in cx.frame_chain(frame).collect::<Vec<_>>().into_iter().rev() {
        for (local, value) in &cx.frames[index].statics {
            eval.bind(*local, value.clone());
        }
    }
    eval
}

// Cleans up every scope inside 'boundary', and 'boundary' itself when 'inclusive'
pub(crate) fn lower_cleanup_to(
    cx: &mut FunctionLowering<'_, '_>,
    boundary: MIRScopeID,
    inclusive: bool,
    span: &TokenRange,
) -> LowerResult<()> {
    for index in (0..cx.scopes.len()).rev() {
        let id = cx.scopes[index].id;
        if id == boundary && !inclusive {
            break;
        }
        lower_cleanup_scope(cx, index, span)?;
        if id == boundary {
            break;
        }
    }
    Ok(())
}

fn lower_cleanup_scope(
    cx: &mut FunctionLowering<'_, '_>,
    index: usize,
    span: &TokenRange,
) -> LowerResult<()> {
    let defers = cx.scopes[index].defers.clone();
    let saved = cx.scopes.split_off(index + 1);
    let mut result = Ok(());
    for (frame, expr) in defers.into_iter().rev() {
        cx.push_scope(span);
        let lowered = lower_expr(cx, frame, expr, Expect::Discard).and_then(|value| {
            let types = cx.program.types();
            if types.is_void(value.ty()) || types.is_unreachable(value.ty()) {
                return Ok(());
            }
            let found = types.display(value.ty());
            let span = cx.span(frame, expr);
            cx.error(
                &span,
                format!("defer requires a void expression, found '{found}'"),
            )
        });
        let popped = cx.pop_scope(span);
        result = lowered.and(popped);
        if result.is_err() {
            break;
        }
    }
    cx.scopes.extend(saved);
    result?;
    let id = cx.scopes[index].id;
    let places = cx
        .body
        .places()
        .iter()
        .filter(|place| place.scope == id)
        .rev()
        .map(|place| place.id)
        .collect::<Vec<_>>();
    for place in places {
        cx.invalidate(MIRBindable::Place(place), MIRInvalidationKind::Drop, span);
    }
    Ok(())
}

pub(crate) fn lower_return(
    cx: &mut FunctionLowering<'_, '_>,
    value: Option<Operand>,
    span: &TokenRange,
) -> LowerResult<()> {
    let value = match value {
        Some(value) if !cx.program.types().is_void(cx.ret) => {
            let value = lower_convert(cx, value, cx.ret, span)?;
            Some(lower_value(cx, value, span)?)
        }
        _ => None,
    };
    contract::lower_return_postcondition(cx, value.as_ref())?;
    let root = cx.scopes[0].id;
    lower_cleanup_to(cx, root, true, span)?;
    cx.emit(MIRInstructionKind::Return { value }, span);
    Err(Stop::Diverged)
}

pub(crate) fn lower_eval_type(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    id: HMIRExprID,
) -> LowerResult<TypeID> {
    let mut eval_frame = lower_eval_frame(cx, frame);
    Ok(eval_type(cx.program, &mut eval_frame, id)?)
}

pub(crate) fn lower_eval_type_hint(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    id: HMIRExprID,
) -> LowerResult<Option<TypeID>> {
    let mut eval_frame = lower_eval_frame(cx, frame);
    Ok(eval_type_hint(cx.program, &mut eval_frame, id)?)
}

pub(crate) fn lower_type_hint(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    id: HMIRExprID,
) -> Option<TypeID> {
    let mut eval_frame = lower_eval_frame(cx, frame);
    type_hint(cx.program, &mut eval_frame, id)
}
