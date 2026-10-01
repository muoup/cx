mod aggregate;
mod call;
mod control;
mod expr;
mod operand;
mod ops;

use std::{collections::HashMap, rc::Rc};

use cx_hmir::{HMIRDefKind, HMIRExprID, HMIRExprKind, HMIRLocalID, HMIRPattern, HMIRUnit};
use cx_log::{CXResult, error::CXError};
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRBody, MIRConstant, MIRFunctionID,
    MIRInstruction, MIRInstructionKind, MIRIntType, MIRIntrinsic, MIRPlaceID, MIRRegisterID,
    MIRScopeID, MIRTypeID, MIRValue, expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    eval::{EvalFrame, RuntimeView},
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

pub(crate) type Lower<T> = Result<T, Stop>;

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
    def: DefKey,
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
    program: &'p mut Program<'l>,
    serial: u64,
    body: MIRBody,
    current: MIRBasicBlockID,
    frames: Vec<Frame>,
    bindings: HashMap<(usize, HMIRLocalID), Operand>,
    scopes: Vec<Scope>,
    controls: Vec<Control>,
    merges: Vec<Merge>,
    labels: HashMap<String, MIRBasicBlockID>,
    pattern_bindings: Vec<PatternBinding>,
    ret: TypeID,
}

impl Expect {
    pub(crate) fn ty(self) -> Option<TypeID> {
        match self {
            Expect::Type(ty) => Some(ty),
            _ => None,
        }
    }

    fn of(ty: Option<TypeID>) -> Self {
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

    fn body(&self) -> &cx_hmir::HMIRBody {
        def_body(self.unit.def(self.def.def())).expect("lowered def has a body")
    }
}

impl Program<'_> {
    pub(crate) fn lower_function(
        &mut self,
        instance: &Instance,
        id: MIRFunctionID,
    ) -> CXResult<MIRBody> {
        let unit = self.unit(instance.0.unit());
        let def = unit.def(instance.0.def());
        let span = def.span().clone();
        let HMIRDefKind::Function(function) = def.kind() else {
            return Err(staging_error(&span, "lowered a non-function".into()));
        };
        let root = function.root().expect("queued functions have a body");
        let signature = self.signature(instance, &span)?;
        let prototype = self.module().function(id).prototype().clone();
        let main = prototype.symbol_name.as_str() == "main";
        let statics = self.frame_for(instance).locals().clone();
        let serial = self.next_serial();

        let mut lowering = FunctionLowering::new(self, serial, signature.ret(), &span);
        let mut frame = Frame::new(unit.clone(), instance.0, Rc::new(instance.clone()));
        frame.statics = statics;
        lowering.frames.push(frame);

        let root_scope = lowering.scopes[0].id;
        for (index, local) in signature.runtime().iter().enumerate() {
            let param = &prototype.signature.params()[index];
            let place = lowering.body.add_parameter(param, root_scope);
            lowering.emit(
                MIRInstructionKind::Initialize {
                    place: MIRBindable::Place(place),
                },
                &span,
            );
            let ty = signature.params()[index].1;
            lowering.bind(0, *local, Operand::place(place, ty));
        }

        lowering.lower_root(root, main)?;
        Ok(lowering.body)
    }
}

impl<'p, 'l> FunctionLowering<'p, 'l> {
    fn new(program: &'p mut Program<'l>, serial: u64, ret: TypeID, span: &TokenRange) -> Self {
        let mut body = MIRBody::new();
        let entry = body.add_block();
        let root = body.add_scope(span.clone());
        Self {
            program,
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
        }
    }

    fn lower_root(&mut self, root: HMIRExprID, main: bool) -> CXResult<()> {
        let span = self.span(0, root);
        let is_block = matches!(
            self.frames[0].body().expr(root).kind(),
            HMIRExprKind::Block { .. }
        );
        let result = if is_block {
            self.expr(0, root, Expect::Discard).map(|_| None)
        } else {
            self.expr(0, root, Expect::Type(self.ret)).map(Some)
        };
        let value = match result {
            Ok(value) => value,
            Err(Stop::Diverged) => return Ok(()),
            Err(Stop::Error(error)) => return Err(error),
        };
        if self.terminated() {
            return Ok(());
        }
        let outcome = (|| -> Lower<()> {
            if let Some(value) = value {
                return self.emit_return(Some(value), &span);
            }
            if self.program.types().is_void(self.ret) {
                return self.emit_return(None, &span);
            }
            if main {
                let zero = Operand::value(
                    MIRValue::Constant(MIRConstant::Integer {
                        ty: MIRIntType::I32,
                        value: 0,
                    }),
                    self.ret,
                );
                return self.emit_return(Some(zero), &span);
            }
            self.emit(MIRInstructionKind::Unreachable, &span);
            Ok(())
        })();
        match outcome {
            Ok(()) | Err(Stop::Diverged) => Ok(()),
            Err(Stop::Error(error)) => Err(error),
        }
    }

    pub(crate) fn span(&self, frame: usize, id: HMIRExprID) -> TokenRange {
        self.frames[frame].body().expr(id).span().clone()
    }

    pub(crate) fn kind(&self, frame: usize, id: HMIRExprID) -> HMIRExprKind {
        self.frames[frame].body().expr(id).kind().clone()
    }

    fn error<T>(&self, span: &TokenRange, message: impl Into<String>) -> Lower<T> {
        Err(Stop::Error(staging_error(span, message.into())))
    }

    pub(crate) fn emit(&mut self, kind: MIRInstructionKind, span: &TokenRange) {
        if !self.terminated() {
            self.body
                .push_instr_at(self.current, MIRInstruction::new(kind, span.clone()));
        }
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

    pub(crate) fn mir(&mut self, ty: TypeID, span: &TokenRange) -> Lower<MIRTypeID> {
        Ok(self.program.types_mut().mir(ty, span)?)
    }

    pub(crate) fn register(&mut self, ty: TypeID, span: &TokenRange) -> Lower<MIRRegisterID> {
        let mir = self.mir(ty, span)?;
        Ok(self.body.add_register(mir, None))
    }

    pub(crate) fn place(
        &mut self,
        ty: TypeID,
        name: Option<CXIdent>,
        span: &TokenRange,
    ) -> Lower<MIRPlaceID> {
        let mir = self.mir(ty, span)?;
        let nodrop = self.program.types().is_nodrop(ty);
        let scope = self.scopes.last().expect("function has a scope").id;
        Ok(self.body.add_place(mir, name, nodrop, scope))
    }

    pub(crate) fn bind(&mut self, frame: usize, local: HMIRLocalID, operand: Operand) {
        self.bindings.insert((frame, local), operand);
    }

    pub(crate) fn binding(&self, frame: usize, local: HMIRLocalID) -> Option<Operand> {
        let mut current = Some(frame);
        while let Some(index) = current {
            if let Some(operand) = self.bindings.get(&(index, local)) {
                return Some(operand.clone());
            }
            current = self.frames[index].origin;
        }
        None
    }

    pub(crate) fn static_binding(&self, frame: usize, local: HMIRLocalID) -> Option<StaticValue> {
        let mut current = Some(frame);
        while let Some(index) = current {
            if let Some(value) = self.frames[index].statics.get(&local) {
                return Some(value.clone());
            }
            current = self.frames[index].origin;
        }
        None
    }

    // An evaluator frame seeing this frame's comptime bindings and its runtime bindings' types
    fn eval_frame(&self, frame: usize) -> EvalFrame {
        let current = &self.frames[frame];
        let mut types = HashMap::new();
        let mut chain = Some(frame);
        while let Some(index) = chain {
            for ((owner, local), operand) in &self.bindings {
                if *owner == index {
                    types.entry(*local).or_insert(operand.ty());
                }
            }
            chain = self.frames[index].origin;
        }
        let mut eval =
            EvalFrame::new(current.unit.clone(), current.def, current.owner.clone()).with_runtime(
                RuntimeView::new(Some(FrameRef::new(self.serial, frame)), types),
            );
        let mut chain = Some(frame);
        let mut frames = Vec::new();
        while let Some(index) = chain {
            frames.push(index);
            chain = self.frames[index].origin;
        }
        for index in frames.into_iter().rev() {
            for (local, value) in &self.frames[index].statics {
                eval.bind(*local, value.clone());
            }
        }
        eval
    }

    pub(crate) fn eval(
        &mut self,
        frame: usize,
        id: HMIRExprID,
        expect: Expect,
    ) -> Lower<StaticValue> {
        let mut eval = self.eval_frame(frame);
        let value = self.program.eval_expecting(&mut eval, id, expect.ty())?;
        for (local, value) in eval.locals() {
            if self.frames[frame].statics.contains_key(local) {
                self.frames[frame].statics.insert(*local, value.clone());
            }
        }
        Ok(value)
    }

    pub(crate) fn eval_type(&mut self, frame: usize, id: HMIRExprID) -> Lower<TypeID> {
        let mut eval = self.eval_frame(frame);
        Ok(self.program.eval_type(&mut eval, id)?)
    }

    pub(crate) fn eval_type_hint(&mut self, frame: usize, id: HMIRExprID) -> Lower<Option<TypeID>> {
        let mut eval = self.eval_frame(frame);
        Ok(self.program.eval_type_hint(&mut eval, id)?)
    }

    pub(crate) fn type_hint(&mut self, frame: usize, id: HMIRExprID) -> Option<TypeID> {
        let mut eval = self.eval_frame(frame);
        self.program.type_hint(&mut eval, id)
    }

    pub(crate) fn push_scope(&mut self, span: &TokenRange) {
        let id = self.body.add_scope(span.clone());
        self.scopes.push(Scope {
            id,
            defers: Vec::new(),
        });
    }

    // Runs the innermost scope's defers and drops its places, then leaves it
    pub(crate) fn pop_scope(&mut self, span: &TokenRange) -> Lower<()> {
        let result = if self.terminated() {
            Ok(())
        } else {
            let index = self.scopes.len() - 1;
            self.cleanup_scope(index, span)
        };
        self.scopes.pop();
        result
    }

    pub(crate) fn current_scope(&self) -> MIRScopeID {
        self.scopes.last().expect("function has a scope").id
    }

    // Cleans up every scope inside 'boundary', and 'boundary' itself when 'inclusive'
    pub(crate) fn cleanup_to(
        &mut self,
        boundary: MIRScopeID,
        inclusive: bool,
        span: &TokenRange,
    ) -> Lower<()> {
        for index in (0..self.scopes.len()).rev() {
            let id = self.scopes[index].id;
            if id == boundary && !inclusive {
                break;
            }
            self.cleanup_scope(index, span)?;
            if id == boundary {
                break;
            }
        }
        Ok(())
    }

    fn cleanup_scope(&mut self, index: usize, span: &TokenRange) -> Lower<()> {
        let defers = self.scopes[index].defers.clone();
        let saved = self.scopes.split_off(index + 1);
        let mut result = Ok(());
        for (frame, expr) in defers.into_iter().rev() {
            self.push_scope(span);
            let lowered = self.expr(frame, expr, Expect::Discard).map(|_| ());
            let popped = self.pop_scope(span);
            result = lowered.and(popped);
            if result.is_err() {
                break;
            }
        }
        self.scopes.extend(saved);
        result?;
        let id = self.scopes[index].id;
        let places = self
            .body
            .places()
            .iter()
            .filter(|place| place.scope == id)
            .rev()
            .map(|place| place.id)
            .collect::<Vec<_>>();
        for place in places {
            self.emit(
                MIRInstructionKind::Invalidate {
                    place: MIRBindable::Place(place),
                    kind: MIRInvalidationKind::Drop,
                },
                span,
            );
        }
        Ok(())
    }

    pub(crate) fn emit_return(&mut self, value: Option<Operand>, span: &TokenRange) -> Lower<()> {
        let value = match value {
            Some(value) if !self.program.types().is_void(self.ret) => {
                let value = self.convert(value, self.ret, span)?;
                Some(self.value(value, span)?)
            }
            _ => None,
        };
        let root = self.scopes[0].id;
        self.cleanup_to(root, true, span)?;
        self.emit(MIRInstructionKind::Return { value }, span);
        Err(Stop::Diverged)
    }
}
