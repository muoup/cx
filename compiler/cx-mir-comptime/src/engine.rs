use std::collections::{HashMap, HashSet};

use cx_log::CXResult;
use cx_mir::{
    MIRAggregateOp, MIRAssignTarget, MIRBinaryOp, MIRBlockTarget, MIRCoercion, MIRConstant,
    MIRFieldLayout, MIRFloatBinaryOp, MIRFunctionID, MIRFunctionMode, MIRGlobalID, MIRGlobalKind,
    MIRInstrKind, MIRIntBinaryOp, MIRIntType, MIRParameterID, MIRPlace, MIRPointerBinaryOp,
    MIRPointerOffsetOp, MIRRegister, MIRTypeID, MIRTypeKind, MIRUnaryOp, MIRValue,
    ty::interface::MTRegistry,
    ty::layout::{field_layout, layout_of},
};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    context::ComptimeResolver,
    error::comptime_error,
    interpretable::{ComptimeInterpretable, InterpretedFunction},
    value::{MIRComptimeValue, MIRStagedBinding, MIRStagedValue},
};

const DEFAULT_STEP_BUDGET: u64 = 1_000_000;
const DEFAULT_MAX_DEPTH: usize = 128;

#[derive(Debug, Clone, Copy)]
pub struct EngineLimits {
    pub max_steps: u64,
    pub max_call_depth: usize,
}

impl Default for EngineLimits {
    fn default() -> Self {
        Self {
            max_steps: DEFAULT_STEP_BUDGET,
            max_call_depth: DEFAULT_MAX_DEPTH,
        }
    }
}

#[derive(Debug, Clone)]
enum PathSeg {
    Field(usize),
    Index(i128),
    Variant(usize),
}

impl PathSeg {
    fn key(&self) -> usize {
        match self {
            PathSeg::Field(key) | PathSeg::Variant(key) => *key,
            PathSeg::Index(index) => *index as usize,
        }
    }
}

struct Frame<'ctx> {
    code: InterpretedFunction<'ctx>,
    registers: HashMap<MIRRegister, MIRComptimeValue>,
    cells: HashMap<MIRPlace, MIRComptimeValue>,
    derived: HashMap<MIRPlace, (MIRPlace, Vec<PathSeg>)>,
}

impl<'ctx> Frame<'ctx> {
    fn new(code: InterpretedFunction<'ctx>) -> Self {
        Self {
            code,
            registers: HashMap::new(),
            cells: HashMap::new(),
            derived: HashMap::new(),
        }
    }
}

pub struct MIRComptimeEngine<'ctx> {
    resolver: &'ctx dyn ComptimeResolver,
    limits: EngineLimits,
    frames: Vec<Frame<'ctx>>,
    globals: HashMap<MIRGlobalID, MIRConstant>,
    evaluating_globals: HashSet<MIRGlobalID>,
    steps: u64,
}

impl<'ctx> MIRComptimeEngine<'ctx> {
    pub fn new(resolver: &'ctx dyn ComptimeResolver) -> Self {
        Self::with_limits(resolver, EngineLimits::default())
    }

    pub fn with_limits(resolver: &'ctx dyn ComptimeResolver, limits: EngineLimits) -> Self {
        Self {
            resolver,
            limits,
            frames: Vec::new(),
            globals: HashMap::new(),
            evaluating_globals: HashSet::new(),
            steps: 0,
        }
    }

    pub fn run(
        &mut self,
        entry: InterpretedFunction<'ctx>,
        args: &[MIRConstant],
    ) -> CXResult<MIRConstant> {
        let args = args
            .iter()
            .cloned()
            .map(MIRComptimeValue::Constant)
            .collect::<Vec<_>>();
        self.run_values(entry, &args)?.constant().ok_or_else(|| {
            cx_log::error::CXErr::new(
                cx_log::error::message::CXStdErrMessage::error(
                    "COMPTIME ERROR",
                    "expected a concrete compile-time value",
                ),
                cx_log::error::context::CXInternalContext::error(
                    "staged value escaped a constant evaluation",
                ),
            )
        })
    }

    pub fn run_values(
        &mut self,
        entry: InterpretedFunction<'ctx>,
        args: &[MIRComptimeValue],
    ) -> CXResult<MIRComptimeValue> {
        self.push_frame(entry, args)?;
        self.run_top_frame()
    }

    fn push_frame(
        &mut self,
        code: InterpretedFunction<'ctx>,
        args: &[MIRComptimeValue],
    ) -> CXResult<()> {
        let mut frame = Frame::new(code);
        for (index, value) in args.iter().enumerate() {
            frame.cells.insert(
                MIRPlace::Parameter(MIRParameterID::new(index)),
                value.clone(),
            );
        }

        let entry_params = frame.code.block_params(frame.code.current_block()).to_vec();
        for (register, value) in entry_params.into_iter().zip(args.iter()) {
            frame.registers.insert(register, value.clone());
        }

        self.frames.push(frame);
        Ok(())
    }

    fn run_top_frame(&mut self) -> CXResult<MIRComptimeValue> {
        loop {
            self.steps += 1;
            if self.steps > self.limits.max_steps {
                return comptime_error(
                    TokenRange::internal(),
                    format!(
                        "comptime evaluation exceeded {} steps",
                        self.limits.max_steps
                    ),
                );
            }

            let (kind, range) = {
                let frame = self.frames.last_mut().expect("engine ran without a frame");
                match frame.code.next_instruction() {
                    Some(instruction) => {
                        (instruction.kind.clone(), instruction.token_range.clone())
                    }
                    None => {
                        return comptime_error(
                            TokenRange::internal(),
                            "block fell through without a terminating instruction",
                        );
                    }
                }
            };

            match kind {
                MIRInstrKind::ScopeEnter { .. } | MIRInstrKind::ScopeExit { .. } => {}
                MIRInstrKind::Initialize { .. } | MIRInstrKind::Leak { .. } => {}
                MIRInstrKind::Create { out, ty } => {
                    let frame = self.frames.last_mut().expect("active frame");
                    frame
                        .cells
                        .insert(out, MIRComptimeValue::Constant(MIRConstant::Undefined));
                    let _ = ty;
                }
                MIRInstrKind::Assign { target, value, ty } => {
                    let value = self.read_value(&value)?;
                    match target {
                        MIRAssignTarget::Register(register) => {
                            let frame = self.frames.last_mut().expect("active frame");
                            frame.registers.insert(register, value);
                        }
                        MIRAssignTarget::Place(place) => {
                            self.write_place(place, value, Some(ty))?;
                        }
                    }
                }
                MIRInstrKind::AddressOf { out, place } => {
                    let constant = self.address_of(place, &range)?;
                    let frame = self.frames.last_mut().expect("active frame");
                    frame
                        .registers
                        .insert(out, MIRComptimeValue::Constant(constant));
                }
                MIRInstrKind::Dereference { .. } => {
                    return comptime_error(
                        range,
                        "dereference is not supported in a comptime context yet",
                    );
                }
                MIRInstrKind::AggregateOp(op) => self.execute_aggregate_op(op)?,
                MIRInstrKind::Call {
                    out, callee, args, ..
                } => {
                    let callee_value = self.read_constant(&callee, &range)?;
                    let function_id = match callee_value {
                        MIRConstant::Function(id) => id,
                        other => {
                            return comptime_error(
                                range,
                                format!("cannot call non-function comptime value {other:?}"),
                            );
                        }
                    };
                    let mut arguments = Vec::with_capacity(args.len());
                    for argument in args {
                        arguments.push(self.read_value(&argument)?);
                    }
                    let result = self.call_function(function_id, &arguments)?;
                    if let Some(out) = out {
                        let frame = self.frames.last_mut().expect("active frame");
                        frame.registers.insert(out, result);
                    }
                }
                MIRInstrKind::VaStart { .. }
                | MIRInstrKind::VaEnd { .. }
                | MIRInstrKind::VaArg { .. } => {
                    return comptime_error(range, "variadic operations are not comptime-capable");
                }
                MIRInstrKind::BinOp { out, op, lhs, rhs } => {
                    let lhs = self.read_constant(&lhs, &range)?;
                    let rhs = self.read_constant(&rhs, &range)?;
                    let result = self.evaluate_binop(op, lhs, rhs)?;
                    let frame = self.frames.last_mut().expect("active frame");
                    frame
                        .registers
                        .insert(out, MIRComptimeValue::Constant(result));
                }
                MIRInstrKind::UnOp { out, op, operand } => {
                    if let MIRUnaryOp::Increment { amount, post } = op {
                        let old = self.read_constant(&operand, &range)?;
                        let updated = increment_constant(old.clone(), amount)?;
                        match operand {
                            MIRValue::Register(register) => {
                                let frame = self.frames.last_mut().expect("active frame");
                                frame
                                    .registers
                                    .insert(register, MIRComptimeValue::Constant(updated.clone()));
                            }
                            MIRValue::PlaceRef(place)
                            | MIRValue::Copy(place)
                            | MIRValue::Move(place) => {
                                self.write_direct_cell(
                                    place,
                                    MIRComptimeValue::Constant(updated.clone()),
                                )?;
                            }
                            MIRValue::Constant(_) => {}
                        }
                        let exposed = if post { old } else { updated };
                        let frame = self.frames.last_mut().expect("active frame");
                        frame
                            .registers
                            .insert(out, MIRComptimeValue::Constant(exposed));
                    } else {
                        let operand = self.read_constant(&operand, &range)?;
                        let result = self.evaluate_unop(op, operand)?;
                        let frame = self.frames.last_mut().expect("active frame");
                        frame
                            .registers
                            .insert(out, MIRComptimeValue::Constant(result));
                    }
                }
                MIRInstrKind::Coerce {
                    out,
                    operand,
                    coercion,
                    to_type,
                } => {
                    let result = if matches!(
                        coercion,
                        MIRCoercion::TypeChange | MIRCoercion::ReinterpretBits
                    ) {
                        match self.coerce_global_special(&operand, to_type)? {
                            Some(constant) => constant,
                            None => {
                                let operand = self.read_constant(&operand, &range)?;
                                self.evaluate_coercion(coercion, operand, to_type)?
                            }
                        }
                    } else {
                        let operand = self.read_constant(&operand, &range)?;
                        self.evaluate_coercion(coercion, operand, to_type)?
                    };
                    let frame = self.frames.last_mut().expect("active frame");
                    frame
                        .registers
                        .insert(out, MIRComptimeValue::Constant(result));
                }
                MIRInstrKind::Assert { condition, message } => {
                    let condition = self.read_constant(&condition, &range)?;
                    if !is_truthy(&condition) {
                        return comptime_error(
                            range,
                            message.unwrap_or_else(|| "assertion failed at compile time".into()),
                        );
                    }
                }
                MIRInstrKind::Assume { condition } => {
                    let _ = self.read_constant(&condition, &range)?;
                }
                MIRInstrKind::Return { value } => {
                    let constant = match value {
                        Some(value) => self.read_value(&value)?,
                        None => MIRComptimeValue::Constant(MIRConstant::Unit),
                    };
                    self.frames.pop();
                    return Ok(constant);
                }
                MIRInstrKind::Jump { target } => {
                    self.jump_to(target)?;
                }
                MIRInstrKind::Branch {
                    cond,
                    true_target,
                    false_target,
                } => {
                    let condition = self.read_constant(&cond, &range)?;
                    let target = if is_truthy(&condition) {
                        true_target
                    } else {
                        false_target
                    };
                    self.jump_to(target)?;
                }
                MIRInstrKind::IntSwitch {
                    value,
                    cases,
                    default,
                } => {
                    let subject = self.read_constant(&value, &range)?;
                    let mut taken = default;
                    for (case, target) in cases {
                        if constant_equals(&subject, &case) {
                            taken = Some(target.clone());
                            break;
                        }
                    }
                    match taken {
                        Some(target) => self.jump_to(target)?,
                        None => {
                            return comptime_error(range, "integer switch fell through all cases");
                        }
                    }
                }
                MIRInstrKind::VariantSwitch {
                    subject,
                    cases,
                    default,
                    ..
                } => {
                    let subject = self.read_constant(&subject, &range)?;
                    let discriminant = variant_discriminant(&subject);
                    let mut taken = default;
                    for (variant, target) in cases {
                        if discriminant == Some(variant) {
                            taken = Some(target.clone());
                            break;
                        }
                    }
                    match taken {
                        Some(target) => self.jump_to(target)?,
                        None => {
                            return comptime_error(range, "variant switch fell through all cases");
                        }
                    }
                }
                MIRInstrKind::Unreachable => {
                    return comptime_error(range, "unreachable code executed at compile time");
                }
                MIRInstrKind::MakeStaged {
                    out,
                    template,
                    captures,
                } => {
                    let mut bindings = Vec::with_capacity(captures.len());
                    for capture in captures {
                        bindings.push(MIRStagedBinding::Comptime(self.read_value(&capture)?));
                    }
                    let staged = MIRStagedValue::new(template, bindings, Vec::new(), None);
                    let frame = self.frames.last_mut().expect("active frame");
                    frame
                        .registers
                        .insert(out, MIRComptimeValue::Staged(std::sync::Arc::new(staged)));
                }
                MIRInstrKind::ApplyStaged { out, staged, args } => {
                    let MIRComptimeValue::Staged(staged) = self.read_value(&staged)? else {
                        return comptime_error(range, "attempted to apply a non-staged value");
                    };
                    let mut bindings = Vec::with_capacity(args.len());
                    for arg in args {
                        bindings.push(MIRStagedBinding::Comptime(self.read_value(&arg)?));
                    }
                    if let Some(out) = out {
                        let frame = self.frames.last_mut().expect("active frame");
                        frame.registers.insert(
                            out,
                            MIRComptimeValue::Staged(std::sync::Arc::new(staged.apply(bindings))),
                        );
                    }
                }
                MIRInstrKind::StagedReturn { .. } => {
                    return comptime_error(range, "staged template executed as a function");
                }
                MIRInstrKind::StagedMove { .. } => {
                    return comptime_error(range, "staged move executed as a function");
                }
                MIRInstrKind::StagedUse { .. } => {
                    return comptime_error(range, "staged use executed as a function");
                }
            }
        }
    }

    fn jump_to(&mut self, target: MIRBlockTarget) -> CXResult<()> {
        let params = {
            let frame = self.frames.last().expect("active frame");
            frame.code.block_params(target.block).to_vec()
        };

        let mut values = Vec::with_capacity(target.args.len());
        for argument in &target.args {
            values.push(self.read_value(&argument)?);
        }

        let frame = self.frames.last_mut().expect("active frame");
        for (register, value) in params.into_iter().zip(values.into_iter()) {
            frame.registers.insert(register, value);
        }
        frame.code.jump_to_block(target.block);
        Ok(())
    }

    fn call_function(
        &mut self,
        function_id: MIRFunctionID,
        args: &[MIRComptimeValue],
    ) -> CXResult<MIRComptimeValue> {
        if self.frames.len() >= self.limits.max_call_depth {
            return comptime_error(
                TokenRange::internal(),
                format!(
                    "comptime call depth exceeded {}",
                    self.limits.max_call_depth
                ),
            );
        }

        let resolver = self.resolver;
        let Some(function) = resolver.resolve(function_id) else {
            return comptime_error(
                TokenRange::internal(),
                format!("function {function_id:?} is not available during comptime evaluation"),
            );
        };

        match function.mode() {
            MIRFunctionMode::Runtime => {
                return comptime_error(
                    TokenRange::internal(),
                    "runtime functions cannot be executed at compile time",
                );
            }
            MIRFunctionMode::Constexpr | MIRFunctionMode::Comptime => {}
        }

        let Some(entry) = InterpretedFunction::new(function) else {
            return comptime_error(
                TokenRange::internal(),
                format!("function {function_id:?} has no definition to interpret"),
            );
        };

        self.push_frame(entry, args)?;
        self.run_top_frame()
    }

    fn execute_aggregate_op(&mut self, op: MIRAggregateOp) -> CXResult<()> {
        match op {
            MIRAggregateOp::Place { out, op } => {
                use cx_mir::MIRPlaceAggregateOp as Op;

                let (root, path) = match op {
                    Op::Field { base, field, .. } => {
                        let (root, mut path) = self.resolve_projection(base);
                        path.push(PathSeg::Field(field));
                        (root, path)
                    }
                    Op::Index { base, index, .. } => {
                        let index = self.read_constant(&index, &TokenRange::internal())?;
                        let index = match index {
                            MIRConstant::Integer { value, .. } => value,
                            other => {
                                return comptime_error(
                                    TokenRange::internal(),
                                    format!("array index is not an integer constant: {other:?}"),
                                );
                            }
                        };
                        let (root, mut path) = self.resolve_projection(base);
                        path.push(PathSeg::Index(index));
                        (root, path)
                    }
                    Op::Variant { base, variant, .. } => {
                        let (root, mut path) = self.resolve_projection(base);
                        path.push(PathSeg::Variant(variant));
                        (root, path)
                    }
                };

                let frame = self.frames.last_mut().expect("active frame");
                frame.derived.insert(out, (root, path));
                Ok(())
            }
            MIRAggregateOp::Value { out, op } => {
                use cx_mir::MIRValueAggregateOp as Op;

                let constant = match op {
                    Op::Discriminant { value, .. } => {
                        let value = self.read_constant(&value, &TokenRange::internal())?;
                        match variant_discriminant(&value) {
                            Some(discriminant) => MIRConstant::Integer {
                                value: discriminant as i128,
                                ty: MIRIntType::I64,
                                signed: false,
                            },
                            None => MIRConstant::Undefined,
                        }
                    }
                    Op::Construct { ty, fields } => {
                        let mut evaluated = Vec::with_capacity(fields.len());
                        for (index, field) in fields {
                            evaluated.push((
                                index,
                                self.read_constant(&field, &TokenRange::internal())?,
                            ));
                        }
                        MIRConstant::Aggregate {
                            ty,
                            fields: evaluated,
                        }
                    }
                    Op::Variant {
                        variant,
                        value,
                        sum_type,
                    } => MIRConstant::Aggregate {
                        ty: sum_type,
                        fields: vec![(
                            variant,
                            self.read_constant(&value, &TokenRange::internal())?,
                        )],
                    },
                    Op::ProjectVariant { variant, value, .. } => {
                        let value = self.read_constant(&value, &TokenRange::internal())?;
                        read_path(&value, &[PathSeg::Variant(variant)])
                    }
                };

                let frame = self.frames.last_mut().expect("active frame");
                frame
                    .registers
                    .insert(out, MIRComptimeValue::Constant(constant));
                Ok(())
            }
        }
    }

    fn resolve_projection(&self, place: MIRPlace) -> (MIRPlace, Vec<PathSeg>) {
        self.frames
            .last()
            .and_then(|frame| frame.derived.get(&place).cloned())
            .unwrap_or((place, Vec::new()))
    }

    fn coerce_global_special(
        &self,
        operand: &MIRValue,
        to_type: cx_mir::MIRTypeID,
    ) -> CXResult<Option<MIRConstant>> {
        let MIRValue::PlaceRef(MIRPlace::Global(global)) = operand else {
            return Ok(None);
        };
        let Some(registry) = self.resolver.types() else {
            return Ok(None);
        };
        let Ok(target_kind) = registry.kind(to_type) else {
            return Ok(None);
        };

        match self.resolver.global_kind(*global) {
            Some(MIRGlobalKind::Variable { ty, .. }) => {
                let decays = matches!(
                    target_kind,
                    MIRTypeKind::PointerTo { .. } | MIRTypeKind::MemoryReference { .. }
                ) && matches!(registry.kind(ty), Ok(MIRTypeKind::Array { .. }));
                if decays {
                    return Ok(Some(relocation_constant(*global, 0, ty)));
                }
                Ok(None)
            }
            Some(MIRGlobalKind::StringLiteral { value }) => {
                if let MIRTypeKind::Array { length, inner } = target_kind {
                    if let Ok(MIRTypeKind::Integer { ty, signed }) = registry.kind(*inner) {
                        if ty.bytes() == 1 {
                            let bytes = value.as_bytes();
                            let fields = (0..*length)
                                .map(|index| {
                                    let byte = bytes.get(index).copied().unwrap_or(0);
                                    (
                                        index,
                                        MIRConstant::Integer {
                                            value: byte as i128,
                                            ty: *ty,
                                            signed: *signed,
                                        },
                                    )
                                })
                                .collect();
                            return Ok(Some(MIRConstant::Aggregate {
                                ty: to_type,
                                fields,
                            }));
                        }
                    }
                }
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    fn address_of(&self, place: MIRPlace, range: &TokenRange) -> CXResult<MIRConstant> {
        let (root, path) = self.resolve_projection(place);
        let MIRPlace::Global(global) = root else {
            return comptime_error(
                range.clone(),
                "cannot take the address of a local value in a comptime context",
            );
        };

        if path.is_empty() {
            let ty = self.global_address_type(global, range)?;
            return Ok(relocation_constant(global, 0, ty));
        }

        let Some(registry) = self.resolver.types() else {
            return comptime_error(
                range.clone(),
                "type layouts are unavailable during comptime evaluation",
            );
        };
        let Some(MIRGlobalKind::Variable { ty: start, .. }) = self.resolver.global_kind(global)
        else {
            return comptime_error(
                range.clone(),
                "cannot project into this global in a comptime context",
            );
        };

        let mut offset: i64 = 0;
        let mut ty = start;
        for segment in &path {
            match segment {
                PathSeg::Field(index) => match field_layout(registry, ty, *index) {
                    Ok(MIRFieldLayout::Standard {
                        offset: field_offset,
                        ty: field_ty,
                    }) => {
                        offset += field_offset as i64;
                        ty = field_ty;
                    }
                    Ok(MIRFieldLayout::Bitfield { .. }) => {
                        return comptime_error(
                            range.clone(),
                            "address-of a bitfield is not supported in a comptime context",
                        );
                    }
                    Err(_) => {
                        return comptime_error(
                            range.clone(),
                            "invalid field projection in an address-of computation",
                        );
                    }
                },
                PathSeg::Index(index) => {
                    let inner = match registry.kind(ty) {
                        Ok(MIRTypeKind::Array { inner, .. }) => *inner,
                        _ => {
                            return comptime_error(
                                range.clone(),
                                "index projection on a non-array in an address-of computation",
                            );
                        }
                    };
                    if *index < 0 {
                        return comptime_error(
                            range.clone(),
                            "negative array index in an address-of computation",
                        );
                    }
                    let stride = match layout_of(registry, inner) {
                        Ok(layout) => layout.size as i64,
                        Err(_) => {
                            return comptime_error(
                                range.clone(),
                                "invalid element layout in an address-of computation",
                            );
                        }
                    };
                    offset += stride * *index as i64;
                    ty = inner;
                }
                PathSeg::Variant(_) => {
                    return comptime_error(
                        range.clone(),
                        "variant projections are not supported in address-of computations",
                    );
                }
            }
        }

        Ok(relocation_constant(global, offset, ty))
    }

    fn global_address_type(&self, global: MIRGlobalID, range: &TokenRange) -> CXResult<MIRTypeID> {
        match self.resolver.global_kind(global) {
            Some(MIRGlobalKind::Variable { ty, .. }) => Ok(ty),
            Some(MIRGlobalKind::StringLiteral { .. }) => {
                let Some(types) = self.resolver.types() else {
                    return comptime_error(
                        range.clone(),
                        "type layouts are unavailable during comptime evaluation",
                    );
                };
                let Some(ty) = types.find_kind(&MIRTypeKind::Str) else {
                    return comptime_error(
                        range.clone(),
                        "the string type is unavailable during comptime evaluation",
                    );
                };
                Ok(ty)
            }
            None => comptime_error(range.clone(), "unknown global in an address-of computation"),
        }
    }

    fn evaluate_binop(
        &mut self,
        op: MIRBinaryOp,
        lhs: MIRConstant,
        rhs: MIRConstant,
    ) -> CXResult<MIRConstant> {
        match op {
            MIRBinaryOp::Integer { ty, signed, op } => {
                let lhs = as_integer(&lhs);
                let rhs = as_integer(&rhs);
                self.integer_binop(ty, signed, op, lhs, rhs)
            }
            MIRBinaryOp::Float { ty, op } => {
                let lhs = as_float(&lhs);
                let rhs = as_float(&rhs);
                let result = match op {
                    MIRFloatBinaryOp::Add => lhs + rhs,
                    MIRFloatBinaryOp::Sub => lhs - rhs,
                    MIRFloatBinaryOp::Mul => lhs * rhs,
                    MIRFloatBinaryOp::Div => lhs / rhs,
                    MIRFloatBinaryOp::Eq => return Ok(MIRConstant::Bool(lhs == rhs)),
                    MIRFloatBinaryOp::Ne => return Ok(MIRConstant::Bool(lhs != rhs)),
                    MIRFloatBinaryOp::Lt => return Ok(MIRConstant::Bool(lhs < rhs)),
                    MIRFloatBinaryOp::Le => return Ok(MIRConstant::Bool(lhs <= rhs)),
                    MIRFloatBinaryOp::Gt => return Ok(MIRConstant::Bool(lhs > rhs)),
                    MIRFloatBinaryOp::Ge => return Ok(MIRConstant::Bool(lhs >= rhs)),
                };
                Ok(MIRConstant::Float {
                    value: FloatWrapper::from(result),
                    ty,
                })
            }
            MIRBinaryOp::PointerOffset { op, pointee } => {
                let (global, base_offset) = match &lhs {
                    MIRConstant::Global { global, .. } => (*global, 0i64),
                    MIRConstant::GlobalOffset { global, offset, .. } => (*global, *offset),
                    other => {
                        return comptime_error(
                            TokenRange::internal(),
                            format!("pointer arithmetic on a non-pointer constant: {other:?}"),
                        );
                    }
                };
                let count = as_integer(&rhs);
                let Ok(count) = i64::try_from(count) else {
                    return comptime_error(
                        TokenRange::internal(),
                        "pointer arithmetic overflowed during compile-time evaluation",
                    );
                };
                let Some(registry) = self.resolver.types() else {
                    return comptime_error(
                        TokenRange::internal(),
                        "type layouts are unavailable during comptime evaluation",
                    );
                };
                let stride = match layout_of(registry, pointee) {
                    Ok(layout) => layout.size as i64,
                    Err(_) => {
                        return comptime_error(
                            TokenRange::internal(),
                            "invalid pointee layout in pointer arithmetic",
                        );
                    }
                };
                let Some(delta) = count.checked_mul(stride) else {
                    return comptime_error(
                        TokenRange::internal(),
                        "pointer arithmetic overflowed during compile-time evaluation",
                    );
                };
                let offset = match op {
                    MIRPointerOffsetOp::Add => base_offset.checked_add(delta),
                    MIRPointerOffsetOp::Sub => base_offset.checked_sub(delta),
                };
                let Some(offset) = offset else {
                    return comptime_error(
                        TokenRange::internal(),
                        "pointer arithmetic overflowed during compile-time evaluation",
                    );
                };
                Ok(relocation_constant(global, offset, pointee))
            }
            MIRBinaryOp::Pointer(op) => {
                let equal = pointer_constants_equal(&lhs, &rhs)?;
                let result = match op {
                    MIRPointerBinaryOp::Eq => equal,
                    MIRPointerBinaryOp::Ne => !equal,
                    MIRPointerBinaryOp::Lt
                    | MIRPointerBinaryOp::Le
                    | MIRPointerBinaryOp::Gt
                    | MIRPointerBinaryOp::Ge => {
                        return comptime_error(
                            TokenRange::internal(),
                            "ordered pointer comparisons are not supported in a comptime context yet",
                        );
                    }
                };
                Ok(MIRConstant::Bool(result))
            }
        }
    }

    fn integer_binop(
        &self,
        ty: MIRIntType,
        signed: bool,
        op: MIRIntBinaryOp,
        lhs: i128,
        rhs: i128,
    ) -> CXResult<MIRConstant> {
        use MIRIntBinaryOp as Op;

        let int = |value: i128| MIRConstant::Integer { value, ty, signed };
        let boolean = |value: bool| MIRConstant::Bool(value);

        Ok(match op {
            Op::Add => int(lhs.wrapping_add(rhs)),
            Op::Sub => int(lhs.wrapping_sub(rhs)),
            Op::Mul | Op::SignedMul => int(lhs.wrapping_mul(rhs)),
            Op::Div | Op::SignedDiv => {
                if rhs == 0 {
                    return comptime_error(
                        TokenRange::internal(),
                        "division by zero during compile-time evaluation",
                    );
                }
                int(lhs.wrapping_div(rhs))
            }
            Op::Mod | Op::SignedMod => {
                if rhs == 0 {
                    return comptime_error(
                        TokenRange::internal(),
                        "remainder by zero during compile-time evaluation",
                    );
                }
                int(lhs.wrapping_rem(rhs))
            }
            Op::Eq => boolean(lhs == rhs),
            Op::Ne => boolean(lhs != rhs),
            Op::Lt | Op::SignedLt => boolean(if signed || matches!(op, Op::SignedLt) {
                lhs < rhs
            } else {
                (lhs as u128) < (rhs as u128)
            }),
            Op::Le | Op::SignedLe => boolean(if signed || matches!(op, Op::SignedLe) {
                lhs <= rhs
            } else {
                (lhs as u128) <= (rhs as u128)
            }),
            Op::Gt | Op::SignedGt => boolean(if signed || matches!(op, Op::SignedGt) {
                lhs > rhs
            } else {
                (lhs as u128) > (rhs as u128)
            }),
            Op::Ge | Op::SignedGe => boolean(if signed || matches!(op, Op::SignedGe) {
                lhs >= rhs
            } else {
                (lhs as u128) >= (rhs as u128)
            }),
            Op::LogicalAnd => boolean(is_truthy(&int(lhs)) && is_truthy(&int(rhs))),
            Op::LogicalOr => boolean(is_truthy(&int(lhs)) || is_truthy(&int(rhs))),
            Op::BitAnd => int(lhs & rhs),
            Op::BitOr => int(lhs | rhs),
            Op::BitXor => int(lhs ^ rhs),
            Op::ShiftLeft => int(width_masked(lhs << (rhs & 127), ty)),
            Op::ArithmeticShiftRight => int(lhs >> (rhs & 127)),
            Op::LogicalShiftRight => int(((lhs as u128) >> (rhs & 127)) as i128),
        })
    }

    fn evaluate_unop(&mut self, op: MIRUnaryOp, operand: MIRConstant) -> CXResult<MIRConstant> {
        Ok(match op {
            MIRUnaryOp::IntegerNeg { ty, signed } => {
                let value = as_integer(&operand).wrapping_neg();
                MIRConstant::Integer { value, ty, signed }
            }
            MIRUnaryOp::FloatNeg(ty) => MIRConstant::Float {
                value: FloatWrapper::from(-as_float(&operand)),
                ty,
            },
            MIRUnaryOp::BitNot(ty) => MIRConstant::Integer {
                value: width_masked(!as_integer(&operand), ty),
                ty,
                signed: false,
            },
            MIRUnaryOp::LogicalNot => MIRConstant::Bool(!is_truthy(&operand)),
            MIRUnaryOp::Increment { .. } => {
                return comptime_error(
                    TokenRange::internal(),
                    "increment is handled by the execution loop",
                );
            }
        })
    }

    fn evaluate_coercion(
        &self,
        coercion: MIRCoercion,
        operand: MIRConstant,
        to_type: cx_mir::MIRTypeID,
    ) -> CXResult<MIRConstant> {
        Ok(match coercion {
            MIRCoercion::Integral {
                sign_extend,
                from,
                to,
            } => {
                let _ = from;
                let source_signed = matches!(operand, MIRConstant::Integer { signed: true, .. });
                let raw = as_integer(&operand);
                let bits = to.bytes() as u32 * 8;
                let mut value = width_masked(raw, to);
                if sign_extend && source_signed && bits > 0 && bits < 128 {
                    let shift = 128 - bits;
                    value = (value << shift) >> shift;
                }
                MIRConstant::Integer {
                    value,
                    ty: to,
                    signed: source_signed,
                }
            }
            MIRCoercion::FloatCast { to, .. } => MIRConstant::Float {
                value: FloatWrapper::from(as_float(&operand)),
                ty: to,
            },
            MIRCoercion::IntToFloat { signed, to, .. } => {
                let raw = as_integer(&operand);
                let value = if signed {
                    raw as f64
                } else {
                    (raw as u128) as f64
                };
                MIRConstant::Float {
                    value: FloatWrapper::from(value),
                    ty: to,
                }
            }
            MIRCoercion::FloatToInt { signed, to, .. } => {
                let value = as_float(&operand) as i128;
                MIRConstant::Integer {
                    value: width_masked(value, to),
                    ty: to,
                    signed,
                }
            }
            MIRCoercion::PointerToInt { .. } => match &operand {
                MIRConstant::Null { .. } => MIRConstant::Integer {
                    value: 0,
                    ty: MIRIntType::I64,
                    signed: false,
                },
                _ => {
                    return comptime_error(
                        TokenRange::internal(),
                        "pointer-to-integer coercions are not supported in a comptime context yet",
                    );
                }
            },
            MIRCoercion::IntToPointer { .. } => {
                let is_null = matches!(
                    operand,
                    MIRConstant::Integer { value: 0, .. } | MIRConstant::Bool(false)
                );
                if is_null {
                    MIRConstant::Null { ty: to_type }
                } else {
                    return comptime_error(
                        TokenRange::internal(),
                        "non-null pointer coercions are not supported in a comptime context yet",
                    );
                }
            }
            MIRCoercion::FunctionToPointer => match &operand {
                MIRConstant::Function(_) | MIRConstant::Null { .. } => operand.clone(),
                _ => {
                    return comptime_error(
                        TokenRange::internal(),
                        "cannot coerce a non-function constant to a pointer in a comptime context",
                    );
                }
            },
            MIRCoercion::TypeChange | MIRCoercion::ReinterpretBits => operand,
        })
    }

    fn read_value(&mut self, value: &MIRValue) -> CXResult<MIRComptimeValue> {
        Ok(match value {
            MIRValue::Constant(constant) => MIRComptimeValue::Constant(constant.clone()),
            MIRValue::Register(register) => self
                .frames
                .last()
                .and_then(|frame| frame.registers.get(register))
                .cloned()
                .unwrap_or(MIRComptimeValue::Constant(MIRConstant::Undefined)),
            MIRValue::PlaceRef(place) | MIRValue::Copy(place) | MIRValue::Move(place) => {
                if let MIRPlace::Global(global) = place {
                    MIRComptimeValue::Constant(self.read_global_rvalue(*global)?)
                } else {
                    self.read_place(*place)?
                }
            }
        })
    }

    fn read_constant(&mut self, value: &MIRValue, range: &TokenRange) -> CXResult<MIRConstant> {
        match self.read_value(value)? {
            MIRComptimeValue::Constant(value) => Ok(value),
            MIRComptimeValue::Staged(_) => {
                comptime_error(range.clone(), "staged value used as a concrete value")
            }
        }
    }

    fn read_global_rvalue(&mut self, global: MIRGlobalID) -> CXResult<MIRConstant> {
        if let Some(MIRGlobalKind::Variable { ty, .. }) = self.resolver.global_kind(global)
            && let Some(registry) = self.resolver.types()
            && let Ok(MIRTypeKind::Array { inner, .. }) = registry.kind(ty)
        {
            return Ok(relocation_constant(global, 0, *inner));
        }
        self.read_global(global)
    }

    fn read_place(&mut self, place: MIRPlace) -> CXResult<MIRComptimeValue> {
        if let MIRPlace::Global(global) = place {
            return Ok(MIRComptimeValue::Constant(self.read_global(global)?));
        }

        let projection = self.resolve_projection(place);
        if projection.1.is_empty() {
            return Ok(self
                .frames
                .last()
                .and_then(|frame| frame.cells.get(&place))
                .cloned()
                .unwrap_or(MIRComptimeValue::Constant(MIRConstant::Undefined)));
        }

        let root = match &projection.0 {
            MIRPlace::Global(global) => MIRComptimeValue::Constant(self.read_global(*global)?),
            other => self
                .frames
                .last()
                .and_then(|frame| frame.cells.get(other))
                .cloned()
                .unwrap_or(MIRComptimeValue::Constant(MIRConstant::Undefined)),
        };
        let MIRComptimeValue::Constant(root) = root else {
            return comptime_error(
                TokenRange::internal(),
                "cannot project through a staged value",
            );
        };
        Ok(MIRComptimeValue::Constant(read_path(&root, &projection.1)))
    }

    fn write_place(
        &mut self,
        place: MIRPlace,
        value: MIRComptimeValue,
        aggregate_type: Option<cx_mir::MIRTypeID>,
    ) -> CXResult<()> {
        if let MIRPlace::Global(global) = place {
            let MIRComptimeValue::Constant(value) = value else {
                return comptime_error(
                    TokenRange::internal(),
                    "cannot store a staged value in a global",
                );
            };
            self.globals.insert(global, value);
            return Ok(());
        }

        let projection = self.resolve_projection(place);
        if projection.1.is_empty() {
            self.write_direct_cell(place, value)?;
            return Ok(());
        }

        let (root, path) = projection;
        let current = match &root {
            MIRPlace::Global(global) => MIRComptimeValue::Constant(self.read_global(*global)?),
            other => self
                .frames
                .last()
                .and_then(|frame| frame.cells.get(other))
                .cloned()
                .unwrap_or(MIRComptimeValue::Constant(MIRConstant::Undefined)),
        };
        let MIRComptimeValue::Constant(current) = current else {
            return comptime_error(
                TokenRange::internal(),
                "cannot assign through a staged value",
            );
        };
        let MIRComptimeValue::Constant(value) = value else {
            return comptime_error(
                TokenRange::internal(),
                "cannot store a staged value in an aggregate projection",
            );
        };
        let updated = write_path(&current, &path, value, aggregate_type);
        match root {
            MIRPlace::Global(global) => {
                self.globals.insert(global, updated);
            }
            other => {
                let frame = self.frames.last_mut().expect("active frame");
                frame
                    .cells
                    .insert(other, MIRComptimeValue::Constant(updated));
            }
        }
        Ok(())
    }

    fn write_direct_cell(&mut self, place: MIRPlace, value: MIRComptimeValue) -> CXResult<()> {
        debug_assert!(
            !matches!(place, MIRPlace::Global(_)),
            "globals are handled by write_place"
        );
        let frame = self.frames.last_mut().expect("active frame");
        frame.cells.insert(place, value);
        Ok(())
    }

    fn read_global(&mut self, global: MIRGlobalID) -> CXResult<MIRConstant> {
        if let Some(cached) = self.globals.get(&global) {
            return Ok(cached.clone());
        }
        if !self.evaluating_globals.insert(global) {
            return comptime_error(
                TokenRange::internal(),
                "cyclic dependency between global initializers",
            );
        }

        let result = (|| {
            let resolver = self.resolver;
            if let Some(constant) = resolver.global_constant(global) {
                return Ok(constant);
            }
            if let Some(initializer) = resolver.global_initializer(global) {
                return match self.call_function(initializer, &[])? {
                    MIRComptimeValue::Constant(value) => Ok(value),
                    MIRComptimeValue::Staged(_) => comptime_error(
                        TokenRange::internal(),
                        "global initializer returned a staged value",
                    ),
                };
            }
            if matches!(
                resolver.global_kind(global),
                Some(MIRGlobalKind::StringLiteral { .. })
            ) {
                let range = TokenRange::internal();
                let ty = self.global_address_type(global, &range)?;
                return Ok(relocation_constant(global, 0, ty));
            }
            comptime_error(
                TokenRange::internal(),
                "global is not available during comptime evaluation",
            )
        })();

        self.evaluating_globals.remove(&global);

        let constant = result?;
        self.globals.insert(global, constant.clone());
        Ok(constant)
    }
}

fn relocation_constant(global: MIRGlobalID, offset: i64, ty: MIRTypeID) -> MIRConstant {
    if offset == 0 {
        MIRConstant::Global { global, ty }
    } else {
        MIRConstant::GlobalOffset { global, offset, ty }
    }
}

fn pointer_constants_equal(lhs: &MIRConstant, rhs: &MIRConstant) -> CXResult<bool> {
    let as_address = |constant: &MIRConstant| match constant {
        MIRConstant::Null { .. } => Some(None),
        MIRConstant::Global { global, .. } => Some(Some((*global, 0i64))),
        MIRConstant::GlobalOffset { global, offset, .. } => Some(Some((*global, *offset))),
        _ => None,
    };

    let (lhs_address, rhs_address) = match (as_address(lhs), as_address(rhs)) {
        (Some(lhs), Some(rhs)) => (lhs, rhs),
        _ => {
            return comptime_error(
                TokenRange::internal(),
                format!("comparison of non-pointer constants {lhs:?} and {rhs:?}"),
            );
        }
    };

    Ok(match (lhs_address, rhs_address) {
        (None, None) => true,
        (None, Some(_)) | (Some(_), None) => false,
        (Some(lhs), Some(rhs)) => lhs == rhs,
    })
}

fn is_truthy(constant: &MIRConstant) -> bool {
    match constant {
        MIRConstant::Bool(value) => *value,
        MIRConstant::Integer { value, .. } => *value != 0,
        MIRConstant::Null { .. } | MIRConstant::Undefined => false,
        _ => true,
    }
}

fn constant_equals(lhs: &MIRConstant, rhs: &MIRConstant) -> bool {
    match (lhs, rhs) {
        (MIRConstant::Integer { value: l, .. }, MIRConstant::Integer { value: r, .. }) => l == r,
        _ => lhs == rhs,
    }
}

fn as_integer(constant: &MIRConstant) -> i128 {
    match constant {
        MIRConstant::Integer { value, .. } => *value,
        MIRConstant::Bool(value) => *value as i128,
        _ => 0,
    }
}

fn as_float(constant: &MIRConstant) -> f64 {
    match constant {
        MIRConstant::Float { value, .. } => f64::from(value),
        MIRConstant::Integer { value, .. } => *value as f64,
        _ => 0.0,
    }
}

fn variant_discriminant(constant: &MIRConstant) -> Option<usize> {
    match constant {
        MIRConstant::Aggregate { fields, .. } => fields.first().map(|(index, _)| *index),
        _ => None,
    }
}

fn read_path(root: &MIRConstant, path: &[PathSeg]) -> MIRConstant {
    let mut current = root.clone();
    for segment in path {
        let fields = match &current {
            MIRConstant::Aggregate { fields, .. } => fields.clone(),
            _ => return MIRConstant::Undefined,
        };
        let key = segment.key();
        current = fields
            .iter()
            .find(|(index, _)| *index == key)
            .map(|(_, value)| value.clone())
            .unwrap_or(MIRConstant::Undefined);
    }
    current
}

fn write_path(
    root: &MIRConstant,
    path: &[PathSeg],
    value: MIRConstant,
    aggregate_type: Option<cx_mir::MIRTypeID>,
) -> MIRConstant {
    let Some((head, tail)) = path.split_first() else {
        return value;
    };

    let key = head.key();

    let (ty, mut fields) = match root {
        MIRConstant::Aggregate { ty, fields } => (*ty, fields.clone()),
        _ => match aggregate_type {
            Some(ty) => (ty, Vec::new()),
            None => {
                return MIRConstant::Undefined;
            }
        },
    };

    match fields.iter().position(|(index, _)| *index == key) {
        Some(position) => {
            if tail.is_empty() {
                fields[position] = (key, value);
            } else {
                let child = fields[position].1.clone();
                let child = write_path(&child, tail, value, Some(ty));
                fields[position] = (key, child);
            }
        }
        None => {
            if tail.is_empty() {
                fields.push((key, value));
            } else {
                let child = write_path(&MIRConstant::Undefined, tail, value, Some(ty));
                fields.push((key, child));
            }
        }
    }

    MIRConstant::Aggregate { ty, fields }
}

fn increment_constant(constant: MIRConstant, amount: i8) -> CXResult<MIRConstant> {
    match constant {
        MIRConstant::Integer { value, ty, signed } => Ok(MIRConstant::Integer {
            value: value.wrapping_add(amount as i128),
            ty,
            signed,
        }),
        other => comptime_error(
            TokenRange::internal(),
            format!("increment applied to non-integer constant {other:?}"),
        ),
    }
}

fn width_masked(value: i128, ty: MIRIntType) -> i128 {
    let bits = ty.bytes() as u32 * 8;
    if bits == 0 || bits >= 128 {
        value
    } else {
        value & ((1i128 << bits) - 1)
    }
}
