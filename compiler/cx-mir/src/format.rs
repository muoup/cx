use std::fmt::{self, Display, Formatter};

use cx_util::linkage::LinkageMode;

use crate::{
    MIRInstructionLike,
    constant::MIRRuntimeConstant,
    expr::{
        body::MIRBody,
        comptime::{MIRComptimeInstruction, MIRComptimeOp},
        instruction::{MIRBasicBlock, MIRInstructionKind},
        intrinsic::{
            MIRAggregateIntrinsic, MIRFloatIntrinsic, MIRIntIntrinsic, MIRInternalIntrinsic,
            MIRIntrinsic, MIRPtrIntrinsic, MIRVAIntrinsic,
        },
    },
    ty::{MIRField, MIRFloatType, MIRIntType, MIRTypeID, MIRTypeKind, interface::MTRegistry},
    unit::{
        MIRGlobalID, MIRGlobalState, MIRGlobalVariable, MIRUnit,
        function::{MIRFnSignature, MIRFunction},
    },
    value::{
        MIRBindable, MIRBlockTarget, MIRConstant, MIRPlaceID, MIRRegisterID, MIRTarget, MIRValue,
    },
};

impl Display for MIRBlockTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.block, f)?;
        if self.args.is_empty() {
            return Ok(());
        }
        f.write_str("(")?;
        write_plain_values(f, &self.args)?;
        f.write_str(")")
    }
}

impl Display for MIRConstant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => f.write_str("()"),
            Self::Integer { value, ty } => write!(f, "{value}:{ty:?}"),
            Self::Float { value, ty } => write!(f, "{value}:{ty:?}"),
            Self::Nullptr { .. } => f.write_str("null"),
            Self::Aggregate { fields, .. } => {
                f.write_str("{")?;
                for (index, (field, value)) in fields.iter().enumerate() {
                    if index != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{field}: {value}")?;
                }
                f.write_str("}")
            }
            Self::String(value) => write!(f, "{value:?}"),
            Self::Global { global, offset, .. } => {
                write!(f, "global {global}")?;
                if *offset != 0 {
                    write!(f, " + {offset}")?;
                }
                Ok(())
            }
            Self::Function(function) => write!(f, "fn {function}"),
            Self::Staged(staged) => write!(f, "staged {staged}"),
            Self::RuntimeValue(value) => Display::fmt(value, f),
            Self::Undefined => f.write_str("undefined"),
        }
    }
}

impl Display for MIRRuntimeConstant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Register(value) => Display::fmt(value, f),
            Self::Place(value) => Display::fmt(value, f),
        }
    }
}

impl Display for MIRValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Register(value) => Display::fmt(value, f),
            Self::PlaceRef(value) => Display::fmt(value, f),
            Self::Global(value) => Display::fmt(value, f),
            Self::Constant(value) => Display::fmt(value, f),
        }
    }
}

impl Display for MIRFnSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("fn (")?;
        for (index, parameter) in self.params().iter().enumerate() {
            if index != 0 {
                f.write_str(", ")?;
            }
            if let Some(name) = parameter.name() {
                write!(f, "{name}: ")?;
            }
            Display::fmt(&parameter.ty(), f)?;
        }
        if self.variadic() {
            if !self.params().is_empty() {
                f.write_str(", ")?;
            }
            f.write_str("...")?;
        }
        write!(f, ") -> {}", self.return_type())
    }
}

impl Display for MIRGlobalState {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::External => f.write_str("external"),
            Self::ZeroInitialized => f.write_str("zero"),
            Self::Initialized(value) => Display::fmt(value, f),
        }
    }
}

impl Display for MIRUnit<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.display_pretty(), f)
    }
}

fn write_plain_values(f: &mut Formatter<'_>, values: &[MIRValue]) -> fmt::Result {
    for (index, value) in values.iter().enumerate() {
        if index != 0 {
            f.write_str(", ")?;
        }
        Display::fmt(value, f)?;
    }
    Ok(())
}

pub struct MIRDisplay<'a, 'thir> {
    unit: &'a MIRUnit<'thir>,
}

impl<'thir> MIRUnit<'thir> {
    pub fn display_pretty(&self) -> MIRDisplay<'_, 'thir> {
        MIRDisplay { unit: self }
    }
}

impl Display for MIRDisplay<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut types = TypePrinter::new(self.unit.types());

        for (i, global) in self.unit.globals().enumerate() {
            if i != 0 {
                f.write_str("\n")?;
            }
            write_global(f, self.unit, global, &mut types)?;
        }

        for (i, function) in self.unit.functions().enumerate() {
            if i != 0 {
                f.write_str("\n")?;
            }
            write_function(f, self.unit, function, &mut types)?;
        }

        Ok(())
    }
}

pub(crate) struct TypePrinter<'a, T: MTRegistry + Sized> {
    registry: &'a T,
    active: Vec<MIRTypeID>,
}

impl<'a, T: MTRegistry + Sized> TypePrinter<'a, T> {
    pub(crate) fn new(registry: &'a T) -> Self {
        Self {
            registry,
            active: Vec::new(),
        }
    }

    pub(crate) fn write(&mut self, f: &mut Formatter<'_>, id: MIRTypeID) -> fmt::Result {
        if self.active.contains(&id) {
            return write!(f, "t{}", id.index());
        }

        let Some(definition) = self.registry.definition(id) else {
            return write!(f, "<invalid t{}>", id.index());
        };

        if is_aggregate(definition.kind())
            && let Some(name) = self.registry.debug_name(id)
        {
            return f.write_str(name);
        }

        let kind = definition.kind().clone();
        self.active.push(id);
        let result = self.write_kind(f, &kind);
        self.active.pop();
        result
    }

    fn write_kind(&mut self, f: &mut Formatter<'_>, kind: &MIRTypeKind) -> fmt::Result {
        match kind {
            MIRTypeKind::Void => f.write_str("void"),
            MIRTypeKind::Integer { ty, signed } => {
                write!(
                    f,
                    "{}{}",
                    if *signed { 'i' } else { 'u' },
                    integer_width(*ty)
                )
            }
            MIRTypeKind::Float { ty } => write!(f, "f{}", float_width(*ty)),
            MIRTypeKind::Str => f.write_str("str"),
            MIRTypeKind::PointerTo { inner } => {
                f.write_str("*")?;
                self.write(f, *inner)
            }
            MIRTypeKind::MemoryReference { inner, .. } => {
                f.write_str("&")?;
                self.write(f, *inner)
            }
            MIRTypeKind::Array { inner, length } => {
                f.write_str("[")?;
                self.write(f, *inner)?;
                write!(f, "; {length}]")
            }
            MIRTypeKind::Structured { fields } => {
                f.write_str("struct {")?;
                self.write_fields(f, fields)?;
                f.write_str("}")
            }
            MIRTypeKind::Union { variants } => {
                f.write_str("union {")?;
                self.write_fields(f, variants)?;
                f.write_str("}")
            }
            MIRTypeKind::TaggedUnion { variants } => {
                f.write_str("tagged union {")?;
                self.write_fields(f, variants)?;
                f.write_str("}")
            }
            MIRTypeKind::Function { signature } => {
                f.write_str("fn(")?;
                for (index, parameter) in signature.params().iter().enumerate() {
                    if index != 0 {
                        f.write_str(", ")?;
                    }
                    self.write(f, parameter.ty())?;
                }
                f.write_str(") -> ")?;
                self.write(f, signature.return_type())
            }
            MIRTypeKind::Opaque { size, .. } => write!(f, "opaque[{size} bytes]"),
            MIRTypeKind::Undefined => f.write_str("undefined"),
        }
    }

    fn write_fields(&mut self, f: &mut Formatter<'_>, fields: &[MIRField]) -> fmt::Result {
        for (index, field) in fields.iter().enumerate() {
            if index != 0 {
                f.write_str(", ")?;
            }
            if let Some(name) = field.name() {
                write!(f, "{name}: ")?;
            } else {
                write!(f, "field_{index}: ")?;
            }
            self.write(f, field.ty())?;
            if let MIRField::Bitfield { width, .. } = field {
                write!(f, ":{width}")?;
            }
        }
        Ok(())
    }

    fn write_member_name(
        &self,
        f: &mut Formatter<'_>,
        aggregate_type: MIRTypeID,
        index: usize,
        prefix: &str,
    ) -> fmt::Result {
        if let Some(name) = self
            .registry
            .definition(aggregate_type)
            .and_then(|definition| aggregate_fields(definition.kind()))
            .and_then(|fields| fields.get(index))
            .and_then(MIRField::name)
        {
            return f.write_str(name);
        }
        write!(f, "{prefix}_{index}")
    }
}

fn is_aggregate(kind: &MIRTypeKind) -> bool {
    matches!(
        kind,
        MIRTypeKind::Structured { .. }
            | MIRTypeKind::Union { .. }
            | MIRTypeKind::TaggedUnion { .. }
    )
}

fn aggregate_fields(kind: &MIRTypeKind) -> Option<&[MIRField]> {
    match kind {
        MIRTypeKind::Structured { fields }
        | MIRTypeKind::Union { variants: fields }
        | MIRTypeKind::TaggedUnion { variants: fields } => Some(fields),
        _ => None,
    }
}

fn write_global<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    global: &MIRGlobalVariable,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    match global.linkage() {
        LinkageMode::Extern => f.write_str("extern ")?,
        LinkageMode::Static => f.write_str("static ")?,
        LinkageMode::Standard => {}
    }
    write!(f, "@{}: ", global.name())?;

    if !global.is_mutable() {
        f.write_str("const ")?;
    }
    types.write(f, global.ty())?;
    match global.state() {
        crate::unit::MIRGlobalState::External => f.write_str(";")?,
        crate::unit::MIRGlobalState::ZeroInitialized => f.write_str(" = zero;")?,
        crate::unit::MIRGlobalState::Initialized(value) => {
            f.write_str(" = ")?;
            Display::fmt(value, f)?;
            f.write_str(";")?;
        }
    }

    writeln!(f)
}

fn write_function<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    write!(f, "fn @{}(", function.prototype().display_name())?;
    for (index, parameter) in function.prototype().signature.params().iter().enumerate() {
        if index != 0 {
            f.write_str(", ")?;
        }
        if let Some(name) = parameter.name() {
            write!(f, "%{name}: ")?;
        } else {
            write!(f, "%arg{index}: ")?;
        }
        types.write(f, parameter.ty())?;
    }
    if function.prototype().signature.variadic() {
        if !function.prototype().signature.params().is_empty() {
            f.write_str(", ")?;
        }
        f.write_str("...")?;
    }
    f.write_str(") -> ")?;
    types.write(f, function.prototype().signature.return_type())?;
    write!(f, " /* {} */", function.prototype().symbol_name)?;

    match function.body() {
        Some(body) => write_body(f, unit, function, body, types, write_instruction),
        None => f.write_str(";"),
    }
}

fn write_body<T: MTRegistry, K: MIRInstructionLike>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    body: &MIRBody<K>,
    types: &mut TypePrinter<'_, T>,
    write_kind: impl Fn(
        &mut Formatter<'_>,
        &MIRUnit,
        &MIRFunction,
        &K,
        &mut TypePrinter<'_, T>,
    ) -> fmt::Result,
) -> fmt::Result {
    f.write_str(" {\n")?;
    for place in body.places() {
        f.write_str("    let ")?;
        write_place_name(f, unit, function, place.id)?;
        f.write_str(": ")?;
        types.write(f, place.ty)?;
        f.write_str(";\n")?;
    }
    for register in body.registers() {
        f.write_str("    let ")?;
        write_register_name(f, function, register.id)?;
        f.write_str(": ")?;
        types.write(f, register.ty)?;
        f.write_str(";\n")?;
    }
    for block in body.blocks() {
        write_block(f, unit, function, block, types, &write_kind)?;
    }
    f.write_str("}")
}

fn write_block<T: MTRegistry, K>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    block: &MIRBasicBlock<K>,
    types: &mut TypePrinter<'_, T>,
    write_kind: &impl Fn(
        &mut Formatter<'_>,
        &MIRUnit,
        &MIRFunction,
        &K,
        &mut TypePrinter<'_, T>,
    ) -> fmt::Result,
) -> fmt::Result {
    write!(f, "    {}", block.id())?;
    if !block.params().is_empty() {
        f.write_str("(")?;
        for (index, parameter) in block.params().iter().enumerate() {
            if index != 0 {
                f.write_str(", ")?;
            }
            write_register_name(f, function, *parameter)?;
        }
        f.write_str(")")?;
    }
    if let Some(name) = &block.debug_name() {
        write!(f, " /* {name} */")?;
    }
    f.write_str(":\n")?;
    for instruction in block.instructions() {
        f.write_str("        ")?;
        write_kind(f, unit, function, instruction, types)?;
        f.write_str(";\n")?;
    }
    Ok(())
}

fn write_instruction<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    instruction: &crate::expr::instruction::MIRInstruction,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    match &instruction.kind {
        MIRInstructionKind::ScopeEnter { scope } => write!(f, "scope.enter {scope}"),
        MIRInstructionKind::ScopeExit { scope } => write!(f, "scope.exit {scope}"),
        MIRInstructionKind::Initialize { place } => {
            f.write_str("initialize ")?;
            write_bindable(f, unit, function, place)
        }
        MIRInstructionKind::Invalidate { place, leak } => {
            if *leak {
                f.write_str("leak ")?;
            } else {
                f.write_str("invalidate ")?;
            }
            write_bindable(f, unit, function, place)
        }
        MIRInstructionKind::LiftPlace { out, place } => {
            write_register_name(f, function, *out)?;
            f.write_str(" = lift ")?;
            write_place_name(f, unit, function, *place)
        }
        MIRInstructionKind::BindLifetime { bind, bind_to: to } => {
            f.write_str("bind ")?;
            write_bindable(f, unit, function, bind)?;
            f.write_str(" to ")?;
            write_place_name(f, unit, function, *to)
        }
        MIRInstructionKind::Store { target, value, .. } => {
            write_place_name(f, unit, function, *target)?;
            f.write_str(" = ")?;
            write_value(f, unit, function, value)
        }
        MIRInstructionKind::Call { out, callee, args } => {
            if let Some(out) = out {
                write_register_name(f, function, *out)?;
                f.write_str(" = ")?;
            }
            write_value(f, unit, function, callee)?;
            f.write_str("(")?;
            write_values(f, unit, function, args)?;
            f.write_str(")")
        }
        MIRInstructionKind::IntrinsicOp(intrinsic) => {
            write_intrinsic(f, unit, function, intrinsic, types)
        }
        MIRInstructionKind::Return { value } => {
            f.write_str("return")?;
            if let Some(value) = value {
                f.write_str(" ")?;
                write_value(f, unit, function, value)?;
            }
            Ok(())
        }
        MIRInstructionKind::Jump { target } => {
            f.write_str("goto ")?;
            write_block_target(f, unit, function, target)
        }
        MIRInstructionKind::Branch {
            cond,
            true_target,
            false_target,
        } => {
            f.write_str("if ")?;
            write_value(f, unit, function, cond)?;
            f.write_str(" goto ")?;
            write_block_target(f, unit, function, true_target)?;
            f.write_str(" else goto ")?;
            write_block_target(f, unit, function, false_target)
        }
        MIRInstructionKind::CaseBranch {
            value,
            cases,
            default,
        } => {
            f.write_str("switch ")?;
            write_value(f, unit, function, value)?;
            f.write_str(" {")?;
            for (index, (case_value, target)) in cases.iter().enumerate() {
                if index != 0 {
                    f.write_str(",")?;
                }
                write!(f, " {case_value} => ")?;
                write_block_target(f, unit, function, target)?;
            }
            if let Some(default) = default {
                if !cases.is_empty() {
                    f.write_str(",")?;
                }
                f.write_str(" _ => ")?;
                write_block_target(f, unit, function, default)?;
            }
            f.write_str(" }")
        }
        MIRInstructionKind::Unreachable => f.write_str("unreachable"),
    }
}

fn write_comptime_instruction<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    instruction: &MIRComptimeInstruction,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    match instruction {
        MIRComptimeInstruction::Runtime(instr) => {
            write_instruction(f, unit, function, instr, types)
        }
        MIRComptimeInstruction::Comptime { op: operation, .. } => {
            f.write_str("comptime ")?;
            match operation {
                MIRComptimeOp::Call { out, callee, args } => {
                    if let Some(out) = out {
                        write_register_name(f, function, *out)?;
                        f.write_str(" = ")?;
                    }
                    f.write_str("call ")?;
                    write!(f, "{callee}")?;
                    f.write_str("(")?;
                    write_values(f, unit, function, args)?;
                    f.write_str(")")
                }
                MIRComptimeOp::Emit { out, captures, .. } => {
                    write_register_name(f, function, *out)?;
                    f.write_str(" = staged.emit(")?;
                    let mut first = true;
                    for capture in captures.values() {
                        if !first {
                            f.write_str(", ")?;
                        }
                        first = false;
                        write_value(f, unit, function, capture)?;
                    }
                    f.write_str(")")
                }
                MIRComptimeOp::Materialize { out, staged, args } => {
                    if let Some(out) = out {
                        write_register_name(f, function, *out)?;
                        f.write_str(" = ")?;
                    }
                    f.write_str("apply ")?;
                    write_value(f, unit, function, staged)?;
                    f.write_str("(")?;
                    write_values(f, unit, function, args)?;
                    f.write_str(")")
                }
            }
        }
    }
}

#[derive(Clone, Copy)]
enum IntrinsicOutput {
    Target(MIRTarget),
}

impl From<MIRTarget> for IntrinsicOutput {
    fn from(target: MIRTarget) -> Self {
        Self::Target(target)
    }
}

fn write_intrinsic_call<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    types: &mut TypePrinter<'_, T>,
    output: Option<IntrinsicOutput>,
    path: &str,
    args: impl FnOnce(
        &mut Formatter<'_>,
        &MIRUnit,
        &MIRFunction,
        &mut TypePrinter<'_, T>,
    ) -> fmt::Result,
) -> fmt::Result {
    if let Some(output) = output {
        match output {
            IntrinsicOutput::Target(target) => write_target(f, unit, function, target)?,
        }
        f.write_str(" = ")?;
    }
    write!(f, "@intrinsic.{path}(")?;
    args(f, unit, function, types)?;
    f.write_str(")")
}

fn write_intrinsic_unary<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    types: &mut TypePrinter<'_, T>,
    path: &str,
    out: MIRTarget,
    value: &MIRValue,
) -> fmt::Result {
    write_intrinsic_call(
        f,
        unit,
        function,
        types,
        Some(IntrinsicOutput::Target(out)),
        path,
        |f, unit, function, _| write_value(f, unit, function, value),
    )
}

fn write_intrinsic_binary<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    types: &mut TypePrinter<'_, T>,
    path: &str,
    out: MIRTarget,
    lhs: &MIRValue,
    rhs: &MIRValue,
) -> fmt::Result {
    write_intrinsic_call(
        f,
        unit,
        function,
        types,
        Some(IntrinsicOutput::Target(out)),
        path,
        |f, unit, function, _| {
            write_value(f, unit, function, lhs)?;
            f.write_str(", ")?;
            write_value(f, unit, function, rhs)
        },
    )
}

fn write_intrinsic_value_type<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    types: &mut TypePrinter<'_, T>,
    path: &str,
    out: MIRTarget,
    value: &MIRValue,
    target_ty: MIRTypeID,
) -> fmt::Result {
    write_intrinsic_call(
        f,
        unit,
        function,
        types,
        Some(IntrinsicOutput::Target(out)),
        path,
        |f, unit, function, types| {
            write_value(f, unit, function, value)?;
            f.write_str(", ")?;
            types.write(f, target_ty)
        },
    )
}

fn write_intrinsic<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    intrinsic: &MIRIntrinsic,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    match intrinsic {
        MIRIntrinsic::Int(operation) => write_int_intrinsic(f, unit, function, operation, types),
        MIRIntrinsic::Float(operation) => {
            write_float_intrinsic(f, unit, function, operation, types)
        }
        MIRIntrinsic::Pointer(operation) => {
            write_pointer_intrinsic(f, unit, function, operation, types)
        }
        MIRIntrinsic::Aggregate(operation) => {
            write_aggregate_intrinsic(f, unit, function, operation, types)
        }
        MIRIntrinsic::Internal(operation) => {
            write_internal_intrinsic(f, unit, function, operation, types)
        }
        MIRIntrinsic::VA(operation) => write_va_intrinsic(f, unit, function, operation, types),
    }
}

fn write_int_intrinsic<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    intrinsic: &MIRIntIntrinsic,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    match intrinsic {
        MIRIntIntrinsic::Neg { out, value } => {
            write_intrinsic_unary(f, unit, function, types, "int.neg", *out, value)
        }
        MIRIntIntrinsic::LNot { out, value } => {
            write_intrinsic_unary(f, unit, function, types, "int.l_not", *out, value)
        }
        MIRIntIntrinsic::BNot { out, value } => {
            write_intrinsic_unary(f, unit, function, types, "int.b_not", *out, value)
        }
        MIRIntIntrinsic::ToFloat { out, value, target } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            Some(IntrinsicOutput::Target(*out)),
            "int.to_float",
            |f, unit, function, _| {
                write_value(f, unit, function, value)?;
                write!(f, ", f{}", float_width(*target))
            },
        ),
        MIRIntIntrinsic::ToPtr { out, value } => {
            write_intrinsic_unary(f, unit, function, types, "int.to_ptr", *out, value)
        }
        MIRIntIntrinsic::IntCast {
            out,
            value,
            target,
            sign_extend,
        } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            Some(IntrinsicOutput::Target(*out)),
            "int.int_cast",
            |f, unit, function, _| {
                write_value(f, unit, function, value)?;
                f.write_str(", ")?;
                write!(f, "{target:?}")?;
                write!(f, ", {sign_extend}")
            },
        ),
        MIRIntIntrinsic::Add { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.add", *out, lhs, rhs)
        }
        MIRIntIntrinsic::Sub { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.sub", *out, lhs, rhs)
        }
        MIRIntIntrinsic::UMul { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_mul", *out, lhs, rhs)
        }
        MIRIntIntrinsic::SMul { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_mul", *out, lhs, rhs)
        }
        MIRIntIntrinsic::UDiv { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_div", *out, lhs, rhs)
        }
        MIRIntIntrinsic::SDiv { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_div", *out, lhs, rhs)
        }
        MIRIntIntrinsic::UMod { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_mod", *out, lhs, rhs)
        }
        MIRIntIntrinsic::SMod { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_mod", *out, lhs, rhs)
        }
        MIRIntIntrinsic::Eq { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.eq", *out, lhs, rhs)
        }
        MIRIntIntrinsic::Neq { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.neq", *out, lhs, rhs)
        }
        MIRIntIntrinsic::ULt { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_lt", *out, lhs, rhs)
        }
        MIRIntIntrinsic::SLt { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_lt", *out, lhs, rhs)
        }
        MIRIntIntrinsic::ULe { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_le", *out, lhs, rhs)
        }
        MIRIntIntrinsic::SLe { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_le", *out, lhs, rhs)
        }
        MIRIntIntrinsic::UGt { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_gt", *out, lhs, rhs)
        }
        MIRIntIntrinsic::SGt { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_gt", *out, lhs, rhs)
        }
        MIRIntIntrinsic::UGe { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_ge", *out, lhs, rhs)
        }
        MIRIntIntrinsic::SGe { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_ge", *out, lhs, rhs)
        }
        MIRIntIntrinsic::LAnd { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.l_and", *out, lhs, rhs)
        }
        MIRIntIntrinsic::LOr { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.l_or", *out, lhs, rhs)
        }
        MIRIntIntrinsic::BAnd { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.b_and", *out, lhs, rhs)
        }
        MIRIntIntrinsic::BOr { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.b_or", *out, lhs, rhs)
        }
        MIRIntIntrinsic::BXor { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.b_xor", *out, lhs, rhs)
        }
        MIRIntIntrinsic::LShift { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.l_shift", *out, lhs, rhs)
        }
        MIRIntIntrinsic::ARShift { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.ar_shift", *out, lhs, rhs)
        }
        MIRIntIntrinsic::LRShift { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "int.lr_shift", *out, lhs, rhs)
        }
    }
}

fn write_float_intrinsic<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    intrinsic: &MIRFloatIntrinsic,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    match intrinsic {
        MIRFloatIntrinsic::Neg { out, value } => {
            write_intrinsic_unary(f, unit, function, types, "float.neg", *out, value)
        }
        MIRFloatIntrinsic::ToInt {
            out,
            value,
            target_ty,
        } => write_intrinsic_value_type(
            f,
            unit,
            function,
            types,
            "float.to_int",
            *out,
            value,
            *target_ty,
        ),
        MIRFloatIntrinsic::FloatCast {
            out,
            value,
            float_ty,
        } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            Some(IntrinsicOutput::Target(*out)),
            "float.cast",
            |f, unit, function, _| {
                write_value(f, unit, function, value)?;
                write!(f, ", f{}", float_width(*float_ty))
            },
        ),
        MIRFloatIntrinsic::Eq { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "float.eq", *out, lhs, rhs)
        }
        MIRFloatIntrinsic::Neq { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "float.neq", *out, lhs, rhs)
        }
        MIRFloatIntrinsic::Lt { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "float.lt", *out, lhs, rhs)
        }
        MIRFloatIntrinsic::Le { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "float.le", *out, lhs, rhs)
        }
        MIRFloatIntrinsic::Gt { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "float.gt", *out, lhs, rhs)
        }
        MIRFloatIntrinsic::Geq { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "float.geq", *out, lhs, rhs)
        }
    }
}

fn write_pointer_intrinsic<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    intrinsic: &MIRPtrIntrinsic,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    match intrinsic {
        MIRPtrIntrinsic::ToInt {
            out,
            ptr,
            target_ty,
        } => write_intrinsic_value_type(
            f,
            unit,
            function,
            types,
            "pointer.to_int",
            *out,
            ptr,
            *target_ty,
        ),
        MIRPtrIntrinsic::Add { out, ptr, offset } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.add", *out, ptr, offset)
        }
        MIRPtrIntrinsic::Sub { out, ptr, offset } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.sub", *out, ptr, offset)
        }
        MIRPtrIntrinsic::Diff { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.diff", *out, lhs, rhs)
        }
        MIRPtrIntrinsic::Eq { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.eq", *out, lhs, rhs)
        }
        MIRPtrIntrinsic::Neq { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.neq", *out, lhs, rhs)
        }
        MIRPtrIntrinsic::Lt { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.lt", *out, lhs, rhs)
        }
        MIRPtrIntrinsic::Leq { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.leq", *out, lhs, rhs)
        }
        MIRPtrIntrinsic::Gt { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.gt", *out, lhs, rhs)
        }
        MIRPtrIntrinsic::Geq { out, lhs, rhs } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.geq", *out, lhs, rhs)
        }
    }
}

fn write_internal_intrinsic<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    intrinsic: &MIRInternalIntrinsic,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    match intrinsic {
        MIRInternalIntrinsic::StringAddress { out, string } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            Some(IntrinsicOutput::Target(*out)),
            "internal.string_address",
            |f, _, _, _| Display::fmt(string, f),
        ),
        MIRInternalIntrinsic::GetFnPtr { out, fn_id } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            Some(IntrinsicOutput::Target(*out)),
            "internal.get_fn_ptr",
            |f, _, _, _| Display::fmt(fn_id, f),
        ),
        MIRInternalIntrinsic::Bitcast {
            out,
            value,
            target_ty,
        } => write_intrinsic_value_type(
            f,
            unit,
            function,
            types,
            "internal.bitcast",
            *out,
            value,
            *target_ty,
        ),
        MIRInternalIntrinsic::Assert { condition, message } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            None,
            "internal.assert",
            |f, unit, function, _| {
                write_value(f, unit, function, condition)?;
                if let Some(message) = message {
                    write!(f, ", {message:?}")?;
                }
                Ok(())
            },
        ),
        MIRInternalIntrinsic::Assume { condition } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            None,
            "internal.assume",
            |f, unit, function, _| write_value(f, unit, function, condition),
        ),
    }
}

fn write_va_intrinsic<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    intrinsic: &MIRVAIntrinsic,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    match intrinsic {
        MIRVAIntrinsic::VaStart { list, last } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            None,
            "va.start",
            |f, unit, function, _| {
                write_value(f, unit, function, list)?;
                f.write_str(", ")?;
                write_value(f, unit, function, last)
            },
        ),
        MIRVAIntrinsic::VaEnd { list } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            None,
            "va.end",
            |f, unit, function, _| write_value(f, unit, function, list),
        ),
        MIRVAIntrinsic::VaArg { out, list, ty } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            Some(IntrinsicOutput::Target(*out)),
            "va.arg",
            |f, unit, function, types| {
                write_value(f, unit, function, list)?;
                f.write_str(", ")?;
                types.write(f, *ty)
            },
        ),
    }
}

fn write_aggregate_intrinsic<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    intrinsic: &MIRAggregateIntrinsic,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    match intrinsic {
        MIRAggregateIntrinsic::SumIndex { out, value, sum_ty } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            Some(IntrinsicOutput::Target(*out)),
            "aggregate.sum_index",
            |f, unit, function, types| {
                write_value(f, unit, function, value)?;
                f.write_str(", ")?;
                types.write(f, *sum_ty)
            },
        ),
        MIRAggregateIntrinsic::SumVariant {
            out,
            base,
            variant,
            sum_ty,
        } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            Some(IntrinsicOutput::Target(*out)),
            "aggregate.sum_variant",
            |f, unit, function, types| {
                write_place_name(f, unit, function, *base)?;
                write!(f, ", {variant}, ")?;
                types.write(f, *sum_ty)
            },
        ),
        MIRAggregateIntrinsic::SumVariantL {
            out,
            base,
            variant,
            sum_ty,
        } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            Some(IntrinsicOutput::Target(*out)),
            "aggregate.sum_variant_l",
            |f, unit, function, types| {
                write_value(f, unit, function, base)?;
                write!(f, ", {variant}, ")?;
                types.write(f, *sum_ty)
            },
        ),
        MIRAggregateIntrinsic::StructInit { out, ty, fields } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            Some(IntrinsicOutput::Target(*out)),
            "aggregate.struct_init",
            |f, unit, function, types| {
                types.write(f, *ty)?;
                for (field, value) in fields {
                    write!(f, ", {field}: ")?;
                    write_value(f, unit, function, value)?;
                }
                Ok(())
            },
        ),
        MIRAggregateIntrinsic::StructField {
            out,
            base,
            field,
            struct_ty,
        } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            Some(IntrinsicOutput::Target(*out)),
            "aggregate.struct_field",
            |f, unit, function, types| {
                write_value(f, unit, function, base)?;
                write!(f, ", {field}, ")?;
                types.write(f, *struct_ty)
            },
        ),
        MIRAggregateIntrinsic::ArrayIndex {
            out,
            base,
            index,
            element_ty,
        } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            Some(IntrinsicOutput::Target(*out)),
            "aggregate.array_index",
            |f, unit, function, types| {
                write_value(f, unit, function, base)?;
                f.write_str(", ")?;
                write_value(f, unit, function, index)?;
                f.write_str(", ")?;
                types.write(f, *element_ty)
            },
        ),
    }
}

fn write_block_target(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    target: &MIRBlockTarget,
) -> fmt::Result {
    Display::fmt(&target.block, f)?;
    if !target.args.is_empty() {
        f.write_str("(")?;
        write_values(f, unit, function, &target.args)?;
        f.write_str(")")?;
    }
    Ok(())
}

fn write_values(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    values: &[MIRValue],
) -> fmt::Result {
    for (index, value) in values.iter().enumerate() {
        if index != 0 {
            f.write_str(", ")?;
        }
        write_value(f, unit, function, value)?;
    }
    Ok(())
}

fn write_value(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    value: &MIRValue,
) -> fmt::Result {
    match value {
        MIRValue::Register(register) => write_register_name(f, function, *register),
        MIRValue::Global(global) => write_global_name(f, unit, *global),
        MIRValue::PlaceRef(place) => write_place_name(f, unit, function, *place),
        MIRValue::Constant(constant) => write_constant(f, unit, constant),
    }
}

fn write_constant(f: &mut Formatter<'_>, unit: &MIRUnit, constant: &MIRConstant) -> fmt::Result {
    match constant {
        MIRConstant::Function(function_id) => {
            if let Some(function) = unit.function(*function_id) {
                write!(f, "@{}", function.prototype().display_name())
            } else {
                Display::fmt(function_id, f)
            }
        }
        MIRConstant::Global { global, offset, .. } => {
            write_global_name(f, unit, *global)?;
            if *offset != 0 {
                write!(f, " + {offset}")?;
            }
            Ok(())
        }
        MIRConstant::Aggregate { fields, .. } => {
            f.write_str("{")?;
            for (index, (field, value)) in fields.iter().enumerate() {
                if index != 0 {
                    f.write_str(", ")?;
                }
                write!(f, "{field}: ")?;
                Display::fmt(value, f)?;
            }
            f.write_str("}")
        }
        _ => Display::fmt(constant, f),
    }
}

fn write_bindable(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    bindable: &MIRBindable,
) -> fmt::Result {
    match bindable {
        MIRBindable::Register(register) => write_register_name(f, function, *register),
        MIRBindable::Place(place) => write_place_name(f, unit, function, *place),
    }
}

fn write_place_name(
    f: &mut Formatter<'_>,
    _unit: &MIRUnit,
    function: &MIRFunction,
    place: MIRPlaceID,
) -> fmt::Result {
    if let Some(place) = function.body().and_then(|body| body.place(place))
        && let Some(name) = &place.debug_name
    {
        return write!(f, "%{name}");
    }
    Display::fmt(&place, f)
}

fn write_register_name(
    f: &mut Formatter<'_>,
    function: &MIRFunction,
    register: MIRRegisterID,
) -> fmt::Result {
    if let Some(register) = function.body().and_then(|body| body.register(register))
        && let Some(name) = &register.debug_name
    {
        return write!(f, "%{name}");
    }
    Display::fmt(&register, f)
}

fn write_global_name(f: &mut Formatter<'_>, unit: &MIRUnit, global: MIRGlobalID) -> fmt::Result {
    if let Some(global) = unit.global(global) {
        write!(f, "@{}", global.name())
    } else {
        Display::fmt(&global, f)
    }
}

fn write_target(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    target: MIRTarget,
) -> fmt::Result {
    match target {
        MIRTarget::Place(place) => write_place_name(f, unit, function, place),
        MIRTarget::Global(global) => write_global_name(f, unit, global),
        MIRTarget::Register(register) => write_register_name(f, function, register),
        MIRTarget::Indirect(register) => {
            f.write_str("*")?;
            write_register_name(f, function, register)
        }
    }
}

fn integer_width(ty: MIRIntType) -> u16 {
    match ty {
        MIRIntType::I1 => 1,
        MIRIntType::I8 => 8,
        MIRIntType::I16 => 16,
        MIRIntType::I32 => 32,
        MIRIntType::I64 => 64,
        MIRIntType::I128 => 128,
    }
}

fn float_width(ty: MIRFloatType) -> u16 {
    match ty {
        MIRFloatType::F32 => 32,
        MIRFloatType::F64 => 64,
    }
}

#[allow(dead_code)]
fn write_aggregate<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    intrinsic: &MIRAggregateIntrinsic,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    write_aggregate_intrinsic(f, unit, function, intrinsic, types)
}
