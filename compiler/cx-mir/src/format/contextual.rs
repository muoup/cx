use crate::{MIRBody, MIRComptimeInstrKind, MIRComptimeOp, MIRFunctionBody, MIRInstructionLike};
use std::fmt::{self, Display, Formatter};

use cx_util::linkage::LinkageMode;

use crate::MIRGlobalVariable;
use crate::global::{MIRFunction, MIRGlobalKind, MIRGlobalState};
use crate::instruction::{
    MIRAggregateOp, MIRBasicBlock, MIRConstant, MIRInstrKind, MIRTarget, MIRTargetAggregateOp,
    MIRValue, MIRValueAggregateOp,
};
use crate::op::{
    MIRBinaryOp, MIRFloatBinaryOp, MIRIntBinaryOp, MIRPointerBinaryOp, MIRPointerOffsetOp,
    MIRUnaryOp,
};
use crate::ty::interface::MTRegistry;
use crate::ty::{MIRField, MIRFloatType, MIRIntType, MIRTypeID, MIRTypeKind};
use crate::unit::MIRUnit;

pub struct MIRDisplay<'a> {
    unit: &'a MIRUnit,
}

impl MIRUnit {
    pub fn display_pretty(&self) -> MIRDisplay<'_> {
        MIRDisplay { unit: self }
    }
}

impl Display for MIRDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut types = TypePrinter::new(self.unit.types());

        for global in self.unit.globals() {
            if global.id.index() != 0 {
                f.write_str("\n")?;
            }
            write_global(f, self.unit, global, &mut types)?;
        }

        for function in self.unit.functions() {
            if function.id().index() != 0 {
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

        if is_aggregate(&definition.kind)
            && let Some(name) = self.registry.debug_name(id)
        {
            return f.write_str(name);
        }
        let kind = definition.kind.clone();

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
                write!(f, "[")?;
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
                for (index, parameter) in signature.params.iter().enumerate() {
                    if index != 0 {
                        f.write_str(", ")?;
                    }
                    self.write(f, *parameter)?;
                }
                f.write_str(") -> ")?;
                self.write(f, signature.return_type)
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
            .and_then(|definition| aggregate_fields(&definition.kind))
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
    match global.linkage {
        LinkageMode::Extern => f.write_str("extern ")?,
        LinkageMode::Static => f.write_str("static ")?,
        _ => {}
    }
    write!(f, "{}: ", global.name)?;

    match &global.kind {
        MIRGlobalKind::StringLiteral { value } => {
            write!(f, "str = {};", value)?;
        }

        MIRGlobalKind::Variable {
            ty,
            state,
            is_mutable,
        } => {
            if !is_mutable {
                f.write_str("const ")?;
            }
            types.write(f, *ty)?;
            match &state {
                MIRGlobalState::External => f.write_str(";")?,
                MIRGlobalState::ZeroInitialized => f.write_str(" = zero;")?,
                MIRGlobalState::Initialized(value) => {
                    f.write_str(" = ")?;
                    write_constant(f, unit, value)?;
                    f.write_str(";")?;
                }
            }
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
    if function.prototype().linkage == LinkageMode::Static {
        f.write_str("static ")?;
    } else if function.prototype().linkage == LinkageMode::Extern {
        f.write_str("extern ")?;
    }
    write!(f, "fn {} (", function.prototype().signature.display_name())?;
    for (index, parameter) in function.prototype().signature.params.iter().enumerate() {
        if index != 0 {
            f.write_str(", ")?;
        }
        if let Some(name) = &parameter.name {
            write!(f, "{name}: ")?;
        } else {
            write!(f, "arg{index}: ")?;
        }
        types.write(f, parameter.ty)?;
    }
    if function.prototype().signature.variadic {
        if !function.prototype().signature.params.is_empty() {
            f.write_str(", ")?;
        }
        f.write_str("...")?;
    }
    write!(
        f,
        ") -> {} /* {} */",
        function.prototype().signature.return_type,
        function.prototype().signature.symbol_name
    )?;

    match function.body() {
        Some(MIRFunctionBody::Runtime(body)) => {
            write_body(f, unit, function, body, types, write_instruction)
        }
        Some(MIRFunctionBody::Comptime(body)) => write_body(
            f,
            unit,
            function,
            body,
            types,
            |f, unit, function, kind, types| match kind {
                MIRComptimeInstrKind::Standard(kind) => {
                    write_instruction(f, unit, function, kind, types)
                }
                MIRComptimeInstrKind::Comptime(kind) => {
                    f.write_str("comptime ")?;
                    match kind {
                        MIRComptimeOp::Call { out, callee, args } => {
                            if let Some(out) = out {
                                write_register_name(f, function, *out)?;
                                f.write_str(" = ")?;
                            }
                            f.write_str("call ")?;
                            write_value(f, unit, function, callee)?;
                            f.write_str("(")?;
                            for (index, arg) in args.iter().enumerate() {
                                if index != 0 {
                                    f.write_str(", ")?;
                                }
                                write_value(f, unit, function, arg)?;
                            }
                            f.write_str(")")
                        }
                        MIRComptimeOp::MakeStaged {
                            out,
                            template,
                            captures,
                        } => {
                            write_register_name(f, function, *out)?;
                            write!(f, " = staged<{} blocks>(", template.body().blocks().len())?;
                            for (index, value) in captures.iter().enumerate() {
                                if index != 0 {
                                    f.write_str(", ")?;
                                }
                                write_value(f, unit, function, value)?;
                            }
                            f.write_str(")")
                        }
                        MIRComptimeOp::ApplyStaged {
                            out,
                            staged,
                            args,
                            targets,
                        } => {
                            if let Some(out) = out {
                                write_register_name(f, function, *out)?;
                                f.write_str(" = ")?;
                            }
                            f.write_str("apply ")?;
                            write_value(f, unit, function, staged)?;
                            f.write_str("(")?;
                            for (index, value) in args.iter().enumerate() {
                                if index != 0 {
                                    f.write_str(", ")?;
                                }
                                write_value(f, unit, function, value)?;
                            }
                            write!(f, ") {targets:?}")
                        }
                    }
                }
            },
        ),
        None => f.write_str(";"),
    }
}

fn write_body<T: MTRegistry, K: MIRInstructionLike>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    definition: &MIRBody<K>,
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
    for place in definition.places() {
        f.write_str("    let ")?;
        write_place_name(f, unit, function, place.id)?;
        f.write_str(": ")?;
        types.write(f, place.ty)?;
        f.write_str(";\n")?;
    }
    for register in definition.registers() {
        f.write_str("    let ")?;
        write_register_name(f, function, register.id)?;
        f.write_str(": ")?;
        types.write(f, register.ty)?;
        f.write_str(";\n")?;
    }
    for block in definition.blocks() {
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
    write!(f, "    bb{}", block.id.index())?;
    if !block.params.is_empty() {
        f.write_str("(")?;
        for (index, parameter) in block.params.iter().enumerate() {
            if index != 0 {
                f.write_str(", ")?;
            }
            write_register_name(f, function, *parameter)?;
        }
        f.write_str(")")?;
    }
    if let Some(name) = &block.debug_name {
        write!(f, " /* {name} */")?;
    }
    f.write_str(":\n")?;
    for instruction in &block.instrs {
        f.write_str("        ")?;
        write_kind(f, unit, function, &instruction.kind, types)?;
        f.write_str(";\n")?;
    }
    Ok(())
}

fn write_instruction<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    instruction: &MIRInstrKind,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    match instruction {
        MIRInstrKind::ScopeEnter { scope } => write!(f, "scope.enter {scope:?}"),
        MIRInstrKind::ScopeExit { scope } => write!(f, "scope.exit {scope:?}"),

        MIRInstrKind::Initialize { place } => {
            f.write_str("initialize ")?;
            write_place_name(f, unit, function, *place)
        }

        MIRInstrKind::Bind { place, to } => {
            f.write_str("bind ")?;
            write_place_name(f, unit, function, *place)?;
            f.write_str(" to ")?;
            write_target(f, unit, function, *to)
        }

        MIRInstrKind::Invalidate { place, leak } => {
            if *leak {
                f.write_str("leak ")?;
            } else {
                f.write_str("invalidate ")?;
            }
            write_place_name(f, unit, function, *place)
        }

        MIRInstrKind::Copy { out, source, .. } => {
            write_register_name(f, function, *out)?;
            write!(f, " = copy ")?;
            write_target(f, unit, function, *source)
        }
        MIRInstrKind::Store { target, value, .. } => {
            write_target(f, unit, function, *target)?;
            f.write_str(" = ")?;
            write_value(f, unit, function, value)
        }
        MIRInstrKind::Let { out, value } => {
            write_register_name(f, function, *out)?;
            f.write_str(" = let ")?;
            write_value(f, unit, function, value)
        }

        MIRInstrKind::AggregateOp(operation) => {
            write_aggregate(f, unit, function, operation, types)
        }

        MIRInstrKind::Call { out, callee, args } => {
            if let Some(out) = out {
                write_register_name(f, function, *out)?;
                f.write_str(" = ")?;
            }
            write_value(f, unit, function, callee)?;
            f.write_str("(")?;
            write_values(f, unit, function, args)?;
            f.write_str(")")
        }

        MIRInstrKind::Intrinsic(crate::MIRIntrinsic::VaStart { list, last }) => {
            f.write_str("va_start(")?;
            write_value(f, unit, function, list)?;
            f.write_str(", ")?;
            write_value(f, unit, function, last)?;
            f.write_str(")")
        }

        MIRInstrKind::Intrinsic(crate::MIRIntrinsic::VaEnd { list }) => {
            f.write_str("va_end(")?;
            write_value(f, unit, function, list)?;
            f.write_str(")")
        }

        MIRInstrKind::Intrinsic(crate::MIRIntrinsic::VaArg { out, list, ty }) => {
            write_register_name(f, function, *out)?;
            f.write_str(" = va_arg(")?;
            write_value(f, unit, function, list)?;
            write!(f, ", ")?;
            types.write(f, *ty)?;
            f.write_str(")")
        }

        MIRInstrKind::BinOp { out, op, lhs, rhs } => {
            write_register_name(f, function, *out)?;
            write!(f, " = ")?;
            write_value(f, unit, function, lhs)?;
            write!(f, " {} ", binary_operator(op))?;
            write_value(f, unit, function, rhs)
        }

        MIRInstrKind::UnOp { out, op, operand } => {
            write_register_name(f, function, *out)?;
            f.write_str(" = ")?;
            match op {
                MIRUnaryOp::Increment { amount, post } if *amount == 1 => {
                    if *post {
                        write_value(f, unit, function, operand)?;
                        f.write_str("++")
                    } else {
                        f.write_str("++")?;
                        write_value(f, unit, function, operand)
                    }
                }
                MIRUnaryOp::Increment { amount, post } if *amount == -1 => {
                    if *post {
                        write_value(f, unit, function, operand)?;
                        f.write_str("--")
                    } else {
                        f.write_str("--")?;
                        write_value(f, unit, function, operand)
                    }
                }
                MIRUnaryOp::Increment { amount, .. } => {
                    write_value(f, unit, function, operand)?;
                    write!(f, " {:+}", amount)
                }
                _ => {
                    f.write_str(unary_operator(op))?;
                    write_value(f, unit, function, operand)
                }
            }
        }

        MIRInstrKind::Coerce {
            out,
            operand,
            to_type,
            ..
        } => {
            write_register_name(f, function, *out)?;
            f.write_str(" = ")?;
            write_value(f, unit, function, operand)?;
            f.write_str(" as ")?;
            types.write(f, *to_type)
        }

        MIRInstrKind::Assert { condition, message } => {
            f.write_str("assert ")?;
            write_value(f, unit, function, condition)?;
            if let Some(message) = message {
                write!(f, ", {message:?}")?;
            }
            Ok(())
        }

        MIRInstrKind::Assume { condition } => {
            f.write_str("assume ")?;
            write_value(f, unit, function, condition)
        }

        MIRInstrKind::Return { value } => {
            f.write_str("return")?;
            if let Some(value) = value {
                f.write_str(" ")?;
                write_value(f, unit, function, value)?;
            }
            Ok(())
        }

        MIRInstrKind::Jump { target } => {
            f.write_str("goto ")?;
            write_block_target(f, unit, function, target)
        }

        MIRInstrKind::Branch {
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

        MIRInstrKind::IntSwitch {
            value,
            cases,
            default,
        } => {
            f.write_str("switch.int ")?;
            write_value(f, unit, function, value)?;
            f.write_str(" {")?;
            for (index, (constant, target)) in cases.iter().enumerate() {
                if index != 0 {
                    f.write_str(",")?;
                }
                write!(f, " {} => ", constant)?;
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

        MIRInstrKind::VariantSwitch {
            subject,
            sum_type,
            cases,
            default,
            ..
        } => {
            f.write_str("switch.variant ")?;
            write_value(f, unit, function, subject)?;
            f.write_str(" {")?;
            for (index, (variant, target)) in cases.iter().enumerate() {
                if index != 0 {
                    f.write_str(",")?;
                }
                f.write_str(" .")?;
                types.write_member_name(f, *sum_type, *variant, "variant")?;
                f.write_str(" => ")?;
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

        MIRInstrKind::Unreachable => f.write_str("unreachable"),
    }
}

fn write_aggregate<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    operation: &MIRAggregateOp,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    match operation {
        MIRAggregateOp::Target { out, op } => {
            write_register_name(f, function, *out)?;
            f.write_str(" = project ")?;
            match op {
                MIRTargetAggregateOp::Field {
                    base,
                    field,
                    aggregate_type,
                } => {
                    write_target(f, unit, function, *base)?;
                    f.write_str(".")?;
                    types.write_member_name(f, *aggregate_type, *field, "field")
                }
                MIRTargetAggregateOp::Index { base, index, .. } => {
                    write_target(f, unit, function, *base)?;
                    f.write_str("[")?;
                    write_value(f, unit, function, index)?;
                    f.write_str("]")
                }
                MIRTargetAggregateOp::Variant {
                    base,
                    variant,
                    sum_type,
                } => {
                    write_target(f, unit, function, *base)?;
                    f.write_str(".")?;
                    types.write_member_name(f, *sum_type, *variant, "variant")
                }
            }
        }
        MIRAggregateOp::Value { out, op } => {
            write_register_name(f, function, *out)?;
            f.write_str(" = ")?;
            match op {
                MIRValueAggregateOp::Discriminant { value, .. } => {
                    f.write_str("discriminant(")?;
                    write_value(f, unit, function, value)?;
                    f.write_str(")")
                }
                MIRValueAggregateOp::Construct { ty, fields } => {
                    types.write(f, *ty)?;
                    f.write_str(" {")?;
                    for (index, (field, value)) in fields.iter().enumerate() {
                        if index != 0 {
                            f.write_str(", ")?;
                        }
                        types.write_member_name(f, *ty, *field, "field")?;
                        f.write_str(": ")?;
                        write_value(f, unit, function, value)?;
                    }
                    f.write_str(" }")
                }
                MIRValueAggregateOp::Variant {
                    variant,
                    value,
                    sum_type,
                } => {
                    types.write_member_name(f, *sum_type, *variant, "variant")?;
                    f.write_str("(")?;
                    write_value(f, unit, function, value)?;
                    f.write_str(")")
                }
                MIRValueAggregateOp::ProjectVariant {
                    variant,
                    value,
                    sum_type,
                } => {
                    f.write_str("project ")?;
                    types.write_member_name(f, *sum_type, *variant, "variant")?;
                    f.write_str("(")?;
                    write_value(f, unit, function, value)?;
                    f.write_str(")")
                }
            }
        }
    }
}

fn write_block_target(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    target: &crate::instruction::MIRBlockTarget,
) -> fmt::Result {
    write!(f, "bb{}", target.block.index())?;
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
        MIRValue::Reference(target) => write_target(f, unit, function, *target),
        MIRValue::Constant(constant) => write_constant(f, unit, constant),
    }
}

fn write_constant(f: &mut Formatter<'_>, unit: &MIRUnit, constant: &MIRConstant) -> fmt::Result {
    match constant {
        MIRConstant::Function(function_id) => {
            if let Some(function) = unit.function(*function_id) {
                write!(f, "fn {}", function.prototype().signature.display_name())
            } else {
                write!(f, "fn f{}", function_id.index())
            }
        }
        _ => Display::fmt(constant, f),
    }
}

fn write_place_name(
    f: &mut Formatter<'_>,
    _unit: &MIRUnit,
    function: &MIRFunction,
    place: crate::MIRPlaceID,
) -> fmt::Result {
    if let Some(place) = function
        .body()
        .and_then(|definition| definition.place(place))
        && let Some(name) = &place.debug_name
    {
        return Display::fmt(name, f);
    }
    write!(f, "local{}", place.index())
}

fn write_target(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    target: MIRTarget,
) -> fmt::Result {
    match target {
        MIRTarget::Place(place) => write_place_name(f, unit, function, place),
        MIRTarget::Global(id) => {
            if let Some(global) = unit.global(id) {
                Display::fmt(&global.name, f)
            } else {
                write!(f, "global{}", id.index())
            }
        }
        MIRTarget::Indirect(register) => write_register_name(f, function, register),
    }
}

fn write_register_name(
    f: &mut Formatter<'_>,
    function: &MIRFunction,
    register: crate::instruction::MIRRegister,
) -> fmt::Result {
    if let Some(register_decl) = function
        .body()
        .and_then(|definition| definition.register(register))
        && let Some(name) = &register_decl.debug_name
    {
        return Display::fmt(name, f);
    }
    write!(f, "r{}", register.index())
}

fn binary_operator(op: &MIRBinaryOp) -> &'static str {
    match op {
        MIRBinaryOp::Integer { op, .. } => match op {
            MIRIntBinaryOp::Add => "+",
            MIRIntBinaryOp::Sub => "-",
            MIRIntBinaryOp::Mul | MIRIntBinaryOp::SignedMul => "*",
            MIRIntBinaryOp::Div | MIRIntBinaryOp::SignedDiv => "/",
            MIRIntBinaryOp::Mod | MIRIntBinaryOp::SignedMod => "%",
            MIRIntBinaryOp::Eq => "==",
            MIRIntBinaryOp::Ne => "!=",
            MIRIntBinaryOp::Lt | MIRIntBinaryOp::SignedLt => "<",
            MIRIntBinaryOp::Le | MIRIntBinaryOp::SignedLe => "<=",
            MIRIntBinaryOp::Gt | MIRIntBinaryOp::SignedGt => ">",
            MIRIntBinaryOp::Ge | MIRIntBinaryOp::SignedGe => ">=",
            MIRIntBinaryOp::LogicalAnd => "&&",
            MIRIntBinaryOp::LogicalOr => "||",
            MIRIntBinaryOp::BitAnd => "&",
            MIRIntBinaryOp::BitOr => "|",
            MIRIntBinaryOp::BitXor => "^",
            MIRIntBinaryOp::ShiftLeft => "<<",
            MIRIntBinaryOp::ArithmeticShiftRight | MIRIntBinaryOp::LogicalShiftRight => ">>",
        },
        MIRBinaryOp::Float { op, .. } => match op {
            MIRFloatBinaryOp::Add => "+",
            MIRFloatBinaryOp::Sub => "-",
            MIRFloatBinaryOp::Mul => "*",
            MIRFloatBinaryOp::Div => "/",
            MIRFloatBinaryOp::Eq => "==",
            MIRFloatBinaryOp::Ne => "!=",
            MIRFloatBinaryOp::Lt => "<",
            MIRFloatBinaryOp::Le => "<=",
            MIRFloatBinaryOp::Gt => ">",
            MIRFloatBinaryOp::Ge => ">=",
        },
        MIRBinaryOp::PointerOffset { op, .. } => match op {
            MIRPointerOffsetOp::Add => "+",
            MIRPointerOffsetOp::Sub => "-",
        },
        MIRBinaryOp::Pointer(op) => match op {
            MIRPointerBinaryOp::Eq => "==",
            MIRPointerBinaryOp::Ne => "!=",
            MIRPointerBinaryOp::Lt => "<",
            MIRPointerBinaryOp::Le => "<=",
            MIRPointerBinaryOp::Gt => ">",
            MIRPointerBinaryOp::Ge => ">=",
        },
    }
}

fn unary_operator(op: &MIRUnaryOp) -> &'static str {
    match op {
        MIRUnaryOp::IntegerNeg { .. } | MIRUnaryOp::FloatNeg(_) => "-",
        MIRUnaryOp::BitNot(_) => "~",
        MIRUnaryOp::LogicalNot => "!",
        MIRUnaryOp::Increment { .. } => "",
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
