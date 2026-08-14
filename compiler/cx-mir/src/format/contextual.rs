use std::fmt::{self, Display, Formatter};

use cx_ast::ast::modifiers::CXLinkageMode;

use crate::expr::{
    MIRAggregateOp, MIRBasicBlock, MIRConstant, MIRInstrKind, MIRPlace, MIRPlaceAggregateOp,
    MIRValue, MIRValueAggregateOp,
};
use crate::global::{MIRFunction, MIRGlobalState};
use crate::module::MIRUnit;
use crate::op::{
    MIRBinaryOp, MIRFloatBinaryOp, MIRIntBinaryOp, MIRPointerBinaryOp, MIRPointerOffsetOp,
    MIRUnaryOp,
};
use crate::ty::{MIRField, MIRFloatType, MIRIntType, MIRTypeID, MIRTypeKind, MIRTypeRegistry};

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
        let mut types = TypePrinter::new(&self.unit.types);

        for (index, global) in self.unit.globals.iter().enumerate() {
            if index != 0 {
                f.write_str("\n")?;
            }
            write_global(f, self.unit, global, &mut types)?;
        }

        if !self.unit.globals.is_empty() && !self.unit.functions.is_empty() {
            f.write_str("\n\n")?;
        }

        for (index, function) in self.unit.functions.iter().enumerate() {
            if index != 0 {
                f.write_str("\n")?;
            }
            write_function(f, self.unit, function, &mut types)?;
        }

        Ok(())
    }
}

struct TypePrinter<'a> {
    registry: &'a MIRTypeRegistry,
    active: Vec<MIRTypeID>,
}

impl<'a> TypePrinter<'a> {
    fn new(registry: &'a MIRTypeRegistry) -> Self {
        Self {
            registry,
            active: Vec::new(),
        }
    }

    fn write(&mut self, f: &mut Formatter<'_>, id: MIRTypeID) -> fmt::Result {
        if self.active.contains(&id) {
            return write!(f, "t{}", id.index());
        }

        let Some(definition) = self.registry.definition(id) else {
            return write!(f, "<invalid t{}>", id.index());
        };

        if is_aggregate(&definition.kind) {
            if let Some(name) = self.registry.debug_name(id) {
                return f.write_str(name);
            }
        }
        let kind = definition.kind.clone();

        self.active.push(id);
        let result = self.write_kind(f, &kind);
        self.active.pop();
        result
    }

    fn write_kind(&mut self, f: &mut Formatter<'_>, kind: &MIRTypeKind) -> fmt::Result {
        match kind {
            MIRTypeKind::Unit => f.write_str("()"),
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
            .kind(aggregate_type)
            .and_then(aggregate_fields)
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

fn write_global(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    global: &crate::global::MIRGlobalVariable,
    types: &mut TypePrinter<'_>,
) -> fmt::Result {
    match global.state {
        MIRGlobalState::External => f.write_str("extern ")?,
        _ if global.linkage == CXLinkageMode::Static => f.write_str("static ")?,
        _ => {}
    }
    if !global.is_mutable {
        f.write_str("const ")?;
    }
    write!(f, "{}: ", global.name)?;
    types.write(f, global.ty)?;
    match &global.state {
        MIRGlobalState::External => f.write_str(";")?,
        MIRGlobalState::ZeroInitialized => f.write_str(" = zero;")?,
        MIRGlobalState::Initialized(value) => {
            f.write_str(" = ")?;
            write_constant(f, unit, value)?;
            f.write_str(";")?;
        }
    }
    Ok(())
}

fn write_function(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    types: &mut TypePrinter<'_>,
) -> fmt::Result {
    if function.prototype.linkage == CXLinkageMode::Static {
        f.write_str("static ")?;
    } else if function.prototype.linkage == CXLinkageMode::Extern || function.is_declaration() {
        f.write_str("extern ")?;
    }
    write!(f, "fn {}(", function.prototype.signature.display_name())?;
    for (index, parameter) in function.prototype.signature.params.iter().enumerate() {
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
    if function.prototype.signature.variadic {
        if !function.prototype.signature.params.is_empty() {
            f.write_str(", ")?;
        }
        f.write_str("...")?;
    }
    f.write_str(") -> ")?;
    if let Some(return_type) = function.prototype.signature.return_type {
        types.write(f, return_type)?;
    } else {
        f.write_str("()")?;
    }

    if function.is_declaration() {
        return f.write_str(";");
    }

    f.write_str(" {\n")?;
    for place in &function.places {
        f.write_str("    let ")?;
        write_place_name(f, unit, function, MIRPlace::FunctionLocal(place.id))?;
        f.write_str(": ")?;
        types.write(f, place.ty)?;
        f.write_str(";\n")?;
    }
    for register in &function.registers {
        f.write_str("    let ")?;
        write_register_name(f, function, register.id)?;
        f.write_str(": ")?;
        types.write(f, register.ty)?;
        f.write_str(";\n")?;
    }
    for block in &function.blocks {
        write_block(f, unit, function, block, types)?;
    }
    f.write_str("}")
}

fn write_block(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    block: &MIRBasicBlock,
    types: &mut TypePrinter<'_>,
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
        write_instruction(f, unit, function, &instruction.kind, types)?;
        f.write_str(";\n")?;
    }
    Ok(())
}

fn write_instruction(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    instruction: &MIRInstrKind,
    types: &mut TypePrinter<'_>,
) -> fmt::Result {
    match instruction {
        MIRInstrKind::Initialize { place } => {
            f.write_str("initialize ")?;
            write_place_name(f, unit, function, *place)
        }
        MIRInstrKind::Leak { place } => {
            f.write_str("leak ")?;
            write_place_name(f, unit, function, *place)
        }
        MIRInstrKind::Create { out, .. } => {
            f.write_str("create ")?;
            write_place_name(f, unit, function, *out)
        }
        MIRInstrKind::Assign { dest, value, .. } => {
            write_place_name(f, unit, function, *dest)?;
            f.write_str(" = ")?;
            write_value(f, unit, function, value)
        }
        MIRInstrKind::AddressOf { out, place } => {
            write_register_name(f, function, *out)?;
            f.write_str(" = &")?;
            write_place_name(f, unit, function, *place)
        }
        MIRInstrKind::Dereference { out, pointer, .. } => {
            write_place_name(f, unit, function, *out)?;
            f.write_str(" = *")?;
            write_value(f, unit, function, pointer)
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
            write_target(f, unit, function, target)
        }
        MIRInstrKind::Branch {
            cond,
            true_target,
            false_target,
        } => {
            f.write_str("if ")?;
            write_value(f, unit, function, cond)?;
            f.write_str(" goto ")?;
            write_target(f, unit, function, true_target)?;
            f.write_str(" else goto ")?;
            write_target(f, unit, function, false_target)
        }
        MIRInstrKind::IntSwitch {
            value,
            cases,
            default,
        } => {
            f.write_str("switch ")?;
            write_value(f, unit, function, value)?;
            f.write_str(" {")?;
            for (index, (constant, target)) in cases.iter().enumerate() {
                if index != 0 {
                    f.write_str(",")?;
                }
                write!(f, " {} => ", constant)?;
                write_target(f, unit, function, target)?;
            }
            if let Some(default) = default {
                if !cases.is_empty() {
                    f.write_str(",")?;
                }
                f.write_str(" _ => ")?;
                write_target(f, unit, function, default)?;
            }
            f.write_str(" }")
        }
        MIRInstrKind::VariantSwitch {
            subject,
            sum_type,
            cases,
            default,
        } => {
            f.write_str("switch ")?;
            write_place_name(f, unit, function, *subject)?;
            f.write_str(" {")?;
            for (index, (variant, target)) in cases.iter().enumerate() {
                if index != 0 {
                    f.write_str(",")?;
                }
                f.write_str(" .")?;
                types.write_member_name(f, *sum_type, *variant, "variant")?;
                f.write_str(" => ")?;
                write_target(f, unit, function, target)?;
            }
            if let Some(default) = default {
                if !cases.is_empty() {
                    f.write_str(",")?;
                }
                f.write_str(" _ => ")?;
                write_target(f, unit, function, default)?;
            }
            f.write_str(" }")
        }
        MIRInstrKind::Unreachable => f.write_str("unreachable"),
        MIRInstrKind::Emit { value } => {
            f.write_str("emit ")?;
            write_value(f, unit, function, value)
        }
    }
}

fn write_aggregate(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    operation: &MIRAggregateOp,
    types: &mut TypePrinter<'_>,
) -> fmt::Result {
    match operation {
        MIRAggregateOp::Place { out, op } => {
            write_place_name(f, unit, function, *out)?;
            f.write_str(" = &")?;
            match op {
                MIRPlaceAggregateOp::Field {
                    base,
                    field,
                    aggregate_type,
                } => {
                    write_place_name(f, unit, function, *base)?;
                    f.write_str(".")?;
                    types.write_member_name(f, *aggregate_type, *field, "field")
                }
                MIRPlaceAggregateOp::Index { base, index, .. } => {
                    write_place_name(f, unit, function, *base)?;
                    f.write_str("[")?;
                    write_value(f, unit, function, index)?;
                    f.write_str("]")
                }
                MIRPlaceAggregateOp::Variant {
                    base,
                    variant,
                    sum_type,
                } => {
                    write_place_name(f, unit, function, *base)?;
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
            }
        }
    }
}

fn write_target(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    target: &crate::expr::MIRBlockTarget,
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
        MIRValue::Place(place) => write_place_name(f, unit, function, *place),
        MIRValue::Move(place) => {
            f.write_str("move ")?;
            write_place_name(f, unit, function, *place)
        }
        MIRValue::Constant(constant) => write_constant(f, unit, constant),
    }
}

fn write_constant(f: &mut Formatter<'_>, unit: &MIRUnit, constant: &MIRConstant) -> fmt::Result {
    match constant {
        MIRConstant::Function(function_id) => {
            if let Some(function) = unit.function(*function_id) {
                write!(f, "fn {}", function.prototype.signature.display_name())
            } else {
                write!(f, "fn f{}", function_id.index())
            }
        }
        _ => Display::fmt(constant, f),
    }
}

fn write_place_name(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    place: MIRPlace,
) -> fmt::Result {
    match place {
        MIRPlace::FunctionLocal(id) => {
            if let Some(place) = function.place(id) {
                if let Some(name) = &place.debug_name {
                    return Display::fmt(name, f);
                }
            }
            write!(f, "local{}", id.index())
        }
        MIRPlace::Parameter(id) => {
            if let Some(parameter) = function.prototype.signature.params.get(id.index()) {
                if let Some(name) = &parameter.name {
                    return Display::fmt(name, f);
                }
            }
            write!(f, "arg{}", id.index())
        }
        MIRPlace::Global(id) => {
            if let Some(global) = unit.global(id) {
                return Display::fmt(&global.name, f);
            }
            write!(f, "global{}", id.index())
        }
    }
}

fn write_register_name(
    f: &mut Formatter<'_>,
    function: &MIRFunction,
    register: crate::expr::MIRRegister,
) -> fmt::Result {
    if let Some(register_decl) = function.register(register) {
        if let Some(name) = &register_decl.debug_name {
            return Display::fmt(name, f);
        }
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