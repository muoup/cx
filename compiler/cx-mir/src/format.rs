use std::fmt::{self, Display, Formatter};

use cx_util::linkage::LinkageMode;

use crate::{
    expr::{
        instruction::{
            MIRBasicBlock, MIRInstruction, MIRInstructionKind, MIRInvalidationKind,
            MIRStoreBitfield,
        },
        intrinsic::{
            MIRAggregateIntrinsic, MIRFloatIntrinsic, MIRIntIntrinsic, MIRInternalIntrinsic,
            MIRIntrinsic, MIRPtrIntrinsic, MIRVAIntrinsic,
        },
    },
    ty::{
        MIRBitfieldAccess, MIRField, MIRFloatType, MIRIntType, MIRTypeID, MIRTypeKind,
        interface::MTRegistry,
    },
    unit::{
        MIRGlobalID, MIRGlobalState, MIRGlobalVariable, MIRUnit,
        body::MIRBody,
        function::{MIRFnSignature, MIRFunction},
    },
    value::{
        MIRBindable, MIRBlockTarget, MIRConstant, MIRGlobalRef, MIRPlaceID, MIRRegisterID,
        MIRTarget, MIRValue,
    },
};

impl Display for MIRBlockTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.block, f)
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
            Self::GlobalRef(inner) => {
                write!(f, "global {}", inner.global)?;
                if inner.offset != 0 {
                    write!(f, " + {}", inner.offset)?;
                }
                write!(f, ": {}", inner.ty)
            }
            Self::String { value, offset } => {
                write!(f, "{value:?}")?;
                if *offset != 0 {
                    write!(f, " + {offset}")?;
                }
                Ok(())
            }
            Self::Function(function) => write!(f, "fn {function}"),
            Self::BlockAddress { function, label } => write!(f, "label {function}::{label}"),
            Self::Undefined => f.write_str("undefined"),
        }
    }
}

impl Display for MIRGlobalRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "global {}", self.global)?;
        if self.offset != 0 {
            write!(f, " + {}", self.offset)?;
        }
        Ok(())
    }
}

impl Display for MIRValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Register(value) => Display::fmt(value, f),
            Self::PlaceRef(value) => Display::fmt(value, f),
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

impl Display for MIRUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.display_pretty(), f)
    }
}

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

        for (i, global) in self.unit.globals() {
            if i.index() != 0 {
                f.write_str("\n")?;
            }
            write_global(f, global, &mut types)?;
        }

        for (i, function) in self.unit.functions() {
            if i.index() != 0 {
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
            MIRTypeKind::Integer { ty } => write!(f, "i{}", integer_width(*ty)),
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
            MIRTypeKind::IncompleteArray { inner } => {
                f.write_str("[")?;
                self.write(f, *inner)?;
                f.write_str("]")
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

    #[allow(dead_code)]
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

#[allow(dead_code)]
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
        MIRGlobalState::External => f.write_str(";")?,
        MIRGlobalState::ZeroInitialized => f.write_str(" = zero;")?,
        MIRGlobalState::Initialized(value) => {
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
        Some(body) => write_body(f, unit, function, body, types),
        None => f.write_str(";"),
    }
}

fn write_body<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    body: &MIRBody,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    f.write_str(" {\n")?;
    for (id, place) in body.places() {
        f.write_str("    let ")?;
        write_place_name(f, unit, function, id)?;
        f.write_str(": ")?;
        if place.nodrop() {
            f.write_str("@nodrop ")?;
        }
        types.write(f, place.ty())?;
        f.write_str(";\n")?;
    }
    for (_, block) in body.blocks() {
        write_block(f, unit, function, block, types)?;
    }
    f.write_str("}")
}

fn write_block<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    block: &MIRBasicBlock,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    write!(f, "    {}", block.id())?;
    if let Some(name) = &block.debug_name() {
        write!(f, " /* {name} */")?;
    }
    f.write_str(":\n")?;
    for instruction in block.instructions() {
        f.write_str("        ")?;
        write_instruction(f, unit, function, instruction, types)?;
        f.write_str(";\n")?;
    }
    Ok(())
}

fn write_instruction<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    instruction: &MIRInstruction,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    match &instruction.kind {
        MIRInstructionKind::Initialize { place } => {
            f.write_str("initialize ")?;
            write_bindable(f, unit, function, place)
        }
        MIRInstructionKind::Invalidate { place, kind } => {
            match kind {
                MIRInvalidationKind::Leak => f.write_str("leak ")?,
                MIRInvalidationKind::Move => f.write_str("move ")?,
                MIRInvalidationKind::Drop => f.write_str("drop ")?,
            }
            write_bindable(f, unit, function, place)
        }
        MIRInstructionKind::Lift {
            out,
            source,
            origin,
        } => {
            write_definition(f, unit, function, MIRTarget::Register(*out), types)?;
            f.write_str("lift ")?;
            write_bindable(f, unit, function, source)?;
            f.write_str(" from ")?;
            write_bindable(f, unit, function, origin)
        }
        MIRInstructionKind::BindLifetime { bind, bind_to: to } => {
            f.write_str("bind ")?;
            write_bindable(f, unit, function, bind)?;
            f.write_str(" to ")?;
            write_place_name(f, unit, function, *to)
        }
        MIRInstructionKind::Store {
            target,
            value,
            bitfield,
            ..
        } => {
            if let Some(MIRStoreBitfield::Target(access)) = bitfield {
                write_target(f, unit, function, *target)?;
                write_bitfield_access(f, access)?;
                f.write_str(" = ")?;
            } else {
                write_definition(f, unit, function, *target, types)?;
            }
            write_value(f, unit, function, value)?;
            if let Some(MIRStoreBitfield::Source(access)) = bitfield {
                write_bitfield_access(f, access)?;
            }
            Ok(())
        }
        MIRInstructionKind::Call { out, callee, args } => {
            if let Some(out) = out {
                write_definition(f, unit, function, MIRTarget::Register(*out), types)?;
            }
            write_value(f, unit, function, callee)?;
            f.write_str("(")?;
            write_values(f, unit, function, args)?;
            f.write_str(")")
        }
        MIRInstructionKind::IntrinsicOp(intrinsic) => {
            if let MIRIntrinsic::Internal(MIRInternalIntrinsic::AdoptPlace { place, .. }) =
                intrinsic
            {
                write_place_name(f, unit, function, *place)?;
                f.write_str(" = ")?;
            } else if let Some(out) = intrinsic.output_target() {
                write_definition(f, unit, function, out, types)?;
            }
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
            write_block_target(f, target)
        }
        MIRInstructionKind::Branch {
            cond,
            true_target,
            false_target,
        } => {
            f.write_str("if ")?;
            write_value(f, unit, function, cond)?;
            f.write_str(" goto ")?;
            write_block_target(f, true_target)?;
            f.write_str(" else goto ")?;
            write_block_target(f, false_target)
        }
        MIRInstructionKind::CaseBranch {
            value,
            signed,
            cases,
            default,
        } => {
            f.write_str(if *signed { "switch.signed " } else { "switch " })?;
            write_value(f, unit, function, value)?;
            f.write_str(" {")?;
            for (index, (case_value, target)) in cases.iter().enumerate() {
                if index != 0 {
                    f.write_str(",")?;
                }
                write!(f, " {case_value} => ")?;
                write_block_target(f, target)?;
            }
            if let Some(default) = default {
                if !cases.is_empty() {
                    f.write_str(",")?;
                }
                f.write_str(" _ => ")?;
                write_block_target(f, default)?;
            }
            f.write_str(" }")
        }
        MIRInstructionKind::IndirectJump { address, targets } => {
            f.write_str("goto *")?;
            write_value(f, unit, function, address)?;
            f.write_str(" [")?;
            for (index, target) in targets.iter().enumerate() {
                if index != 0 {
                    f.write_str(", ")?;
                }
                write_block_target(f, target)?;
            }
            f.write_str("]")
        }
        MIRInstructionKind::Unreachable => f.write_str("unreachable"),
    }
}

fn write_intrinsic_call<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    types: &mut TypePrinter<'_, T>,
    path: &str,
    args: impl FnOnce(
        &mut Formatter<'_>,
        &MIRUnit,
        &MIRFunction,
        &mut TypePrinter<'_, T>,
    ) -> fmt::Result,
) -> fmt::Result {
    write!(f, "@intrinsic.{path}(")?;
    args(f, unit, function, types)?;
    f.write_str(")")
}

fn write_bitfield_access(f: &mut Formatter<'_>, access: &MIRBitfieldAccess) -> fmt::Result {
    write!(
        f,
        ".bits[{}:{}{}]",
        access.bit_offset,
        access.bit_width,
        if access.signed { ", signed" } else { "" }
    )
}

fn write_intrinsic_unary<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    types: &mut TypePrinter<'_, T>,
    path: &str,
    value: &MIRValue,
) -> fmt::Result {
    write_intrinsic_call(f, unit, function, types, path, |f, unit, function, _| {
        write_value(f, unit, function, value)
    })
}

fn write_intrinsic_binary<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    types: &mut TypePrinter<'_, T>,
    path: &str,
    lhs: &MIRValue,
    rhs: &MIRValue,
) -> fmt::Result {
    write_intrinsic_call(f, unit, function, types, path, |f, unit, function, _| {
        write_value(f, unit, function, lhs)?;
        f.write_str(", ")?;
        write_value(f, unit, function, rhs)
    })
}

fn write_intrinsic_value_type<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    types: &mut TypePrinter<'_, T>,
    path: &str,
    value: &MIRValue,
    target_ty: MIRTypeID,
) -> fmt::Result {
    write_intrinsic_call(
        f,
        unit,
        function,
        types,
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
        MIRIntIntrinsic::Neg { value, .. } => {
            write_intrinsic_unary(f, unit, function, types, "int.neg", value)
        }
        MIRIntIntrinsic::LNot { value, .. } => {
            write_intrinsic_unary(f, unit, function, types, "int.l_not", value)
        }
        MIRIntIntrinsic::BNot { value, .. } => {
            write_intrinsic_unary(f, unit, function, types, "int.b_not", value)
        }
        MIRIntIntrinsic::ToFloat {
            value,
            target,
            signed,
            ..
        } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            if *signed {
                "int.to_float.signed"
            } else {
                "int.to_float.unsigned"
            },
            |f, unit, function, _| {
                write_value(f, unit, function, value)?;
                write!(f, ", f{}", float_width(*target))
            },
        ),
        MIRIntIntrinsic::ToPtr {
            value, sign_extend, ..
        } => write_intrinsic_unary(
            f,
            unit,
            function,
            types,
            if *sign_extend {
                "int.to_ptr.signed"
            } else {
                "int.to_ptr.unsigned"
            },
            value,
        ),
        MIRIntIntrinsic::IntCast {
            value,
            target,
            sign_extend,
            ..
        } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "int.int_cast",
            |f, unit, function, _| {
                write_value(f, unit, function, value)?;
                f.write_str(", ")?;
                write!(f, "{target:?}")?;
                write!(f, ", {sign_extend}")
            },
        ),
        MIRIntIntrinsic::Add { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.add", lhs, rhs)
        }
        MIRIntIntrinsic::Sub { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.sub", lhs, rhs)
        }
        MIRIntIntrinsic::UMul { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_mul", lhs, rhs)
        }
        MIRIntIntrinsic::SMul { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_mul", lhs, rhs)
        }
        MIRIntIntrinsic::UDiv { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_div", lhs, rhs)
        }
        MIRIntIntrinsic::SDiv { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_div", lhs, rhs)
        }
        MIRIntIntrinsic::UMod { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_mod", lhs, rhs)
        }
        MIRIntIntrinsic::SMod { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_mod", lhs, rhs)
        }
        MIRIntIntrinsic::Eq { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.eq", lhs, rhs)
        }
        MIRIntIntrinsic::Neq { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.neq", lhs, rhs)
        }
        MIRIntIntrinsic::ULt { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_lt", lhs, rhs)
        }
        MIRIntIntrinsic::SLt { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_lt", lhs, rhs)
        }
        MIRIntIntrinsic::ULe { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_le", lhs, rhs)
        }
        MIRIntIntrinsic::SLe { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_le", lhs, rhs)
        }
        MIRIntIntrinsic::UGt { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_gt", lhs, rhs)
        }
        MIRIntIntrinsic::SGt { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_gt", lhs, rhs)
        }
        MIRIntIntrinsic::UGe { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.u_ge", lhs, rhs)
        }
        MIRIntIntrinsic::SGe { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.s_ge", lhs, rhs)
        }
        MIRIntIntrinsic::LAnd { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.l_and", lhs, rhs)
        }
        MIRIntIntrinsic::LOr { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.l_or", lhs, rhs)
        }
        MIRIntIntrinsic::BAnd { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.b_and", lhs, rhs)
        }
        MIRIntIntrinsic::BOr { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.b_or", lhs, rhs)
        }
        MIRIntIntrinsic::BXor { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.b_xor", lhs, rhs)
        }
        MIRIntIntrinsic::LShift { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.l_shift", lhs, rhs)
        }
        MIRIntIntrinsic::ARShift { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.ar_shift", lhs, rhs)
        }
        MIRIntIntrinsic::LRShift { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "int.lr_shift", lhs, rhs)
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
        MIRFloatIntrinsic::Neg { value, .. } => {
            write_intrinsic_unary(f, unit, function, types, "float.neg", value)
        }
        MIRFloatIntrinsic::ToInt {
            value,
            target_ty,
            signed,
            ..
        } => write_intrinsic_value_type(
            f,
            unit,
            function,
            types,
            if *signed {
                "float.to_int.signed"
            } else {
                "float.to_int.unsigned"
            },
            value,
            *target_ty,
        ),
        MIRFloatIntrinsic::FloatCast {
            value, float_ty, ..
        } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "float.cast",
            |f, unit, function, _| {
                write_value(f, unit, function, value)?;
                write!(f, ", f{}", float_width(*float_ty))
            },
        ),
        MIRFloatIntrinsic::Add { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "float.add", lhs, rhs)
        }
        MIRFloatIntrinsic::Sub { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "float.sub", lhs, rhs)
        }
        MIRFloatIntrinsic::Mul { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "float.mul", lhs, rhs)
        }
        MIRFloatIntrinsic::Div { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "float.div", lhs, rhs)
        }
        MIRFloatIntrinsic::Eq { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "float.eq", lhs, rhs)
        }
        MIRFloatIntrinsic::Neq { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "float.neq", lhs, rhs)
        }
        MIRFloatIntrinsic::Lt { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "float.lt", lhs, rhs)
        }
        MIRFloatIntrinsic::Le { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "float.le", lhs, rhs)
        }
        MIRFloatIntrinsic::Gt { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "float.gt", lhs, rhs)
        }
        MIRFloatIntrinsic::Geq { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "float.geq", lhs, rhs)
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
        MIRPtrIntrinsic::ToInt { ptr, target_ty, .. } => {
            write_intrinsic_value_type(f, unit, function, types, "pointer.to_int", ptr, *target_ty)
        }
        MIRPtrIntrinsic::Add { ptr, offset, .. } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.add", ptr, offset)
        }
        MIRPtrIntrinsic::Sub { ptr, offset, .. } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.sub", ptr, offset)
        }
        MIRPtrIntrinsic::Diff {
            lhs,
            rhs,
            element_ty,
            ..
        } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "pointer.diff",
            |f, unit, function, types| {
                write_value(f, unit, function, lhs)?;
                f.write_str(", ")?;
                write_value(f, unit, function, rhs)?;
                f.write_str(", ")?;
                types.write(f, *element_ty)
            },
        ),
        MIRPtrIntrinsic::Eq { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.eq", lhs, rhs)
        }
        MIRPtrIntrinsic::Neq { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.neq", lhs, rhs)
        }
        MIRPtrIntrinsic::Lt { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.lt", lhs, rhs)
        }
        MIRPtrIntrinsic::Leq { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.leq", lhs, rhs)
        }
        MIRPtrIntrinsic::Gt { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.gt", lhs, rhs)
        }
        MIRPtrIntrinsic::Geq { lhs, rhs, .. } => {
            write_intrinsic_binary(f, unit, function, types, "pointer.geq", lhs, rhs)
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
        MIRInternalIntrinsic::AdoptPlace { address, .. } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "internal.adopt_place",
            |f, unit, function, _| write_value(f, unit, function, address),
        ),
        MIRInternalIntrinsic::PlaceAddress { place, .. } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "internal.place_address",
            |f, unit, function, _| write_place_name(f, unit, function, *place),
        ),
        MIRInternalIntrinsic::GlobalAddress { global, .. } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "internal.global_address",
            |f, unit, _, _| write_global_ref(f, unit, *global),
        ),
        MIRInternalIntrinsic::ReferenceAddress { reference, .. } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "internal.reference_address",
            |f, unit, function, _| write_value(f, unit, function, reference),
        ),
        MIRInternalIntrinsic::ArrayAddress { array, .. } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "internal.array_address",
            |f, unit, function, _| write_value(f, unit, function, array),
        ),
        MIRInternalIntrinsic::StringAddress { string, .. } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "internal.string_address",
            |f, _, _, _| write!(f, "{string:?}"),
        ),
        MIRInternalIntrinsic::GetFnPtr { fn_id, .. } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "internal.get_fn_ptr",
            |f, _, _, _| Display::fmt(fn_id, f),
        ),
        MIRInternalIntrinsic::Bitcast {
            value, target_ty, ..
        } => write_intrinsic_value_type(
            f,
            unit,
            function,
            types,
            "internal.bitcast",
            value,
            *target_ty,
        ),
        MIRInternalIntrinsic::Assert { condition, message } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
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
            "va.end",
            |f, unit, function, _| write_value(f, unit, function, list),
        ),
        MIRVAIntrinsic::VaArg { list, ty, .. } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
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
        MIRAggregateIntrinsic::SumIndex { value, sum_ty, .. } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "aggregate.sum_index",
            |f, unit, function, types| {
                write_value(f, unit, function, value)?;
                f.write_str(", ")?;
                types.write(f, *sum_ty)
            },
        ),
        MIRAggregateIntrinsic::SumVariant {
            base,
            variant,
            sum_ty,
            ..
        } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "aggregate.sum_variant",
            |f, unit, function, types| {
                write_value(f, unit, function, base)?;
                write!(f, ", {variant}, ")?;
                types.write(f, *sum_ty)
            },
        ),
        MIRAggregateIntrinsic::AggregateInit { ty, fields, .. } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "aggregate.init",
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
            base,
            field,
            struct_ty,
            ..
        } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
            "aggregate.struct_field",
            |f, unit, function, types| {
                write_value(f, unit, function, base)?;
                write!(f, ", {field}, ")?;
                types.write(f, *struct_ty)
            },
        ),
        MIRAggregateIntrinsic::ArrayIndex {
            base,
            index,
            element_ty,
            ..
        } => write_intrinsic_call(
            f,
            unit,
            function,
            types,
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

fn write_block_target(f: &mut Formatter<'_>, target: &MIRBlockTarget) -> fmt::Result {
    Display::fmt(&target.block, f)
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
    if let Some(place) = function.body().map(|body| body.place(place))
        && let Some(name) = place.debug_name()
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
    if let Some(register) = function.body().map(|body| body.register(register))
        && let Some(name) = register.debug_name()
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

fn write_global_ref(f: &mut Formatter<'_>, unit: &MIRUnit, global: MIRGlobalRef) -> fmt::Result {
    write_global_name(f, unit, global.global)?;
    if global.offset != 0 {
        write!(f, " + {}", global.offset)?;
    }
    Ok(())
}

fn write_target(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    target: MIRTarget,
) -> fmt::Result {
    match target {
        MIRTarget::Place(place) => write_place_name(f, unit, function, place),
        MIRTarget::Global(global) => write_global_ref(f, unit, global),
        MIRTarget::Register(register) => write_register_name(f, function, register),
        MIRTarget::Indirect(register) => {
            f.write_str("*")?;
            write_register_name(f, function, register)
        }
    }
}

fn write_definition<T: MTRegistry>(
    f: &mut Formatter<'_>,
    unit: &MIRUnit,
    function: &MIRFunction,
    target: MIRTarget,
    types: &mut TypePrinter<'_, T>,
) -> fmt::Result {
    write_target(f, unit, function, target)?;
    if let MIRTarget::Register(register) = target
        && let Some(body) = function.body()
    {
        f.write_str(" : ")?;
        types.write(f, body.register(register).ty())?;
    }
    f.write_str(" = ")
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
