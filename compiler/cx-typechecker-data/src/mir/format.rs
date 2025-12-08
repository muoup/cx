use crate::mir::expression::{
    MIRBinOp, MIRCoercion, MIRInstruction, MIRRegister, MIRUnOp, MIRValue,
};
use crate::mir::program::{
    MIRBasicBlock, MIRFunction, MIRGlobalVarKind, MIRGlobalVariable, MIRUnit,
};
use crate::mir::types::{
    CXFloatType, CXFunctionPrototype, CXIntegerType, CXType, CXTypeKind, TCParameter,
};
use std::fmt::{Display, Formatter};

impl Display for MIRUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "MIR Unit:")?;

        writeln!(f, "\nFunction Prototypes:")?;
        for prototype in &self.prototypes {
            writeln!(f, "{prototype}")?;
        }

        writeln!(f, "\nFunctions:")?;
        for function in &self.functions {
            writeln!(f, "{function}")?;
        }

        writeln!(f, "\nEnd of MIR Unit")?;
        for global in &self.global_variables {
            writeln!(f, "{global}")?;
        }

        Ok(())
    }
}

impl Display for MIRFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.prototype)?;

        for block in &self.basic_blocks {
            writeln!(f, "{block}")?;
        }
        
        Ok(())
    }
}

impl Display for MIRBasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.id)?;
        for instruction in &self.instructions {
            writeln!(f, "\t{instruction}")?;
        }
        Ok(())
    }
}

impl Display for CXFunctionPrototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.name)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{param}")?;
        }
        if self.var_args {
            if !self.params.is_empty() {
                write!(f, ", ")?;
            }
            write!(f, "...")?;
        }
        write!(f, ") -> {}", self.return_type)
    }
}

impl Display for MIRGlobalVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "global {} ", self.linkage)?;
        write!(f, "{}", self.kind)?;
        write!(
            f,
            " [{}]",
            if self.is_mutable {
                "mutable"
            } else {
                "immutable"
            }
        )?;
        Ok(())
    }
}

impl Display for MIRGlobalVarKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRGlobalVarKind::StringLiteral { name, value } => {
                // do basic sanitization of the string value for display
                let escaped_value = value
                    .replace('\\', "\\\\")
                    .replace('\n', "\\n")
                    .replace('\t', "\\t")
                    .replace('\"', "\\\"");

                write!(f, "string {} = \"{}\"", name, escaped_value)
            }
            MIRGlobalVarKind::Variable {
                name,
                _type,
                initializer,
            } => {
                if let Some(init) = initializer {
                    write!(f, "{} {} = {}", _type, name, init)
                } else {
                    write!(f, "{} {}", _type, name)
                }
            }
        }
    }
}

impl Display for TCParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{name}: {}", self._type)
        } else {
            write!(f, "{}", self._type)
        }
    }
}

impl Display for MIRRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.name)
    }
}

impl Display for MIRInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRInstruction::Alias { result, value } => write!(f, "{result} = alias {value}"),
            MIRInstruction::CreateStackRegion { result, _type } => {
                write!(f, "{result} = create_region {_type}")
            }
            MIRInstruction::CopyRegionInto {
                destination,
                source,
                _type,
            } => write!(f, "copy region {_type} {source} into {destination}"),
            MIRInstruction::MemoryRead {
                result,
                source,
                _type,
            } => write!(f, "{result} = mem_read {source} as {_type}"),
            MIRInstruction::MemoryWrite { target, value } => {
                write!(f, "mem_write {target}, {value}")
            }
            MIRInstruction::StructGet {
                result,
                source,
                field_index,
                struct_type,
                ..
            } => write!(
                f,
                "{result} = struct_get {source}, index {field_index} of {struct_type}"
            ),
            MIRInstruction::TaggedUnionGet {
                result,
                source,
                variant_type,
            } => write!(f, "{result} = tagged_union_get {source} as {variant_type}"),
            MIRInstruction::TaggedUnionIs {
                result,
                source,
                tag_id,
            } => write!(f, "{result} = tagged_union_is {source}, tag {tag_id}"),
            MIRInstruction::ArrayGet {
                result,
                source,
                index,
                ..
            } => write!(f, "{result} = array_get {source}[{index}]"),
            MIRInstruction::CallFunction {
                result,
                function,
                arguments,
            } => {
                if let Some(result) = result {
                    write!(f, "{result} = ")?;
                }
                write!(f, "call {function}(")?;
                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            MIRInstruction::ConstructTaggedUnionInto {
                variant_index,
                memory,
                value,
                sum_type,
            } => write!(
                f,
                "construct_sum {sum_type}::variant({variant_index}) with {value} into {memory}"
            ),
            MIRInstruction::LoopPreHeader {
                loop_id,
                condition_precheck,
                condition_block,
                body_block,
            } => write!(
                f,
                "loop_preheader id {loop_id} condition: {condition_block} body: {body_block} precheck: {condition_precheck}"
            ),
            MIRInstruction::LoopConditionBranch {
                loop_id,
                condition,
                body_block,
                exit_block,
            } => write!(
                f,
                "loop_branch id {loop_id} if {condition} -> {body_block} else -> {exit_block}"
            ),
            MIRInstruction::LoopContinue {
                loop_id,
                condition_block,
            } => {
                write!(f, "loop_continue id {loop_id} to {condition_block}")
            }
            MIRInstruction::Branch {
                condition,
                true_block,
                false_block,
            } => write!(f, "branch {condition} ? {true_block} : {false_block}"),
            MIRInstruction::Phi { result, predecessors: incomings } => {
                write!(f, "{result} = phi ")?;
                for (i, (value, block)) in incomings.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value} from {block}")?;
                }
                Ok(())
            }
            MIRInstruction::Jump { target } => write!(f, "jump {target}"),
            MIRInstruction::JumpTable {
                condition,
                targets,
                default,
            } => {
                write!(f, "jump_table {condition} -> [")?;
                for (i, (val, target)) in targets.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{val} -> {target}")?;
                }
                write!(f, "], default {default}")
            }
            MIRInstruction::Return { value } => {
                write!(f, "return")?;
                if let Some(value) = value {
                    write!(f, " {value}")?;
                }
                Ok(())
            }
            MIRInstruction::BinOp {
                result,
                lhs,
                rhs,
                op,
            } => write!(f, "{result} = binop({op}) {lhs}, {rhs}"),
            MIRInstruction::UnOp {
                result,
                operand,
                op,
            } => write!(f, "{result} = unop({op}) {operand}"),
            MIRInstruction::Coercion {
                result,
                operand,
                cast_type,
            } => write!(f, "{result} = coercion({cast_type}) {operand}"),
            MIRInstruction::Assert { value, message } => write!(f, "assert {value} \"{message}\""),
            MIRInstruction::Assume { value } => write!(f, "assume {value}"),
            MIRInstruction::Havoc { target } => write!(f, "havoc %{target}"),
        }
    }
}

impl Display for MIRValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRValue::IntLiteral {
                value,
                signed,
                _type,
            } => write!(
                f,
                "{}{} {value}",
                if *signed { "i" } else { "u" },
                _type.bytes() * 8
            ),
            MIRValue::FloatLiteral { value, _type } => write!(f, "{}{}", value, _type),
            MIRValue::BoolLiteral { value } => write!(f, "bool {}", if *value { "true" } else { "false" }),
            MIRValue::FunctionReference { prototype, .. } => {
                write!(f, "fn @{}", prototype.name)
            }
            MIRValue::GlobalValue { name, _type } => write!(f, "{_type} global {name}"),
            MIRValue::Parameter { index, _type } => write!(f, "{_type} param {index}"),
            MIRValue::Register { register, _type } => write!(f, "{_type} {register}"),
            MIRValue::NULL => write!(f, "null"),
        }
    }
}

impl Display for MIRBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRBinOp::Float { ftype, op } => write!(f, "f{} {:?}", ftype.bytes() * 8, op),
            MIRBinOp::Integer { itype, op } => write!(f, "i{} {:?}", itype.bytes() * 8, op),
            MIRBinOp::Bool { op } => write!(f, "bool {:?}", op),
            MIRBinOp::PtrDiff { ptr_inner, op } => write!(f, "ptrdiff<{}> {:?}", ptr_inner, op),
            MIRBinOp::Pointer { op } => write!(f, "ptr {:?}", op),
        }
    }
}

impl Display for MIRUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MIRUnOp::NEG => "neg",
                MIRUnOp::INEG => "ineg",
                MIRUnOp::FNEG => "fneg",
                MIRUnOp::BNOT => "bnot",
                MIRUnOp::LNOT => "lnot",
            }
        )
    }
}

impl Display for MIRCoercion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRCoercion::Integral { sextend, to_type } => write!(
                f,
                "integral({}, to: {})",
                if *sextend { "sext" } else { "zext" },
                to_type
            ),
            MIRCoercion::FloatCast { to_type } => write!(f, "fp_integral(to: {})", to_type),
            MIRCoercion::PtrToInt { to_type } => write!(f, "ptr_to_int(to: {})", to_type),
            MIRCoercion::IntToPtr { sextend }=> write!(f, "int_to_ptr({})", if *sextend { "sext" } else { "zext" }),
            MIRCoercion::IntToFloat { to_type, sextend } => write!(
                f,
                "int_to_float({}, to: {})",
                if *sextend { "sext" } else { "zext" },
                to_type
            ),
            MIRCoercion::IntToBool => write!(f, "int_to_bool"),
            MIRCoercion::BoolToInt { to_type } => write!(f, "bool_to_int(to: {})", to_type),
            MIRCoercion::FloatToInt { sextend, to_type } => write!(f, "float_to_int({}, to: {})", if *sextend { "sext" } else { "zext" }, to_type),
            MIRCoercion::ReinterpretBits => write!(f, "reinterpret_bits"),
        }
    }
}

impl Display for CXType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // Here you might want to add specifiers if they are relevant for display
        write!(f, "{}", self.kind)
    }
}

impl Display for CXTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXTypeKind::Integer { _type, signed } => write!(
                f,
                "{}{}",
                if *signed { 'i' } else { 'u' },
                _type.bytes() * 8
            ),
            CXTypeKind::Float { _type } => write!(f, "{}", _type),
            CXTypeKind::Bool => write!(f, "bool"),
            CXTypeKind::Structured { name, fields, .. } => {
                write!(
                    f,
                    "struct {}",
                    name.as_ref()
                        .map(|n| n.to_string())
                        .unwrap_or_else(|| "".to_string())
                )?;
                write!(f, " {{ ")?;
                for (i, (name, _type)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, _type)?;
                }
                write!(f, " }}")
            }
            CXTypeKind::Union { name, variants } => {
                write!(
                    f,
                    "union {}",
                    name.as_ref()
                        .map(|n| n.to_string())
                        .unwrap_or_else(|| "".to_string())
                )?;
                write!(f, " {{ ")?;
                for (i, (name, _type)) in variants.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, _type)?;
                }
                write!(f, " }}")
            }
            CXTypeKind::TaggedUnion { name, variants } => {
                write!(f, "tagged_union {} {{ ", name)?;
                for (i, (name, _type)) in variants.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, _type)?;
                }
                write!(f, " }}")
            }
            CXTypeKind::Unit => write!(f, "()"),
            CXTypeKind::StrongPointer { inner_type, .. } => write!(f, "*strong {}", inner_type),
            CXTypeKind::PointerTo { inner_type, .. } => write!(f, "{}*", inner_type),
            CXTypeKind::MemoryReference(inner) => write!(f, "{}&", inner),
            CXTypeKind::Array { size, inner_type } => write!(f, "[{}; {}]", inner_type, size),
            CXTypeKind::Opaque { name, .. } => write!(f, "opaque {}", name),
            CXTypeKind::Function { prototype } => write!(f, "{prototype}"),
        }
    }
}

impl Display for CXIntegerType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "i{}", self.bytes() * 8)
    }
}

impl Display for CXFloatType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "f{}", self.bytes() * 8)
    }
}
