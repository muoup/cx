use crate::mir::data::{MIRFunctionPrototype, MIRFunctionSignature, MIRParameter};
use crate::mir::expression::{MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRUnOp};
use crate::mir::program::{MIRFunction, MIRGlobalVarKind, MIRGlobalVariable, MIRUnit};
use crate::mir::r#type::{
    MIRFloatType, MIRIntegerType, MIRType, MIRTypeContext, MIRTypeId, MIRTypeKind,
};
use std::fmt::{Display, Formatter};

#[derive(Clone, Copy)]
enum TypeRenderMode {
    Inline,
    Definition,
}

#[derive(Default)]
struct TypeDisplayState {
    active_ids: Vec<MIRTypeId>,
}

impl TypeDisplayState {
    fn contains(&self, id: MIRTypeId) -> bool {
        self.active_ids.contains(&id)
    }

    fn enter(&mut self, id: MIRTypeId) {
        self.active_ids.push(id);
    }

    fn exit(&mut self, id: MIRTypeId) {
        let popped = self.active_ids.pop();
        debug_assert_eq!(popped, Some(id));
    }
}

pub struct MIRTypeDisplay<'a> {
    ty: &'a MIRType,
    definitions: &'a MIRTypeContext,
}

pub struct MIRExpressionDisplay<'a> {
    expr: &'a MIRExpression,
    definitions: &'a MIRTypeContext,
}

pub struct MIRFunctionDisplay<'a> {
    function: &'a MIRFunction,
    definitions: &'a MIRTypeContext,
}

pub struct MIRFunctionSignatureDisplay<'a> {
    signature: &'a MIRFunctionSignature,
    definitions: &'a MIRTypeContext,
}

pub struct MIRFunctionPrototypeDisplay<'a> {
    prototype: &'a MIRFunctionPrototype,
    definitions: &'a MIRTypeContext,
}

pub struct MIRParameterDisplay<'a> {
    parameter: &'a MIRParameter,
    definitions: &'a MIRTypeContext,
}

pub struct MIRGlobalVariableDisplay<'a> {
    global: &'a MIRGlobalVariable,
    definitions: &'a MIRTypeContext,
}

pub struct MIRGlobalVarKindDisplay<'a> {
    kind: &'a MIRGlobalVarKind,
    definitions: &'a MIRTypeContext,
}

pub struct MIRUnitDisplay<'a> {
    unit: &'a MIRUnit,
}

impl MIRType {
    pub fn display_with<'a>(&'a self, definitions: &'a MIRTypeContext) -> MIRTypeDisplay<'a> {
        MIRTypeDisplay {
            ty: self,
            definitions,
        }
    }
}

impl MIRExpression {
    pub fn display_with<'a>(&'a self, definitions: &'a MIRTypeContext) -> MIRExpressionDisplay<'a> {
        MIRExpressionDisplay {
            expr: self,
            definitions,
        }
    }
}

impl MIRFunction {
    pub fn display_with<'a>(&'a self, definitions: &'a MIRTypeContext) -> MIRFunctionDisplay<'a> {
        MIRFunctionDisplay {
            function: self,
            definitions,
        }
    }
}

impl MIRFunctionSignature {
    pub fn display_with<'a>(
        &'a self,
        definitions: &'a MIRTypeContext,
    ) -> MIRFunctionSignatureDisplay<'a> {
        MIRFunctionSignatureDisplay {
            signature: self,
            definitions,
        }
    }
}

impl MIRFunctionPrototype {
    pub fn display_with<'a>(
        &'a self,
        definitions: &'a MIRTypeContext,
    ) -> MIRFunctionPrototypeDisplay<'a> {
        MIRFunctionPrototypeDisplay {
            prototype: self,
            definitions,
        }
    }
}

impl MIRParameter {
    pub fn display_with<'a>(&'a self, definitions: &'a MIRTypeContext) -> MIRParameterDisplay<'a> {
        MIRParameterDisplay {
            parameter: self,
            definitions,
        }
    }
}

impl MIRGlobalVariable {
    pub fn display_with<'a>(
        &'a self,
        definitions: &'a MIRTypeContext,
    ) -> MIRGlobalVariableDisplay<'a> {
        MIRGlobalVariableDisplay {
            global: self,
            definitions,
        }
    }
}

impl MIRGlobalVarKind {
    pub fn display_with<'a>(
        &'a self,
        definitions: &'a MIRTypeContext,
    ) -> MIRGlobalVarKindDisplay<'a> {
        MIRGlobalVarKindDisplay {
            kind: self,
            definitions,
        }
    }
}

impl MIRUnit {
    pub fn display_pretty(&self) -> MIRUnitDisplay<'_> {
        MIRUnitDisplay { unit: self }
    }
}

fn indentation(depth: usize) -> String {
    "  ".repeat(depth)
}

fn type_id_of(definitions: &MIRTypeContext, ty: &MIRType) -> Option<MIRTypeId> {
    ty.named_type_id(definitions)
        .or_else(|| definitions.type_id(ty))
}

fn should_inline_as_reference(ty: &MIRType, id: Option<MIRTypeId>) -> bool {
    matches!(
        ty.kind,
        MIRTypeKind::Structured { .. }
            | MIRTypeKind::Union { .. }
            | MIRTypeKind::TaggedUnion { .. }
            | MIRTypeKind::Undefined
    ) && (ty.get_name().is_some() || id.is_some())
}

fn should_emit_type_definition(ty: &MIRType) -> bool {
    ty.get_name().is_some()
        || matches!(
            ty.kind,
            MIRTypeKind::Structured { .. }
                | MIRTypeKind::Union { .. }
                | MIRTypeKind::TaggedUnion { .. }
                | MIRTypeKind::Undefined
        )
}

fn write_type_reference(
    f: &mut Formatter<'_>,
    ty: &MIRType,
    id: Option<MIRTypeId>,
) -> std::fmt::Result {
    if let Some(name) = ty.debug_name().or_else(|| ty.get_name()) {
        write!(f, "{name}")
    } else if let Some(id) = id {
        write!(f, "{id}")
    } else {
        match &ty.kind {
            MIRTypeKind::Structured { .. } => write!(f, "struct"),
            MIRTypeKind::Union { .. } => write!(f, "union"),
            MIRTypeKind::TaggedUnion { .. } => write!(f, "tagged_union"),
            MIRTypeKind::Undefined => write!(f, "undefined"),
            _ => unreachable!("type reference requested for non-referenceable type"),
        }
    }
}

fn write_recursive_reference(
    f: &mut Formatter<'_>,
    ty: &MIRType,
    id: MIRTypeId,
) -> std::fmt::Result {
    if let Some(name) = ty.debug_name().or_else(|| ty.get_name()) {
        write!(f, "{name}")
    } else {
        write!(f, "{id}<recursive>")
    }
}

fn write_type_root(
    f: &mut Formatter<'_>,
    definitions: &MIRTypeContext,
    ty: &MIRType,
    mode: TypeRenderMode,
) -> std::fmt::Result {
    let mut state = TypeDisplayState::default();
    write_type_value(f, definitions, ty, mode, &mut state)
}

fn write_type_value(
    f: &mut Formatter<'_>,
    definitions: &MIRTypeContext,
    ty: &MIRType,
    mode: TypeRenderMode,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    let id = type_id_of(definitions, ty);
    if let Some(id) = id {
        if state.contains(id) {
            return write_recursive_reference(f, ty, id);
        }
        state.enter(id);
        let result = write_type_body(f, definitions, ty, id, mode, state);
        state.exit(id);
        result
    } else {
        write_type_body(f, definitions, ty, MIRTypeId(0), mode, state)
    }
}

fn write_type_id(
    f: &mut Formatter<'_>,
    definitions: &MIRTypeContext,
    id: MIRTypeId,
    mode: TypeRenderMode,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    let Some(ty) = definitions.get(id) else {
        return write!(f, "{id}<unknown>");
    };

    if state.contains(id) {
        return write_recursive_reference(f, ty, id);
    }

    state.enter(id);
    let result = write_type_body(f, definitions, ty, id, mode, state);
    state.exit(id);
    result
}

fn write_aggregate(
    f: &mut Formatter<'_>,
    keyword: &str,
    ty: &MIRType,
    fields: &[(String, MIRTypeId)],
    definitions: &MIRTypeContext,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    write!(f, "{keyword}")?;
    if let Some(name) = ty.debug_name().or_else(|| ty.get_name()) {
        write!(f, " {name}")?;
    }
    write!(f, " {{")?;
    if !fields.is_empty() {
        write!(f, " ")?;
        for (idx, (field_name, field_id)) in fields.iter().enumerate() {
            if idx > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{field_name}: ")?;
            write_type_id(f, definitions, *field_id, TypeRenderMode::Inline, state)?;
        }
        write!(f, " ")?;
    }
    write!(f, "}}")
}

fn write_type_body(
    f: &mut Formatter<'_>,
    definitions: &MIRTypeContext,
    ty: &MIRType,
    id: MIRTypeId,
    mode: TypeRenderMode,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    let display_id = (id.0 != 0).then_some(id);

    if matches!(mode, TypeRenderMode::Inline) && should_inline_as_reference(ty, display_id) {
        return write_type_reference(f, ty, display_id);
    }

    match &ty.kind {
        MIRTypeKind::Integer { _type, signed } => {
            write!(f, "{}{}", if *signed { 'i' } else { 'u' }, _type)
        }
        MIRTypeKind::Float { _type } => write!(f, "{_type}"),
        MIRTypeKind::Structured { fields } => {
            write_aggregate(f, "struct", ty, fields, definitions, state)
        }
        MIRTypeKind::Union { variants } => {
            write_aggregate(f, "union", ty, variants, definitions, state)
        }
        MIRTypeKind::TaggedUnion { variants } => {
            write_aggregate(f, "tagged_union", ty, variants, definitions, state)
        }
        MIRTypeKind::Unit => write!(f, "()"),
        MIRTypeKind::PointerTo { inner_type } => {
            write_type_id(f, definitions, *inner_type, TypeRenderMode::Inline, state)?;
            write!(f, "*")
        }
        MIRTypeKind::MemoryReference { inner_type } => {
            write_type_id(f, definitions, *inner_type, TypeRenderMode::Inline, state)?;
            write!(f, "&")
        }
        MIRTypeKind::Array {
            length: size,
            inner_type,
        } => {
            write!(f, "[")?;
            write_type_id(f, definitions, *inner_type, TypeRenderMode::Inline, state)?;
            write!(f, "; {size}]")
        }
        MIRTypeKind::Opaque { size } => write!(f, "opaque({size})"),
        MIRTypeKind::Undefined => {
            if matches!(mode, TypeRenderMode::Definition) {
                write!(f, "undefined")?;
                if let Some(name) = ty.debug_name().or_else(|| ty.get_name()) {
                    write!(f, " {name}")?;
                }
                Ok(())
            } else {
                write_type_reference(f, ty, display_id)
            }
        }
        MIRTypeKind::Str => write!(f, "_str"),
        MIRTypeKind::Function { signature } => {
            write_signature_with_context(f, signature, definitions, state)
        }
    }
}

fn write_type_definitions(f: &mut Formatter<'_>, definitions: &MIRTypeContext) -> std::fmt::Result {
    writeln!(f, "\nType Definitions:")?;

    let mut any = false;
    for (idx, ty) in definitions.types.iter().enumerate() {
        if !should_emit_type_definition(ty) {
            continue;
        }

        any = true;
        let id = MIRTypeId(idx as u64 + 1);
        writeln!(
            f,
            "{} = {}",
            id,
            MIRTypeDefinitionDisplay { ty, definitions }
        )?;
    }

    if !any {
        writeln!(f, "(none)")?;
    }

    Ok(())
}

struct MIRTypeDefinitionDisplay<'a> {
    ty: &'a MIRType,
    definitions: &'a MIRTypeContext,
}

impl Display for MIRTypeDefinitionDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write_type_root(f, self.definitions, self.ty, TypeRenderMode::Definition)
    }
}

impl Display for MIRTypeDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write_type_root(f, self.definitions, self.ty, TypeRenderMode::Inline)
    }
}

impl Display for MIRExpressionDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        MIRExpressionFormatter::with_definitions(self.expr, 0, self.definitions).fmt(f)
    }
}

impl Display for MIRFunctionDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{}\nBody:",
            self.function.prototype.display_with(self.definitions)
        )?;
        MIRExpressionFormatter::with_definitions(&self.function.body, 1, self.definitions).fmt(f)
    }
}

impl Display for MIRFunctionSignatureDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write_signature_with_context(
            f,
            self.signature,
            self.definitions,
            &mut TypeDisplayState::default(),
        )
    }
}

impl Display for MIRFunctionPrototypeDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}",
            self.prototype.name,
            self.prototype.signature().display_with(self.definitions)
        )
    }
}

impl Display for MIRParameterDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.parameter.name {
            write!(
                f,
                "{name}: {}",
                self.parameter._type.display_with(self.definitions)
            )
        } else {
            write!(f, "{}", self.parameter._type.display_with(self.definitions))
        }
    }
}

impl Display for MIRGlobalVariableDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "global {} ", self.global.linkage)?;
        write!(f, "{}", self.global.kind.display_with(self.definitions))?;
        write!(
            f,
            " [{}]",
            if self.global.is_mutable {
                "mutable"
            } else {
                "immutable"
            }
        )?;
        Ok(())
    }
}

impl Display for MIRGlobalVarKindDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            MIRGlobalVarKind::StringLiteral { name, value } => {
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
                    write!(
                        f,
                        "{} {} = {}",
                        _type.display_with(self.definitions),
                        name,
                        init
                    )
                } else {
                    write!(f, "{} {}", _type.display_with(self.definitions), name)
                }
            }
        }
    }
}

impl Display for MIRUnitDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "MIR Unit:")?;

        writeln!(f, "\nFunction Prototypes:")?;
        for prototype in &self.unit.prototypes {
            writeln!(f, "{}", prototype.display_with(&self.unit.type_definitions))?;
        }

        write_type_definitions(f, &self.unit.type_definitions)?;

        writeln!(f, "\nFunctions:")?;
        for function in &self.unit.functions {
            writeln!(f, "{}", function.display_with(&self.unit.type_definitions))?;
        }

        writeln!(f, "\nGlobal Variables:")?;
        for global in &self.unit.global_variables {
            writeln!(f, "{}", global.display_with(&self.unit.type_definitions))?;
        }

        writeln!(f, "\nEnd of MIR Unit")
    }
}

fn write_signature_with_context(
    f: &mut Formatter<'_>,
    signature: &MIRFunctionSignature,
    definitions: &MIRTypeContext,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    write!(f, "fn(")?;
    for (i, param) in signature.params.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        if let Some(name) = &param.name {
            write!(f, "{name}: ")?;
        }
        write_type_value(f, definitions, &param._type, TypeRenderMode::Inline, state)?;
    }
    if signature.var_args {
        if !signature.params.is_empty() {
            write!(f, ", ")?;
        }
        write!(f, "...")?;
    }
    write!(f, ") -> ")?;
    write_type_value(
        f,
        definitions,
        &signature.return_type,
        TypeRenderMode::Inline,
        state,
    )?;

    Ok(())
}

struct MIRExpressionFormatter<'a> {
    expr: &'a MIRExpression,
    depth: usize,
    definitions: &'a MIRTypeContext,
}

impl<'a> MIRExpressionFormatter<'a> {
    fn with_definitions(
        expr: &'a MIRExpression,
        depth: usize,
        definitions: &'a MIRTypeContext,
    ) -> Self {
        Self {
            expr,
            depth,
            definitions,
        }
    }

    fn indent(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", indentation(self.depth))
    }

    fn write_type(&self, f: &mut Formatter<'_>, ty: &MIRType) -> std::fmt::Result {
        write!(f, "{}", ty.display_with(self.definitions))
    }

    fn write_bin_op(&self, f: &mut Formatter<'_>, op: &MIRBinOp) -> std::fmt::Result {
        match op {
            MIRBinOp::Float { ftype, op } => write!(f, "f{} {:?}", ftype.bytes() * 8, op),
            MIRBinOp::Integer { itype, op } => write!(f, "i{} {:?}", itype.bytes() * 8, op),
            MIRBinOp::PtrDiff { ptr_inner, op } => {
                write!(
                    f,
                    "ptrdiff<{}> {:?}",
                    ptr_inner.display_with(self.definitions),
                    op
                )
            }
            MIRBinOp::Pointer { op } => write!(f, "ptr {:?}", op),
        }
    }
}

impl<'a> Display for MIRExpressionFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.indent(f)?;
        match &self.expr.kind {
            MIRExpressionKind::BoolLiteral(value) => {
                write!(f, "BoolLiteral {} <'", value)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            MIRExpressionKind::IntLiteral(value, int_type, signed) => {
                let prefix = if *signed { "i" } else { "u" };
                write!(
                    f,
                    "IntLiteral {}{}:{} <'",
                    prefix,
                    int_type.bytes() * 8,
                    value
                )?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            MIRExpressionKind::FloatLiteral(value, float_type) => {
                write!(f, "FloatLiteral f{}:{} <'", float_type.bytes() * 8, value)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            MIRExpressionKind::Unit => {
                write!(f, "Unit <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            MIRExpressionKind::Variable(name) => {
                write!(f, "LocalVariable {} <'", name)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            MIRExpressionKind::ContractVariable { name, .. } => {
                write!(f, "ContractVariable \"{name}\" <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            MIRExpressionKind::FunctionReference { name } => {
                write!(f, "FunctionReference {name} <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            MIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
                write!(f, "BinaryOperation ")?;
                self.write_bin_op(f, op)?;
                write!(f, " <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: lhs,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                MIRExpressionFormatter {
                    expr: rhs,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::UnaryOperation { operand, op } => {
                write!(f, "UnaryOperation {} <'", op)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: operand,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::MemoryWrite { target, value } => {
                write!(f, "MemoryWrite <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: target,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                MIRExpressionFormatter {
                    expr: value,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::Typechange(expression) => {
                write!(f, "Typechange <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: expression,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::RegionCreate {
                name,
                _type,
                initial_value,
            } => {
                let name_str = name.as_ref().map(|t| t.as_str()).unwrap_or("(unnamed)");
                write!(f, "CreateStackVariable {}: ", name_str)?;
                self.write_type(f, _type)?;
                write!(f, " <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;

                if let Some(init) = initial_value {
                    MIRExpressionFormatter {
                        expr: init,
                        depth: self.depth + 1,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                } else {
                    self.indent(f)?;
                    writeln!(f, "(no initializer)")?;
                }

                Ok(())
            }
            MIRExpressionKind::RegionDuplicate { source } => {
                write!(f, "RegionDuplicate")?;
                write!(f, " <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: source,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::StructFieldAccess {
                base,
                field_index,
                field_offset,
                struct_type,
                ..
            } => {
                write!(f, "StructFieldAccess [")?;
                self.write_type(f, struct_type)?;
                write!(f, "] index {field_index} (+{field_offset}) <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: base,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::UnionAliasAccess {
                base,
                variant_type,
                union_type,
            } => {
                write!(f, "UnionAliasAccess [")?;
                self.write_type(f, variant_type)?;
                write!(f, " as ")?;
                self.write_type(f, union_type)?;
                write!(f, "] <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: base,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::ArrayAccess { array, index, .. } => {
                write!(f, "ArrayAccess <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: array,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                MIRExpressionFormatter {
                    expr: index,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::PatternIs {
                lhs,
                sum_type,
                variant_index,
                inner_name,
            } => {
                write!(f, "PatternIs variant {} of ", variant_index)?;
                self.write_type(f, sum_type)?;
                write!(f, " (create {}) <'", inner_name)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: lhs,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::TaggedUnionTag { value, .. } => {
                write!(f, "TaggedUnionTag <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: value,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::TaggedUnionGet {
                value,
                variant_type,
            } => {
                write!(f, "TaggedUnionGet [")?;
                self.write_type(f, variant_type)?;
                write!(f, "] <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: value,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::TaggedUnionSet {
                target,
                variant_index,
                inner_value,
                ..
            } => {
                write!(f, "TaggedUnionSet variant {} <'", variant_index)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: target,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                MIRExpressionFormatter {
                    expr: inner_value,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::ConstructTaggedUnion {
                variant_index,
                value,
                ..
            } => {
                write!(f, "ConstructTaggedUnion variant {} <'", variant_index)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: value,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::StructInitializer {
                initializations,
                struct_type,
            } => {
                write!(f, "StructInitializer ")?;
                self.write_type(f, struct_type)?;
                write!(f, " {{ <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                for initializer in initializations {
                    self.indent(f)?;
                    writeln!(
                        f,
                        "  Field {} (+{}):",
                        initializer.field_index, initializer.field_offset
                    )?;
                    MIRExpressionFormatter {
                        expr: &initializer.value,
                        depth: self.depth + 2,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }
                self.indent(f)?;
                writeln!(f, "}}")
            }
            MIRExpressionKind::ArrayInitializer {
                elements,
                element_type,
            } => {
                write!(f, "ArrayInitializer ")?;
                self.write_type(f, element_type)?;
                write!(f, " [ <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                for element in elements {
                    MIRExpressionFormatter {
                        expr: element,
                        depth: self.depth + 1,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }
                self.indent(f)?;
                writeln!(f, "]")
            }
            MIRExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "If <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: condition,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                MIRExpressionFormatter {
                    expr: then_branch,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                if let Some(else_branch) = else_branch {
                    MIRExpressionFormatter {
                        expr: else_branch,
                        depth: self.depth + 1,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }
                Ok(())
            }
            MIRExpressionKind::While {
                condition,
                body,
                pre_eval,
            } => {
                let name = if *pre_eval { "While" } else { "Do-While" };
                write!(f, "{} <'", name)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: condition,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                MIRExpressionFormatter {
                    expr: body,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                write!(f, "For <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: init,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                MIRExpressionFormatter {
                    expr: condition,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                MIRExpressionFormatter {
                    expr: increment,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                MIRExpressionFormatter {
                    expr: body,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::CSwitch {
                condition,
                cases,
                default,
            } => {
                write!(f, "CSwitch <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: condition,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                for (case_value, label) in cases {
                    self.indent(f)?;
                    writeln!(f, "Case:")?;
                    MIRExpressionFormatter {
                        expr: case_value,
                        depth: self.depth + 2,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                    MIRExpressionFormatter {
                        expr: label,
                        depth: self.depth + 2,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }
                if let Some(default) = default {
                    self.indent(f)?;
                    writeln!(f, "Default:")?;
                    MIRExpressionFormatter {
                        expr: default,
                        depth: self.depth + 2,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }
                Ok(())
            }
            MIRExpressionKind::Match {
                condition,
                arms,
                default,
            } => {
                write!(f, "Match <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: condition,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                for (pattern, arm_body) in arms {
                    self.indent(f)?;
                    writeln!(f, "Arm:")?;
                    MIRExpressionFormatter {
                        expr: pattern,
                        depth: self.depth + 2,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                    MIRExpressionFormatter {
                        expr: arm_body,
                        depth: self.depth + 2,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }
                if let Some(default) = default {
                    self.indent(f)?;
                    writeln!(f, "Default:")?;
                    MIRExpressionFormatter {
                        expr: default,
                        depth: self.depth + 2,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }
                Ok(())
            }
            MIRExpressionKind::Return {
                value,
                postcondition,
            } => {
                write!(f, "Return <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                if let Some(value) = value {
                    MIRExpressionFormatter {
                        expr: value,
                        depth: self.depth + 1,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }
                if let Some((name, postcondition)) = postcondition {
                    self.indent(f)?;
                    write!(f, " ++ Postcondition")?;

                    if let Some(name) = name {
                        write!(f, "({})", name)?;
                    }

                    writeln!(f, ":")?;

                    MIRExpressionFormatter {
                        expr: postcondition,
                        depth: self.depth + 2,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }
                Ok(())
            }
            MIRExpressionKind::Block { statements } => {
                write!(f, "Block {{ <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                for stmt in statements {
                    MIRExpressionFormatter {
                        expr: stmt,
                        depth: self.depth + 1,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }
                self.indent(f)?;
                writeln!(f, "}}")
            }
            MIRExpressionKind::CallFunction {
                function,
                arguments,
                contract,
            } => {
                write!(f, "CallFunction <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: function,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                for arg in arguments {
                    MIRExpressionFormatter {
                        expr: arg,
                        depth: self.depth + 1,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }

                if let Some(precondition) = contract.precondition.as_ref() {
                    self.indent(f)?;
                    writeln!(f, "Precondition:")?;
                    MIRExpressionFormatter {
                        expr: precondition,
                        depth: self.depth + 2,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }

                if let Some((binding, postcondition)) = contract.postcondition.as_ref() {
                    self.indent(f)?;
                    write!(f, " ++ Postcondition")?;

                    if let Some(binding) = binding {
                        self.indent(f)?;
                        write!(f, "(binding: {binding})")?;
                    }

                    writeln!(f, ":")?;

                    MIRExpressionFormatter {
                        expr: postcondition,
                        depth: self.depth + 2,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }

                Ok(())
            }
            MIRExpressionKind::TypeConversion {
                operand,
                conversion,
            } => {
                write!(f, "TypeConversion {}", conversion)?;
                write!(f, " <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: operand,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::LifetimeStart { variable, _type } => {
                write!(f, "LifetimeStart {} (", variable)?;
                self.write_type(f, _type)?;
                write!(f, ") <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            MIRExpressionKind::LifetimeEnd { variable, _type } => {
                write!(f, "LifetimeEnd {} (", variable)?;
                self.write_type(f, _type)?;
                write!(f, ") <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            MIRExpressionKind::LeakLifetime { expression } => {
                write!(f, "LeakLifetime <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: expression,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::Unsafe { expression } => {
                write!(f, "Unsafe <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: expression,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::RegionMove { source } => {
                write!(f, "Move <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: source,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            MIRExpressionKind::Break { scope_depth } => {
                write!(f, "Break <scope_depth={}, type='", scope_depth)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, "'>")
            }
            MIRExpressionKind::Continue { scope_depth } => {
                write!(f, "Continue <scope_depth={}, type='", scope_depth)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, "'>")
            }
        }
    }
}

impl Display for MIRUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRUnOp::NEG => write!(f, "neg"),
            MIRUnOp::INEG => write!(f, "ineg"),
            MIRUnOp::FNEG => write!(f, "fneg"),
            MIRUnOp::BNOT => write!(f, "bnot"),
            MIRUnOp::LNOT => write!(f, "lnot"),
            MIRUnOp::PreIncrement(amt) => write!(f, "pre_increment({})", amt),
            MIRUnOp::PostIncrement(amt) => write!(f, "post_increment({})", amt),
        }
    }
}

impl Display for MIRCoercion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRCoercion::Integral {
                sextend,
                from_type,
                to_type,
            } => write!(
                f,
                "integral({}, {} -> {})",
                from_type,
                if *sextend { "sext" } else { "zext" },
                to_type
            ),
            MIRCoercion::FloatCast { to_type } => write!(f, "fp_integral(to: {})", to_type),
            MIRCoercion::PtrToInt { to_type } => write!(f, "ptr_to_int(to: {})", to_type),
            MIRCoercion::IntToPtr { sextend } => {
                write!(f, "int_to_ptr({})", if *sextend { "sext" } else { "zext" })
            }
            MIRCoercion::IntToFloat { to_type, sextend } => write!(
                f,
                "int_to_float({}, to: {})",
                if *sextend { "sext" } else { "zext" },
                to_type
            ),
            MIRCoercion::FloatToInt { sextend, to_type } => write!(
                f,
                "float_to_int({}, to: {})",
                if *sextend { "sext" } else { "zext" },
                to_type
            ),
            MIRCoercion::GetFnPtr => write!(f, "get_fn_ptr"),

            MIRCoercion::ReinterpretBits => write!(f, "reinterpret_bits"),
            MIRCoercion::Typechange => write!(f, "typechange"),
        }
    }
}

impl Display for MIRTypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "T{}", self.0)
    }
}

impl Display for MIRFloatType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRFloatType::F32 => write!(f, "f32"),
            MIRFloatType::F64 => write!(f, "f64"),
        }
    }
}

impl Display for MIRIntegerType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRIntegerType::I1 => write!(f, "1"),
            MIRIntegerType::I8 => write!(f, "8"),
            MIRIntegerType::I16 => write!(f, "16"),
            MIRIntegerType::I32 => write!(f, "32"),
            MIRIntegerType::I64 => write!(f, "64"),
            MIRIntegerType::I128 => write!(f, "128"),
        }
    }
}
