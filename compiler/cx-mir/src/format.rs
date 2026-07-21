use cx_ast::ast::modifiers::{CX_CONST, CX_RESTRICT, CX_VOLATILE, CXTypeQualifiers};
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use crate::mir::data::{MIRFunctionPrototype, MIRFunctionSignature, MIRParameter};
use crate::mir::expression::{MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRUnOp};
use crate::mir::global::{MIRGlobalVarKind, MIRGlobalVariable};
use crate::mir::r#type::{MIRField, MIRFloatType, MIRIntegerType, MIRType, MIRTypeId, MIRTypeKind};
use crate::type_context::MIRTypeContext;
use crate::{MIRFunction, MIRUnit};
use std::fmt::{Display, Formatter};

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

pub struct MIRDisplay<'a, T: ?Sized> {
    content: &'a T,
    definitions: &'a dyn MIRTypeContext,
}

trait MIRDisplayable {
    fn display_with_definitions<'a>(
        &'a self,
        definitions: &'a dyn MIRTypeContext,
    ) -> MIRDisplay<'a, Self>
    where
        Self: Sized,
    {
        MIRDisplay {
            content: self,
            definitions,
        }
    }
}

impl MIRDisplayable for MIRType {}
impl MIRDisplayable for MIRExpression {}
impl MIRDisplayable for MIRFunction {}
impl MIRDisplayable for MIRFunctionSignature {}
impl MIRDisplayable for MIRFunctionPrototype {}
impl MIRDisplayable for MIRParameter {}
impl MIRDisplayable for MIRGlobalVariable {}
impl MIRDisplayable for MIRGlobalVarKind {}

impl MIRType {
    pub fn display_with<'a>(&'a self, definitions: &'a dyn MIRTypeContext) -> MIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl MIRExpression {
    pub fn display_with<'a>(&'a self, definitions: &'a dyn MIRTypeContext) -> MIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl MIRFunction {
    pub fn display_with<'a>(&'a self, definitions: &'a dyn MIRTypeContext) -> MIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl MIRFunctionSignature {
    pub fn display_with<'a>(&'a self, definitions: &'a dyn MIRTypeContext) -> MIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl MIRFunctionPrototype {
    pub fn display_with<'a>(&'a self, definitions: &'a dyn MIRTypeContext) -> MIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl MIRParameter {
    pub fn display_with<'a>(&'a self, definitions: &'a dyn MIRTypeContext) -> MIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl MIRGlobalVariable {
    pub fn display_with<'a>(&'a self, definitions: &'a dyn MIRTypeContext) -> MIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl MIRGlobalVarKind {
    pub fn display_with<'a>(&'a self, definitions: &'a dyn MIRTypeContext) -> MIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl MIRUnit {
    pub fn display_pretty(&self) -> MIRDisplay<'_, Self> {
        MIRDisplay {
            content: self,
            definitions: &self.registry,
        }
    }
}

fn indentation(depth: usize) -> String {
    "  ".repeat(depth)
}

fn write_type_reference(
    f: &mut Formatter<'_>,
    definitions: &dyn MIRTypeContext,
    ty: &MIRType,
    id: Option<MIRTypeId>,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    if write_type_name(f, definitions, ty, id, state)? {
        Ok(())
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
    definitions: &dyn MIRTypeContext,
    ty: &MIRType,
    id: MIRTypeId,
) -> std::fmt::Result {
    if write_type_base_name(f, definitions, ty, Some(id))? {
        Ok(())
    } else {
        write!(f, "{id}<recursive>")
    }
}

fn write_type_name(
    f: &mut Formatter<'_>,
    definitions: &dyn MIRTypeContext,
    ty: &MIRType,
    id: Option<MIRTypeId>,
    state: &mut TypeDisplayState,
) -> Result<bool, std::fmt::Error> {
    if !write_type_base_name(f, definitions, ty, id)? {
        return Ok(false);
    }

    let Some(template_info) = ty.get_template_data() else {
        return Ok(true);
    };

    if template_info.template_input.args.is_empty() {
        return Ok(true);
    }

    write!(f, "<")?;
    for (idx, arg) in template_info.template_input.args.iter().enumerate() {
        if idx > 0 {
            write!(f, ", ")?;
        }
        write_type_id(f, definitions, *arg, state)?;
    }
    write!(f, ">")?;

    Ok(true)
}

fn write_type_base_name(
    f: &mut Formatter<'_>,
    definitions: &dyn MIRTypeContext,
    ty: &MIRType,
    id: Option<MIRTypeId>,
) -> Result<bool, std::fmt::Error> {
    if let Some(name) = ty
        .get_template_data()
        .and_then(|template_info| template_info.base_name.as_ref())
        .or_else(|| ty.lookup_identifier())
        .or_else(|| id.and_then(|id| definitions.type_id_lookup_identifier(id)))
    {
        write_qualified_name(f, name)?;
        return Ok(true);
    }

    Ok(false)
}

fn has_type_name(definitions: &dyn MIRTypeContext, ty: &MIRType, id: Option<MIRTypeId>) -> bool {
    ty.get_template_data()
        .and_then(|template_info| template_info.base_name.as_ref())
        .or_else(|| ty.lookup_identifier())
        .or_else(|| id.and_then(|id| definitions.type_id_lookup_identifier(id)))
        .is_some()
}

fn write_qualified_name(f: &mut Formatter<'_>, name: &QualifiedName) -> std::fmt::Result {
    write!(f, "{name}")
}

fn write_type_root(
    f: &mut Formatter<'_>,
    definitions: &dyn MIRTypeContext,
    ty: &MIRType,
) -> std::fmt::Result {
    let mut state = TypeDisplayState::default();
    write_type_value(f, definitions, ty, &mut state)
}

fn write_type_value(
    f: &mut Formatter<'_>,
    definitions: &dyn MIRTypeContext,
    ty: &MIRType,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    write_type_body(f, definitions, ty, None, state)
}

fn write_type_id(
    f: &mut Formatter<'_>,
    definitions: &dyn MIRTypeContext,
    id: MIRTypeId,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    let ty = definitions.resolve_type_id(id);

    if state.contains(id) {
        return write_recursive_reference(f, definitions, ty, id);
    }

    state.enter(id);
    let result = write_type_body(f, definitions, ty, Some(id), state);
    state.exit(id);
    result
}

fn write_type_qualifiers_prefix(
    f: &mut Formatter<'_>,
    specifiers: CXTypeQualifiers,
) -> std::fmt::Result {
    if specifiers & CX_CONST != 0 {
        write!(f, "const ")?;
    }
    if specifiers & CX_VOLATILE != 0 {
        write!(f, "volatile ")?;
    }
    if specifiers & CX_RESTRICT != 0 {
        write!(f, "restrict ")?;
    }

    Ok(())
}

fn write_type_qualifiers_suffix(
    f: &mut Formatter<'_>,
    specifiers: CXTypeQualifiers,
) -> std::fmt::Result {
    if specifiers & CX_CONST != 0 {
        write!(f, " const")?;
    }
    if specifiers & CX_VOLATILE != 0 {
        write!(f, " volatile")?;
    }
    if specifiers & CX_RESTRICT != 0 {
        write!(f, " restrict")?;
    }

    Ok(())
}

fn write_aggregate(
    f: &mut Formatter<'_>,
    keyword: &str,
    ty: &MIRType,
    fields: &[MIRField],
    definitions: &dyn MIRTypeContext,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    write!(f, "{keyword}")?;
    if has_type_name(definitions, ty, None) {
        write!(f, " ")?;
        write_type_name(f, definitions, ty, None, state)?;
    }
    write!(f, " {{")?;
    if !fields.is_empty() {
        write!(f, " ")?;
        for (idx, field) in fields.iter().enumerate() {
            if idx > 0 {
                write!(f, ", ")?;
            }
            match field {
                MIRField::Standard { name, type_id } => {
                    write!(f, "{name}: ")?;
                    write_type_id(f, definitions, *type_id, state)?;
                }
                MIRField::Bitfield {
                    name,
                    integer_type_id,
                    width,
                } => {
                    if let Some(name) = name {
                        write!(f, "{name}: ")?;
                    }
                    write_type_id(f, definitions, *integer_type_id, state)?;
                    write!(f, " : {width}")?;
                }
            }
        }
        write!(f, " ")?;
    }
    write!(f, "}}")
}

fn write_type_body(
    f: &mut Formatter<'_>,
    definitions: &dyn MIRTypeContext,
    ty: &MIRType,
    id: Option<MIRTypeId>,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    if (id.is_some() || has_type_name(definitions, ty, id))
        && matches!(
            ty.kind,
            MIRTypeKind::Structured { .. }
                | MIRTypeKind::Union { .. }
                | MIRTypeKind::TaggedUnion { .. }
                | MIRTypeKind::Undefined
        )
    {
        write_type_qualifiers_prefix(f, ty.specifiers)?;
        return write_type_reference(f, definitions, ty, id, state);
    }

    if !matches!(ty.kind, MIRTypeKind::PointerTo { .. }) {
        write_type_qualifiers_prefix(f, ty.specifiers)?;
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
            write_type_id(f, definitions, *inner_type, state)?;
            write!(f, "*")?;
            write_type_qualifiers_suffix(f, ty.specifiers)
        }
        MIRTypeKind::MemoryReference {
            inner_type,
            bitfield,
        } => {
            if let Some(bitfield) = bitfield {
                write!(
                    f,
                    "&<bitfield @{}:{}>",
                    bitfield.bit_offset, bitfield.bit_width
                )?;
            } else {
                write!(f, "&")?;
            }

            write_type_id(f, definitions, *inner_type, state)
        }
        MIRTypeKind::Array {
            length: size,
            inner_type,
        } => {
            write!(f, "[")?;
            write_type_id(f, definitions, *inner_type, state)?;
            write!(f, "; {size}]")
        }
        MIRTypeKind::Opaque { size, alignment } => {
            write!(f, "opaque(size: {size}, align: {alignment})")
        }
        MIRTypeKind::Undefined => write_type_reference(f, definitions, ty, id, state),
        MIRTypeKind::Str => write!(f, "_str"),
        MIRTypeKind::Function { signature } => {
            write_signature_with_context(f, signature, definitions, state)
        }
    }
}

impl Display for MIRDisplay<'_, MIRType> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write_type_root(f, self.definitions, self.content)
    }
}

impl Display for MIRDisplay<'_, MIRExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        MIRExpressionFormatter::with_definitions(self.content, 0, self.definitions).fmt(f)
    }
}

impl Display for MIRDisplay<'_, MIRFunction> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{}\nBody:",
            self.content
                .prototype
                .display_with_definitions(self.definitions)
        )?;
        MIRExpressionFormatter::with_definitions(&self.content.body, 1, self.definitions).fmt(f)
    }
}

impl Display for MIRDisplay<'_, MIRFunctionSignature> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write_signature_with_context(
            f,
            self.content,
            self.definitions,
            &mut TypeDisplayState::default(),
        )
    }
}

impl Display for MIRDisplay<'_, MIRFunctionPrototype> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} :: {}",
            self.content.name(),
            self.content
                .signature()
                .display_with_definitions(self.definitions)
        )
    }
}

impl Display for MIRDisplay<'_, MIRParameter> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.content.name {
            write!(
                f,
                "{name}: {}",
                self.content
                    ._type
                    .display_with_definitions(self.definitions)
            )
        } else {
            write!(
                f,
                "{}",
                self.content
                    ._type
                    .display_with_definitions(self.definitions)
            )
        }
    }
}

impl Display for MIRDisplay<'_, MIRGlobalVariable> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "global {} ", self.content.linkage)?;
        write!(
            f,
            "{}",
            self.content.kind.display_with_definitions(self.definitions)
        )?;
        write!(
            f,
            " [{}]",
            if self.content.is_mutable {
                "mutable"
            } else {
                "immutable"
            }
        )?;
        Ok(())
    }
}

impl Display for MIRDisplay<'_, MIRGlobalVarKind> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.content {
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
                        _type.display_with_definitions(self.definitions),
                        name,
                        init
                    )
                } else {
                    write!(
                        f,
                        "{} {}",
                        _type.display_with_definitions(self.definitions),
                        name
                    )
                }
            }
        }
    }
}

impl Display for MIRDisplay<'_, MIRUnit> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "MIR Unit:")?;

        writeln!(f, "\nFunctions:")?;
        for function in &self.content.functions {
            writeln!(f, "{}", function.display_with_definitions(self.definitions))?;
        }

        writeln!(f, "\nGlobal Variables:")?;
        for global in &self.content.global_variables {
            writeln!(f, "{}", global.display_with_definitions(self.definitions))?;
        }

        writeln!(f, "\nEnd of MIR Unit")
    }
}

fn write_signature_with_context(
    f: &mut Formatter<'_>,
    signature: &MIRFunctionSignature,
    definitions: &dyn MIRTypeContext,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    write!(f, "fn(")?;
    for (i, param) in signature.params.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(
            f,
            "{}: ",
            param.name.as_ref().map(CXIdent::as_str).unwrap_or("_")
        )?;
        write_type_value(f, definitions, &param._type, state)?;
    }
    if signature.var_args {
        if !signature.params.is_empty() {
            write!(f, ", ")?;
        }
        write!(f, "...")?;
    }
    write!(f, ") -> ")?;
    write_type_value(f, definitions, &signature.return_type, state)?;

    Ok(())
}

struct MIRExpressionFormatter<'a> {
    expr: &'a MIRExpression,
    depth: usize,
    definitions: &'a dyn MIRTypeContext,
}

impl<'a> MIRExpressionFormatter<'a> {
    fn with_definitions(
        expr: &'a MIRExpression,
        depth: usize,
        definitions: &'a dyn MIRTypeContext,
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
        write!(f, "{}", ty.display_with_definitions(self.definitions))
    }

    fn write_bin_op(&self, f: &mut Formatter<'_>, op: &MIRBinOp) -> std::fmt::Result {
        match op {
            MIRBinOp::Float { ftype, op } => write!(f, "f{} {:?}", ftype.bytes() * 8, op),
            MIRBinOp::Integer { itype, op } => write!(f, "i{} {:?}", itype.bytes() * 8, op),
            MIRBinOp::PtrDiff { ptr_inner, op } => {
                write!(
                    f,
                    "ptrdiff<{}> {:?}",
                    ptr_inner.display_with_definitions(self.definitions),
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
            MIRExpressionKind::IntLiteral(value) => {
                write!(f, "IntLiteral {} <'", value)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            MIRExpressionKind::FloatLiteral(value) => {
                write!(f, "FloatLiteral f{} <'", value)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            MIRExpressionKind::Unit => {
                write!(f, "Unit <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            MIRExpressionKind::Variable { name, location: _ } => {
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
            MIRExpressionKind::RegionWrite { target, value } => {
                write!(f, "RegionWrite <'")?;
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
                _type,
                initial_value,
            } => {
                write!(f, "RegionCreate ")?;
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
            MIRExpressionKind::BindRegion {
                name,
                _type,
                initial_region,
                adopting,
            } => {
                write!(f, "BindRegion {} adopting={}: ", name, adopting)?;
                self.write_type(f, _type)?;
                write!(f, " <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: initial_region,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
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
            MIRExpressionKind::MemberAccess {
                base,
                member_index,
                aggregate_type,
                ..
            } => {
                write!(f, "MemberAccess [")?;
                self.write_type(f, aggregate_type)?;
                write!(f, "] member {member_index} <'")?;
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
            MIRExpressionKind::PatternIs { lhs, pattern } => {
                write!(f, "PatternIs {pattern} <'")?;
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
                    writeln!(f, "  Field {}:", initializer.field_index)?;
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
                subject_name,
                arms,
                default,
                exhaustive,
            } => {
                write!(f, "Match exhaustive={exhaustive} <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                if let Some(subject_name) = subject_name {
                    self.indent(f)?;
                    writeln!(f, "Subject {subject_name}:")?;
                }
                MIRExpressionFormatter {
                    expr: condition,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                for (pattern, arm_body) in arms {
                    self.indent(f)?;
                    writeln!(f, "Arm {pattern}:")?;
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
                if let Some(postcondition) = postcondition {
                    self.indent(f)?;
                    write!(f, " ++ Postcondition")?;

                    if let Some(name) = &postcondition.binding {
                        write!(f, "({})", name)?;
                    }

                    writeln!(f, ":")?;

                    MIRExpressionFormatter {
                        expr: &postcondition.condition,
                        depth: self.depth + 2,
                        definitions: self.definitions,
                    }
                    .fmt(f)?;
                }
                Ok(())
            }
            MIRExpressionKind::Yield {
                value,
                target_scope,
            } => {
                write!(f, "Yield -> scope {} <'", target_scope)?;
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
                Ok(())
            }
            MIRExpressionKind::Emit(expr) => {
                write!(f, "Emit <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
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

                if let Some(postcondition) = contract.postcondition.as_ref() {
                    self.indent(f)?;
                    write!(f, " ++ Postcondition")?;

                    if let Some(binding) = &postcondition.binding {
                        self.indent(f)?;
                        write!(f, "(binding: {binding})")?;
                    }

                    writeln!(f, ":")?;

                    MIRExpressionFormatter {
                        expr: &postcondition.condition,
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
