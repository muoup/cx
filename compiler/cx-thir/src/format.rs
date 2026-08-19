use cx_hir::ast::modifiers::{HIR_CONST, HIR_RESTRICT, HIR_VOLATILE, HIRTypeQualifiers};
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use crate::thir::data::{THIRFnPrototype, THIRFnSignature, THIRParameter};
use crate::thir::expression::{
    THIRBinOp, THIRCoercion, THIRExpression, THIRExpressionKind, THIRUnOp,
};
use crate::thir::global::{THIRGlobalVarKind, THIRGlobalVariable};
use crate::thir::r#type::{
    THIRField, THIRFloatType, THIRIntType, THIRType, THIRTypeID, THIRTypeKind,
};
use crate::type_context::THIRTypeContext;
use crate::{THIRFunction, THIRUnit};
use std::fmt::{Display, Formatter};

#[derive(Default)]
struct TypeDisplayState {
    active_ids: Vec<THIRTypeID>,
}

impl TypeDisplayState {
    fn contains(&self, id: THIRTypeID) -> bool {
        self.active_ids.contains(&id)
    }

    fn enter(&mut self, id: THIRTypeID) {
        self.active_ids.push(id);
    }

    fn exit(&mut self, id: THIRTypeID) {
        let popped = self.active_ids.pop();
        debug_assert_eq!(popped, Some(id));
    }
}

pub struct THIRDisplay<'a, T: ?Sized> {
    content: &'a T,
    definitions: &'a dyn THIRTypeContext,
}

trait MIRDisplayable {
    fn display_with_definitions<'a>(
        &'a self,
        definitions: &'a dyn THIRTypeContext,
    ) -> THIRDisplay<'a, Self>
    where
        Self: Sized,
    {
        THIRDisplay {
            content: self,
            definitions,
        }
    }
}

impl MIRDisplayable for THIRType {}
impl MIRDisplayable for THIRExpression {}
impl MIRDisplayable for THIRFunction {}
impl MIRDisplayable for THIRFnSignature {}
impl MIRDisplayable for THIRFnPrototype {}
impl MIRDisplayable for THIRParameter {}
impl MIRDisplayable for THIRGlobalVariable {}
impl MIRDisplayable for THIRGlobalVarKind {}

impl THIRType {
    pub fn display_with<'a>(
        &'a self,
        definitions: &'a dyn THIRTypeContext,
    ) -> THIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl THIRExpression {
    pub fn display_with<'a>(
        &'a self,
        definitions: &'a dyn THIRTypeContext,
    ) -> THIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl THIRFunction {
    pub fn display_with<'a>(
        &'a self,
        definitions: &'a dyn THIRTypeContext,
    ) -> THIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl THIRFnSignature {
    pub fn display_with<'a>(
        &'a self,
        definitions: &'a dyn THIRTypeContext,
    ) -> THIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl THIRFnPrototype {
    pub fn display_with<'a>(
        &'a self,
        definitions: &'a dyn THIRTypeContext,
    ) -> THIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl THIRParameter {
    pub fn display_with<'a>(
        &'a self,
        definitions: &'a dyn THIRTypeContext,
    ) -> THIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl THIRGlobalVariable {
    pub fn display_with<'a>(
        &'a self,
        definitions: &'a dyn THIRTypeContext,
    ) -> THIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl THIRGlobalVarKind {
    pub fn display_with<'a>(
        &'a self,
        definitions: &'a dyn THIRTypeContext,
    ) -> THIRDisplay<'a, Self> {
        self.display_with_definitions(definitions)
    }
}

impl THIRUnit {
    pub fn display_pretty(&self) -> THIRDisplay<'_, Self> {
        THIRDisplay {
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
    definitions: &dyn THIRTypeContext,
    ty: &THIRType,
    id: Option<THIRTypeID>,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    if write_type_name(f, definitions, ty, id, state)? {
        Ok(())
    } else if let Some(id) = id {
        write!(f, "{id}")
    } else {
        match &ty.kind {
            THIRTypeKind::Structured { .. } => write!(f, "struct"),
            THIRTypeKind::Union { .. } => write!(f, "union"),
            THIRTypeKind::TaggedUnion { .. } => write!(f, "tagged_union"),
            THIRTypeKind::Undefined => write!(f, "undefined"),
            _ => unreachable!("type reference requested for non-referenceable type"),
        }
    }
}

fn write_recursive_reference(
    f: &mut Formatter<'_>,
    definitions: &dyn THIRTypeContext,
    ty: &THIRType,
    id: THIRTypeID,
) -> std::fmt::Result {
    if write_type_base_name(f, definitions, ty, Some(id))? {
        Ok(())
    } else {
        write!(f, "{id}<recursive>")
    }
}

fn write_type_name(
    f: &mut Formatter<'_>,
    definitions: &dyn THIRTypeContext,
    ty: &THIRType,
    id: Option<THIRTypeID>,
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
    definitions: &dyn THIRTypeContext,
    ty: &THIRType,
    id: Option<THIRTypeID>,
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

fn has_type_name(definitions: &dyn THIRTypeContext, ty: &THIRType, id: Option<THIRTypeID>) -> bool {
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
    definitions: &dyn THIRTypeContext,
    ty: &THIRType,
) -> std::fmt::Result {
    let mut state = TypeDisplayState::default();
    write_type_value(f, definitions, ty, &mut state)
}

fn write_type_value(
    f: &mut Formatter<'_>,
    definitions: &dyn THIRTypeContext,
    ty: &THIRType,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    write_type_body(f, definitions, ty, None, state)
}

fn write_type_id(
    f: &mut Formatter<'_>,
    definitions: &dyn THIRTypeContext,
    id: THIRTypeID,
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
    specifiers: HIRTypeQualifiers,
) -> std::fmt::Result {
    if specifiers & HIR_CONST != 0 {
        write!(f, "const ")?;
    }
    if specifiers & HIR_VOLATILE != 0 {
        write!(f, "volatile ")?;
    }
    if specifiers & HIR_RESTRICT != 0 {
        write!(f, "restrict ")?;
    }

    Ok(())
}

fn write_type_qualifiers_suffix(
    f: &mut Formatter<'_>,
    specifiers: HIRTypeQualifiers,
) -> std::fmt::Result {
    if specifiers & HIR_CONST != 0 {
        write!(f, " const")?;
    }
    if specifiers & HIR_VOLATILE != 0 {
        write!(f, " volatile")?;
    }
    if specifiers & HIR_RESTRICT != 0 {
        write!(f, " restrict")?;
    }

    Ok(())
}

fn write_aggregate(
    f: &mut Formatter<'_>,
    keyword: &str,
    ty: &THIRType,
    fields: &[THIRField],
    definitions: &dyn THIRTypeContext,
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
                THIRField::Standard { name, type_id } => {
                    write!(f, "{name}: ")?;
                    write_type_id(f, definitions, *type_id, state)?;
                }
                THIRField::Bitfield {
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
    definitions: &dyn THIRTypeContext,
    ty: &THIRType,
    id: Option<THIRTypeID>,
    state: &mut TypeDisplayState,
) -> std::fmt::Result {
    if (id.is_some() || has_type_name(definitions, ty, id))
        && matches!(
            ty.kind,
            THIRTypeKind::Structured { .. }
                | THIRTypeKind::Union { .. }
                | THIRTypeKind::TaggedUnion { .. }
                | THIRTypeKind::Undefined
        )
    {
        write_type_qualifiers_prefix(f, ty.specifiers)?;
        return write_type_reference(f, definitions, ty, id, state);
    }

    if !matches!(ty.kind, THIRTypeKind::PointerTo { .. }) {
        write_type_qualifiers_prefix(f, ty.specifiers)?;
    }

    match &ty.kind {
        THIRTypeKind::Integer { _type, signed } => {
            write!(f, "{}{}", if *signed { 'i' } else { 'u' }, _type)
        }
        THIRTypeKind::Float { _type } => write!(f, "{_type}"),
        THIRTypeKind::Structured { fields } => {
            write_aggregate(f, "struct", ty, fields, definitions, state)
        }
        THIRTypeKind::Union { variants } => {
            write_aggregate(f, "union", ty, variants, definitions, state)
        }
        THIRTypeKind::TaggedUnion { variants } => {
            write_aggregate(f, "tagged_union", ty, variants, definitions, state)
        }
        THIRTypeKind::Void => write!(f, "void"),
        THIRTypeKind::PointerTo { inner_type } => {
            write_type_id(f, definitions, *inner_type, state)?;
            write!(f, "*")?;
            write_type_qualifiers_suffix(f, ty.specifiers)
        }
        THIRTypeKind::MemoryReference {
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
        THIRTypeKind::Array {
            length: size,
            inner_type,
        } => {
            write!(f, "[")?;
            write_type_id(f, definitions, *inner_type, state)?;
            write!(f, "; {size}]")
        }
        THIRTypeKind::Opaque { size, alignment } => {
            write!(f, "opaque(size: {size}, align: {alignment})")
        }
        THIRTypeKind::Undefined => write_type_reference(f, definitions, ty, id, state),
        THIRTypeKind::Str => write!(f, "_str"),
        THIRTypeKind::Function { signature } => {
            write_signature_with_context(f, signature, definitions, state)
        }
    }
}

impl Display for THIRDisplay<'_, THIRType> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write_type_root(f, self.definitions, self.content)
    }
}

impl Display for THIRDisplay<'_, THIRExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        MIRExpressionFormatter::with_definitions(self.content, 0, self.definitions).fmt(f)
    }
}

impl Display for THIRDisplay<'_, THIRFunction> {
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

impl Display for THIRDisplay<'_, THIRFnSignature> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write_signature_with_context(
            f,
            self.content,
            self.definitions,
            &mut TypeDisplayState::default(),
        )
    }
}

impl Display for THIRDisplay<'_, THIRFnPrototype> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write_function_name(f, self.content)?;
        write!(
            f,
            " :: {}",
            self.content
                .signature()
                .display_with_definitions(self.definitions)
        )
    }
}

fn write_function_name(f: &mut Formatter<'_>, prototype: &THIRFnPrototype) -> std::fmt::Result {
    let display_name = prototype
        .debug_name()
        .map(CXIdent::as_str)
        .unwrap_or(prototype.symbol_name());
    write!(f, "{display_name}")?;

    if prototype
        .debug_name()
        .is_some_and(|debug_name| debug_name.as_str() != prototype.symbol_name())
    {
        write!(f, " {{{}}}", prototype.symbol_name())?;
    }

    Ok(())
}

impl Display for THIRDisplay<'_, THIRParameter> {
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

impl Display for THIRDisplay<'_, THIRGlobalVariable> {
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

impl Display for THIRDisplay<'_, THIRGlobalVarKind> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.content {
            THIRGlobalVarKind::StringLiteral { name, value } => {
                let escaped_value = value
                    .replace('\\', "\\\\")
                    .replace('\n', "\\n")
                    .replace('\t', "\\t")
                    .replace('\"', "\\\"");

                write!(f, "string {} = \"{}\"", name, escaped_value)
            }
            THIRGlobalVarKind::Variable {
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
                        init.display_with_definitions(self.definitions)
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

impl Display for THIRDisplay<'_, THIRUnit> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "THIR Unit:")?;

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
    signature: &THIRFnSignature,
    definitions: &dyn THIRTypeContext,
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
    expr: &'a THIRExpression,
    depth: usize,
    definitions: &'a dyn THIRTypeContext,
}

impl<'a> MIRExpressionFormatter<'a> {
    fn with_definitions(
        expr: &'a THIRExpression,
        depth: usize,
        definitions: &'a dyn THIRTypeContext,
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

    fn write_type(&self, f: &mut Formatter<'_>, ty: &THIRType) -> std::fmt::Result {
        write!(f, "{}", ty.display_with_definitions(self.definitions))
    }

    fn write_bin_op(&self, f: &mut Formatter<'_>, op: &THIRBinOp) -> std::fmt::Result {
        match op {
            THIRBinOp::Float { ftype, op } => write!(f, "f{} {:?}", ftype.bytes() * 8, op),
            THIRBinOp::Integer { itype, op } => write!(f, "i{} {:?}", itype.bytes() * 8, op),
            THIRBinOp::PtrDiff { ptr_inner, op } => {
                write!(
                    f,
                    "ptrdiff<{}> {:?}",
                    ptr_inner.display_with_definitions(self.definitions),
                    op
                )
            }
            THIRBinOp::Pointer { op } => write!(f, "ptr {:?}", op),
        }
    }
}

impl<'a> Display for MIRExpressionFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.indent(f)?;
        match &self.expr.kind {
            THIRExpressionKind::BoolLiteral(value) => {
                write!(f, "BoolLiteral {} <'", value)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            THIRExpressionKind::IntLiteral(value) => {
                write!(f, "IntLiteral {} <'", value)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            THIRExpressionKind::FloatLiteral(value) => {
                write!(f, "FloatLiteral f{} <'", value)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            THIRExpressionKind::Unit => {
                write!(f, "Unit <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            THIRExpressionKind::SizeOf { _type } => {
                write!(f, "SizeOf ")?;
                self.write_type(f, _type)?;
                writeln!(f, " <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            THIRExpressionKind::AlignOf { _type } => {
                write!(f, "AlignOf ")?;
                self.write_type(f, _type)?;
                writeln!(f, " <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            THIRExpressionKind::GlobalVariable { symbol } => {
                write!(f, "GlobalVariable \"{symbol}\" <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            },
            THIRExpressionKind::Variable { name, .. } => {
                write!(f, "LocalVariable {} <'", name)?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            THIRExpressionKind::ContractVariable { name, .. } => {
                write!(f, "ContractVariable \"{name}\" <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            THIRExpressionKind::FunctionReference { name, debug_name } => {
                write!(
                    f,
                    "FunctionReference {} <'",
                    debug_name.as_ref().unwrap_or(name)
                )?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            THIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
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
            THIRExpressionKind::UnaryOperation { operand, op } => {
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
            THIRExpressionKind::Typechange(expression) => {
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
            THIRExpressionKind::Assign { target, value } => {
                write!(f, "Assign <'")?;
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
            THIRExpressionKind::CreateLocalVariable {
                name,
                local_id,
                _type,
                initial_value,
                adopting
            } => {
                write!(
                    f,
                    "CreateStackVariable {} (local_id={:?}) <'",
                    name, local_id
                )?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ", adopting={adopting}>")?;
                if let Some(initial_value) = initial_value {
                    MIRExpressionFormatter {
                        expr: initial_value,
                        depth: self.depth + 1,
                        definitions: self.definitions,
                    }
                    .fmt(f)
                } else {
                    Ok(())
                }
            }
            THIRExpressionKind::Copy { source } => {
                write!(f, "Copy")?;
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
            
            THIRExpressionKind::Move { name, .. } => {
                write!(f, "Move <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, "> {}", name)
            }
            
            THIRExpressionKind::MemberAccess {
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
            THIRExpressionKind::ArrayAccess { array, index, .. } => {
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
            THIRExpressionKind::PatternIs { lhs, pattern } => {
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
            THIRExpressionKind::Unpack { name, bindings, .. } => {
                write!(f, "Unpack <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, "> {}", name)?;
                for binding in bindings {
                    self.indent(f)?;
                    writeln!(
                        f,
                        ".{} -> {}",
                        binding.field_name,
                        binding.binding_name,
                    )?;
                }
                Ok(())
            },
            
            THIRExpressionKind::TaggedUnionTag { value, .. } => {
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
            THIRExpressionKind::TaggedUnionGet {
                value,
                variant_type,
                ..
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
            THIRExpressionKind::TaggedUnionSet {
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
            THIRExpressionKind::TaggedUnionInitializer {
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
            THIRExpressionKind::StructInitializer {
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
            THIRExpressionKind::ArrayInitializer {
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
            THIRExpressionKind::If {
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
            THIRExpressionKind::While {
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
            THIRExpressionKind::For {
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
            THIRExpressionKind::CSwitch {
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
            THIRExpressionKind::Match {
                condition,
                subject,
                arms,
                default,
                exhaustive,
                ..
            } => {
                write!(f, "Match exhaustive={exhaustive} <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                self.indent(f)?;
                writeln!(f, "Subject #{}:", subject.0)?;
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
            THIRExpressionKind::Return {
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
            THIRExpressionKind::Yield { value } => {
                write!(f, "Yield <'")?;
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
            THIRExpressionKind::Emit(expr) => {
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
            THIRExpressionKind::Assert { condition, message } => {
                writeln!(f, "Assert {message:?}")?;
                MIRExpressionFormatter {
                    expr: condition,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            THIRExpressionKind::Defer { expression } => {
                write!(f, "Defer <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: expression,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            THIRExpressionKind::Block {
                statements,
                creates_scope,
            } => {
                write!(
                    f,
                    "{}Block {{ <'",
                    if *creates_scope { "Scoped " } else { "" }
                )?;
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
            THIRExpressionKind::CallFunction {
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
            THIRExpressionKind::VaStart { list, last } => {
                write!(f, "VaStart <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: list,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)?;
                MIRExpressionFormatter {
                    expr: last,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            THIRExpressionKind::VaEnd { list } => {
                write!(f, "VaEnd <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: list,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            THIRExpressionKind::VaArg { list, _type } => {
                write!(f, "VaArg <'")?;
                self.write_type(f, _type)?;
                writeln!(f, ">")?;
                MIRExpressionFormatter {
                    expr: list,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
            THIRExpressionKind::TypeConversion {
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
            THIRExpressionKind::LifetimeStart { variable, _type } => {
                write!(f, "LifetimeStart {} (", variable)?;
                self.write_type(f, _type)?;
                write!(f, ") <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            THIRExpressionKind::LifetimeEnd { variable, _type } => {
                write!(f, "LifetimeEnd {} (", variable)?;
                self.write_type(f, _type)?;
                write!(f, ") <'")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, ">")
            }
            THIRExpressionKind::LeakLifetime { expression } => {
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
            THIRExpressionKind::Unsafe { expression } => {
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
            THIRExpressionKind::Break => {
                write!(f, "Break <type='")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, "'>")
            }
            THIRExpressionKind::Continue => {
                write!(f, "Continue <type='")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, "'>")
            }
            THIRExpressionKind::Goto { name } => {
                write!(f, "Goto {name} <type='")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, "'>")
            }
            THIRExpressionKind::Label { name, statement } => {
                write!(f, "Label {name} <type='")?;
                self.write_type(f, &self.expr._type)?;
                writeln!(f, "'>")?;
                MIRExpressionFormatter {
                    expr: statement,
                    depth: self.depth + 1,
                    definitions: self.definitions,
                }
                .fmt(f)
            }
        }
    }
}

impl Display for THIRUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            THIRUnOp::NEG => write!(f, "neg"),
            THIRUnOp::INEG => write!(f, "ineg"),
            THIRUnOp::FNEG => write!(f, "fneg"),
            THIRUnOp::BNOT => write!(f, "bnot"),
            THIRUnOp::LNOT => write!(f, "lnot"),
            THIRUnOp::PreIncrement(amt) => write!(f, "pre_increment({})", amt),
            THIRUnOp::PostIncrement(amt) => write!(f, "post_increment({})", amt),
        }
    }
}

impl Display for THIRCoercion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            THIRCoercion::Integral {
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
            THIRCoercion::FloatCast { to_type } => write!(f, "fp_integral(to: {})", to_type),
            THIRCoercion::PtrToInt { to_type } => write!(f, "ptr_to_int(to: {})", to_type),
            THIRCoercion::IntToPtr { sextend } => {
                write!(f, "int_to_ptr({})", if *sextend { "sext" } else { "zext" })
            }
            THIRCoercion::IntToFloat { to_type, sextend } => write!(
                f,
                "int_to_float({}, to: {})",
                if *sextend { "sext" } else { "zext" },
                to_type
            ),
            THIRCoercion::FloatToInt { sextend, to_type } => write!(
                f,
                "float_to_int({}, to: {})",
                if *sextend { "sext" } else { "zext" },
                to_type
            ),
            THIRCoercion::GetFnPtr => write!(f, "get_fn_ptr"),

            THIRCoercion::ReinterpretBits => write!(f, "reinterpret_bits"),
            THIRCoercion::Typechange => write!(f, "typechange"),
        }
    }
}

impl Display for THIRTypeID {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "T{}", self.0)
    }
}

impl Display for THIRFloatType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            THIRFloatType::F32 => write!(f, "f32"),
            THIRFloatType::F64 => write!(f, "f64"),
        }
    }
}

impl Display for THIRIntType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            THIRIntType::I1 => write!(f, "1"),
            THIRIntType::I8 => write!(f, "8"),
            THIRIntType::I16 => write!(f, "16"),
            THIRIntType::I32 => write!(f, "32"),
            THIRIntType::I64 => write!(f, "64"),
            THIRIntType::I128 => write!(f, "128"),
        }
    }
}
