mod body;
mod intrinsic;
mod meta;
mod obj;
mod ty;

use std::fmt::{self, Display, Formatter};

use cx_util::linkage::LinkageMode;

use crate::{
    def::{HMIRDef, HMIRDefKind, HMIRGlobal},
    function::{HMIRFunction, HMIRFunctionRoot, HMIRParam},
    ty::nominal::{HMIRAggregateKind, HMIRMoveSemantics},
    type_def::{HMIRTypeDef, HMIRTypeDefKind},
    unit::HMIRUnit,
};

use body::BodyPrinter;

impl Display for HMIRUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "; HMIR module {}", self.namespace())?;
        for (_, def) in self.defs() {
            writeln!(f)?;
            write_def(f, self, def)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

fn write_def(f: &mut Formatter<'_>, unit: &HMIRUnit, def: &HMIRDef) -> fmt::Result {
    match def.kind() {
        HMIRDefKind::Function(function) => write_function(f, unit, def, function),
        HMIRDefKind::Global(global) => write_global(f, unit, def, global),
        HMIRDefKind::Type(type_def) => write_type_def(f, unit, def, type_def),
    }
}

fn write_linkage(f: &mut Formatter<'_>, linkage: LinkageMode) -> fmt::Result {
    match linkage {
        LinkageMode::Standard => Ok(()),
        other => write!(f, "{other} "),
    }
}

fn write_function(
    f: &mut Formatter<'_>,
    unit: &HMIRUnit,
    def: &HMIRDef,
    function: &HMIRFunction,
) -> fmt::Result {
    let printer = BodyPrinter::new(unit, function.body());
    let signature = function.signature();

    write_linkage(f, signature.linkage())?;
    write!(f, "fn @{}(", def.name())?;
    for (index, param) in signature.params().iter().enumerate() {
        if index != 0 {
            f.write_str(", ")?;
        }
        match param {
            HMIRParam::Static(local) => {
                f.write_str("static ")?;
                printer.meta_local_decl(f, *local)?;
            }
            HMIRParam::Runtime(local) => printer.obj_local_decl(f, *local)?,
        }
    }
    if signature.is_variadic() {
        f.write_str(if signature.params().is_empty() {
            "..."
        } else {
            ", ..."
        })?;
    }
    f.write_str(") -> ")?;
    printer.meta(f, signature.return_type(), 0)?;

    let contract = signature.contract();
    if !contract.is_empty() {
        f.write_str(" where")?;
        if contract.is_safe() {
            f.write_str(" safe")?;
        }
        if let Some(precondition) = contract.precondition() {
            f.write_str(" pre(")?;
            printer.obj(f, precondition, 0)?;
            f.write_str(")")?;
        }
        if let Some((binding, postcondition)) = contract.postcondition() {
            f.write_str(" post(")?;
            if let Some(binding) = binding {
                printer.obj_local(f, binding)?;
                f.write_str(" => ")?;
            }
            printer.obj(f, postcondition, 0)?;
            f.write_str(")")?;
        }
    }

    match function.root() {
        None => f.write_str(";"),
        Some(HMIRFunctionRoot::Meta(root)) => {
            f.write_str(" ")?;
            printer.meta(f, root, 0)
        }
        Some(HMIRFunctionRoot::Obj(root)) => {
            f.write_str(" ")?;
            printer.obj(f, root, 0)
        }
    }
}

fn write_global(
    f: &mut Formatter<'_>,
    unit: &HMIRUnit,
    def: &HMIRDef,
    global: &HMIRGlobal,
) -> fmt::Result {
    let printer = BodyPrinter::new(unit, global.body());

    write_linkage(f, global.linkage())?;
    f.write_str(if global.is_mutable() {
        "global mut"
    } else {
        "global"
    })?;
    write!(f, " @{}: ", def.name())?;
    printer.meta(f, global.ty(), 0)?;
    if let Some(initializer) = global.initializer() {
        f.write_str(" = ")?;
        printer.obj(f, initializer, 0)?;
    }
    f.write_str(";")
}

fn write_type_def(
    f: &mut Formatter<'_>,
    unit: &HMIRUnit,
    def: &HMIRDef,
    type_def: &HMIRTypeDef,
) -> fmt::Result {
    let printer = BodyPrinter::new(unit, type_def.body());

    write!(f, "type @{}", def.name())?;
    if !type_def.params().is_empty() {
        f.write_str("(")?;
        for (index, param) in type_def.params().iter().enumerate() {
            if index != 0 {
                f.write_str(", ")?;
            }
            printer.meta_local_decl(f, *param)?;
        }
        f.write_str(")")?;
    }
    f.write_str(" = ")?;

    match type_def.kind() {
        HMIRTypeDefKind::Alias(ty) => printer.meta(f, *ty, 0),
        HMIRTypeDefKind::Aggregate {
            kind,
            semantics,
            fields,
        } => {
            f.write_str(match kind {
                HMIRAggregateKind::Struct => "struct",
                HMIRAggregateKind::Union => "union",
                HMIRAggregateKind::TaggedUnion => "tagged_union",
            })?;
            match semantics {
                HMIRMoveSemantics::POD => {}
                HMIRMoveSemantics::Nocopy => f.write_str(" nocopy")?,
                HMIRMoveSemantics::Nodrop => f.write_str(" nodrop")?,
            }
            f.write_str(" {\n")?;
            for field in fields {
                body::indent(f, 1)?;
                match field.name() {
                    Some(name) => write!(f, "{name}: ")?,
                    None => f.write_str("_: ")?,
                }
                printer.meta(f, field.ty(), 1)?;
                if let Some(width) = field.bit_width() {
                    write!(f, " : {width}")?;
                }
                f.write_str(",\n")?;
            }
            f.write_str("}")
        }
    }
}
