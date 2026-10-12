mod body;
mod expr;
mod native;
mod ty;

use std::fmt::{self, Display, Formatter};

use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    ty::HMIRTypeID, unit::{
        HMIRUnit,
        def::{HMIRDef, HMIRDefKind},
        function::{HMIRFunction, HMIRFunctionStage},
        global::{HMIRComptimeGlobal, HMIRGlobal},
    },
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
        HMIRDefKind::ComptimeGlobal(global) => write_comptime_global(f, unit, def, global),
        HMIRDefKind::Type(ty) => write_type_def(f, unit, def, *ty),
    }
}

fn write_linkage(f: &mut Formatter<'_>, linkage: LinkageMode) -> fmt::Result {
    match linkage {
        LinkageMode::Standard => Ok(()),
        other => write!(f, "{other} "),
    }
}

fn write_link_name(f: &mut Formatter<'_>, def: &HMIRDef, link_name: &CXIdent) -> fmt::Result {
    if *link_name == def.name().name {
        return Ok(());
    }
    write!(f, " link \"{link_name}\"")
}

fn write_function(
    f: &mut Formatter<'_>,
    unit: &HMIRUnit,
    def: &HMIRDef,
    function: &HMIRFunction,
) -> fmt::Result {
    let printer = BodyPrinter::new(unit, function.body());
    let signature = function.signature();

    write_linkage(f, function.prototype().linkage())?;
    let stage = match signature.stage() {
        HMIRFunctionStage::Runtime => "runtime",
        HMIRFunctionStage::Comptime => "comptime",
    };
    write!(f, "{stage} fn @{}(", def.name())?;
    for (index, param) in signature.params().iter().enumerate() {
        if index != 0 {
            f.write_str(", ")?;
        }
        match function.def().and_then(|def| def.params().get(index)) {
            Some(local) => printer.param_decl(f, *local)?,
            None => {
                if param.comptime() {
                    f.write_str("comptime ")?;
                }
                printer.expr(f, param.ty(), 0)?;
            }
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
    printer.expr(f, signature.return_type(), 0)?;
    write_link_name(f, def, function.prototype().symbol_name())?;

    let contract = signature.contract();
    if !contract.is_empty() {
        f.write_str(" where")?;
        if contract.is_safe() {
            f.write_str(" safe")?;
        }
        if let Some(precondition) = contract.precondition() {
            f.write_str(" pre(")?;
            printer.expr(f, precondition, 0)?;
            f.write_str(")")?;
        }
        if let Some((binding, postcondition)) = contract.postcondition() {
            f.write_str(" post(")?;
            if let Some(binding) = binding {
                printer.local(f, binding)?;
                f.write_str(" => ")?;
            }
            printer.expr(f, postcondition, 0)?;
            f.write_str(")")?;
        }
    }

    match function.def() {
        None => f.write_str(";"),
        Some(def) => {
            f.write_str(" ")?;
            printer.expr(f, def.root(), 0)
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
    write!(f, "global @{}: ", def.name())?;
    printer.expr(f, global.ty(), 0)?;
    write_link_name(f, def, global.link_name())?;
    if let Some(initializer) = global.initializer() {
        f.write_str(" = ")?;
        printer.expr(f, initializer, 0)?;
    }
    f.write_str(";")
}

fn write_comptime_global(
    f: &mut Formatter<'_>,
    unit: &HMIRUnit,
    def: &HMIRDef,
    global: &HMIRComptimeGlobal,
) -> fmt::Result {
    let printer = BodyPrinter::new(unit, global.body());
    write!(f, "comptime global @{}: ", def.name())?;
    printer.expr(f, global.ty(), 0)?;
    f.write_str(" = ")?;
    printer.expr(f, global.initializer(), 0)?;
    f.write_str(";")
}

fn write_type_def(
    f: &mut Formatter<'_>,
    unit: &HMIRUnit,
    def: &HMIRDef,
    ty: HMIRTypeID,
) -> fmt::Result {
    write!(f, "type @{} = ", def.name())?;
    ty::write_type(f, unit, ty)?;
    f.write_str(";")
}
