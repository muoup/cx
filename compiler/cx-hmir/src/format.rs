mod body;
mod emit;
mod expr;
mod flow;
mod inline;
mod native;
mod ty;

use std::fmt::{self, Display, Formatter};

use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::unit::{
    HMIRUnit,
    def::{HMIRDef, HMIRDefKind},
    function::{HMIRFunction, HMIRFunctionStage},
    global::{HMIRComptimeGlobal, HMIRGlobal},
};

use body::BodyPrinter;
use emit::Emitter;

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
    for (index, declaration) in def.declarations().iter().enumerate() {
        if index != 0 {
            writeln!(f)?;
        }
        match declaration.kind() {
            Some(HMIRDefKind::Function(function)) => write_function(f, unit, def, function)?,
            Some(HMIRDefKind::Global(global)) => write_global(f, unit, def, global)?,
            Some(HMIRDefKind::ComptimeGlobal(global)) => {
                write_comptime_global(f, unit, def, global)?
            }
            None => write!(f, "declare @{};", def.name())?,
        }
    }
    Ok(())
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
        match function.params().get(index) {
            Some(local) => f.write_str(&printer.param_decl(*local))?,
            None => {
                if param.comptime() {
                    f.write_str("comptime ")?;
                }
                f.write_str(&printer.inline(*param.ty()))?;
            }
        }
        if param.defaulted() {
            f.write_str(" = _")?;
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
    f.write_str(&printer.inline(*signature.return_type()))?;
    write_link_name(f, def, function.prototype().symbol_name())?;

    let contract = signature.contract();
    if !contract.is_empty() {
        let mut conditions = Emitter::new(BodyPrinter::new(unit, contract.body()));

        f.write_str(" where")?;
        if contract.is_safe() {
            f.write_str(" safe")?;
        }
        if let Some(precondition) = contract.precondition() {
            write!(f, " pre({})", conditions.value(precondition))?;
        }
        if let Some((binding, postcondition)) = contract.postcondition() {
            f.write_str(" post(")?;
            if let Some(binding) = binding {
                write!(f, "{} => ", conditions.printer.local(binding))?;
            }
            write!(f, "{})", conditions.value(postcondition))?;
        }
    }

    match function.def() {
        None => f.write_str(";"),
        Some(def) => write!(f, " {}", Emitter::new(printer).body(def.root())),
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
    f.write_str(&printer.inline(global.ty()))?;
    write_link_name(f, def, global.symbol_name())?;
    if let Some(initializer) = global.initializer() {
        write!(f, " = {}", Emitter::new(printer).value(initializer))?;
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
    f.write_str(&printer.inline(global.ty()))?;
    write!(
        f,
        " = {};",
        Emitter::new(printer).value(global.initializer())
    )
}
