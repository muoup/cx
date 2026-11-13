mod declarator;
mod definitions;

use cx_mir::{MIRFnPrototype, MIRUnit};
use cx_pipeline_data::config::LinkEntry;
use cx_util::linkage::LinkageMode;

use crate::{
    declarator::{declare, parameters},
    definitions::Definitions,
};

pub fn generate_header(unit: &MIRUnit) -> String {
    let types = unit.types();
    let exported = unit
        .functions()
        .filter(|(_, function)| function.body().is_some())
        .map(|(_, function)| function.prototype())
        .filter(|prototype| prototype.linkage == LinkageMode::Standard)
        .collect::<Vec<_>>();

    let mut definitions = Definitions::new(types);
    for prototype in &exported {
        definitions.collect(prototype.signature.return_type());
        for param in prototype.signature.params() {
            definitions.collect(param.ty());
        }
    }

    let mut sections = vec![
        "#include <stdint.h>\n#include <stdbool.h>".to_string(),
    ];
    sections.push("#ifdef __cplusplus\nextern \"C\" {\n#endif".into());
    if !definitions.forward.is_empty() {
        sections.push(definitions.forward.iter().map(|decl| format!("{decl};\n")).collect::<String>().trim_end().into());
    }
    sections.extend(definitions.ordered);
    if !exported.is_empty() {
        let declarations = exported.iter().map(|prototype| declaration(unit, prototype)).collect::<Vec<_>>();
        sections.push(declarations.join("\n"));
    }
    sections.push("#ifdef __cplusplus\n}\n#endif".into());

    sections.join("\n\n") + "\n"
}

pub fn render_header(body: &str, link_entries: &[LinkEntry]) -> String {
    let mut sections = vec!["#pragma once".to_string()];
    if !link_entries.is_empty() {
        let entries = link_entries
            .iter()
            .map(|entry| format!(" *   -l{} ({:?})\n", entry.name, entry.kind))
            .collect::<String>();
        sections.push(format!("/* Link dependencies:\n{entries} */"));
    }
    sections.push(body.to_string());
    sections.join("\n\n")
}

fn declaration(unit: &MIRUnit, prototype: &MIRFnPrototype) -> String {
    let signature = &prototype.signature;
    let declarator = format!("{}({})", prototype.symbol_name, parameters(unit.types(), signature, true));
    format!("extern {};", declare(unit.types(), signature.return_type(), &declarator))
}
