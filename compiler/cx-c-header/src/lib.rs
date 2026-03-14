use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{LMIRFunctionPrototype, LMIRUnit, LinkageType};
use cx_pipeline_data::config::LinkEntry;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;

/// Generate a C header file from LMIR units.
///
/// Exports all functions with non-Static linkage that have bodies (are defined, not just declared).
pub fn generate_header(lib_name: &str, lmir_units: &[LMIRUnit], link_entries: &[LinkEntry]) -> String {
    let mut output = String::new();
    let guard = lib_name.to_uppercase().replace(|c: char| !c.is_alphanumeric(), "_");

    writeln!(output, "#pragma once").unwrap();
    writeln!(output, "#ifndef {guard}_H").unwrap();
    writeln!(output, "#define {guard}_H").unwrap();
    writeln!(output).unwrap();

    // Link dependency comments
    if !link_entries.is_empty() {
        writeln!(output, "/* Link dependencies:").unwrap();
        for entry in link_entries {
            writeln!(output, " *   -l{} ({})", entry.name, entry.kind).unwrap();
        }
        writeln!(output, " */").unwrap();
        writeln!(output).unwrap();
    }

    writeln!(output, "#include <stdint.h>").unwrap();
    writeln!(output, "#include <stdbool.h>").unwrap();
    writeln!(output).unwrap();
    writeln!(output, "#ifdef __cplusplus").unwrap();
    writeln!(output, "extern \"C\" {{").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // Collect all exported function prototypes
    let mut exported_protos: Vec<&LMIRFunctionPrototype> = Vec::new();
    for unit in lmir_units {
        for func in &unit.fn_defs {
            if func.prototype.linkage != LinkageType::Static {
                exported_protos.push(&func.prototype);
            }
        }
    }

    // Collect all types referenced by exported functions
    let mut type_defs: BTreeMap<String, String> = BTreeMap::new();
    let mut forward_decls: BTreeSet<String> = BTreeSet::new();

    for proto in &exported_protos {
        collect_types(&proto.return_type, &mut type_defs, &mut forward_decls);
        if let Some(ref tb) = proto.temp_buffer {
            collect_types(tb, &mut type_defs, &mut forward_decls);
        }
        for param in &proto.params {
            collect_types(&param._type, &mut type_defs, &mut forward_decls);
        }
    }

    // Emit forward declarations
    for decl in &forward_decls {
        writeln!(output, "{decl};").unwrap();
    }
    if !forward_decls.is_empty() {
        writeln!(output).unwrap();
    }

    // Emit type definitions
    for (_, def) in &type_defs {
        writeln!(output, "{def}").unwrap();
        writeln!(output).unwrap();
    }

    // Emit function declarations
    for proto in &exported_protos {
        let decl = format_function_declaration(proto);
        writeln!(output, "{decl};").unwrap();
    }

    writeln!(output).unwrap();
    writeln!(output, "#ifdef __cplusplus").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();
    writeln!(output, "#endif /* {guard}_H */").unwrap();

    output
}

fn collect_types(
    ty: &LMIRType,
    type_defs: &mut BTreeMap<String, String>,
    forward_decls: &mut BTreeSet<String>,
) {
    match &ty.kind {
        LMIRTypeKind::Struct { name, fields } => {
            if type_defs.contains_key(name) {
                return;
            }
            forward_decls.insert(format!("struct {name}"));
            // Recurse into fields first
            for (_, field_ty) in fields {
                collect_types(field_ty, type_defs, forward_decls);
            }
            let mut def = format!("struct {name} {{\n");
            for (field_name, field_ty) in fields {
                let c_type = lmir_type_to_c(field_ty, Some(field_name));
                writeln!(def, "    {c_type};").unwrap();
            }
            write!(def, "}}").unwrap();
            type_defs.insert(name.clone(), def);
        }
        LMIRTypeKind::Union { name, fields } => {
            if type_defs.contains_key(name) {
                return;
            }
            forward_decls.insert(format!("union {name}"));
            for (_, field_ty) in fields {
                collect_types(field_ty, type_defs, forward_decls);
            }
            let mut def = format!("union {name} {{\n");
            for (field_name, field_ty) in fields {
                let c_type = lmir_type_to_c(field_ty, Some(field_name));
                writeln!(def, "    {c_type};").unwrap();
            }
            write!(def, "}}").unwrap();
            type_defs.insert(name.clone(), def);
        }
        LMIRTypeKind::Array { element, .. } => {
            collect_types(element, type_defs, forward_decls);
        }
        LMIRTypeKind::Pointer { .. } => {}
        _ => {}
    }
}

/// Format an LMIR type as a C type string.
/// If `var_name` is Some, includes the variable name (needed for arrays).
fn lmir_type_to_c(ty: &LMIRType, var_name: Option<&str>) -> String {
    let name = var_name.unwrap_or("");
    match &ty.kind {
        LMIRTypeKind::Integer(int_ty) => {
            let base = match int_ty {
                LMIRIntegerType::I1 => "bool",
                LMIRIntegerType::I8 => "int8_t",
                LMIRIntegerType::I16 => "int16_t",
                LMIRIntegerType::I32 => "int32_t",
                LMIRIntegerType::I64 => "int64_t",
                LMIRIntegerType::I128 => "__int128",
            };
            if name.is_empty() { base.to_string() } else { format!("{base} {name}") }
        }
        LMIRTypeKind::Float(float_ty) => {
            let base = match float_ty {
                LMIRFloatType::F32 => "float",
                LMIRFloatType::F64 => "double",
            };
            if name.is_empty() { base.to_string() } else { format!("{base} {name}") }
        }
        LMIRTypeKind::Pointer { .. } => {
            if name.is_empty() { "void*".to_string() } else { format!("void* {name}") }
        }
        LMIRTypeKind::Unit => {
            if name.is_empty() { "void".to_string() } else { format!("void {name}") }
        }
        LMIRTypeKind::Struct { name: struct_name, .. } => {
            if name.is_empty() {
                format!("struct {struct_name}")
            } else {
                format!("struct {struct_name} {name}")
            }
        }
        LMIRTypeKind::Union { name: union_name, .. } => {
            if name.is_empty() {
                format!("union {union_name}")
            } else {
                format!("union {union_name} {name}")
            }
        }
        LMIRTypeKind::Array { element, size } => {
            let elem_c = lmir_type_to_c(element, None);
            if name.is_empty() {
                format!("{elem_c}[{size}]")
            } else {
                format!("{elem_c} {name}[{size}]")
            }
        }
        LMIRTypeKind::Opaque { bytes } => {
            // Represent opaque types as byte arrays
            if name.is_empty() {
                format!("uint8_t[{bytes}]")
            } else {
                format!("uint8_t {name}[{bytes}]")
            }
        }
    }
}

fn format_function_declaration(proto: &LMIRFunctionPrototype) -> String {
    // If function has a temp_buffer, it returns a struct via implicit first pointer param.
    // For the C header, we emit it as returning by value.
    let return_type = if let Some(ref tb) = proto.temp_buffer {
        lmir_type_to_c(tb, None)
    } else {
        lmir_type_to_c(&proto.return_type, None)
    };

    let params: Vec<String> = proto.params.iter().enumerate()
        .filter(|(i, _)| {
            // Skip the implicit temp_buffer parameter (first param if temp_buffer is set)
            !(proto.temp_buffer.is_some() && *i == 0)
        })
        .map(|(i, param)| {
            let name = param.name.as_deref()
                .unwrap_or(&format!("arg{i}"))
                .to_string();
            lmir_type_to_c(&param._type, Some(&name))
        })
        .collect();

    let params_str = if params.is_empty() {
        "void".to_string()
    } else {
        params.join(", ")
    };

    format!("extern {return_type} {}({params_str})", proto.name)
}
