use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{LMIRFunctionPrototype, LMIRUnit, LinkageType};
use cx_pipeline_data::config::LinkEntry;
use cx_util::identifier::CXIdent;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;

pub fn generate_header(
    _lib_name: &str,
    lmir_unit: &LMIRUnit,
    link_entries: &[LinkEntry],
) -> Result<String, std::fmt::Error> {
    let mut output = String::new();

    writeln!(output, "#pragma once")?;
    writeln!(output)?;

    if !link_entries.is_empty() {
        writeln!(output, "/* Link dependencies:").unwrap();
        for entry in link_entries {
            writeln!(output, " *   -l{} ({})", entry.name, entry.kind)?;
        }
        writeln!(output, " */")?;
        writeln!(output)?;
    }

    writeln!(output, "#include <stdint.h>")?;
    writeln!(output, "#include <stdbool.h>")?;
    writeln!(output)?;
    writeln!(output, "#ifdef __cplusplus")?;
    writeln!(output, "extern \"C\" {{")?;
    writeln!(output, "#endif")?;
    writeln!(output)?;

    let mut exported_protos: Vec<&LMIRFunctionPrototype> = Vec::new();
    for func in &lmir_unit.fn_defs {
        if func.prototype.linkage != LinkageType::Static
            && func.prototype.linkage != LinkageType::External
        {
            exported_protos.push(&func.prototype);
        }
    }

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

    for decl in &forward_decls {
        writeln!(output, "{decl};")?;
    }
    if !forward_decls.is_empty() {
        writeln!(output)?;
    }

    for def in type_defs.values() {
        writeln!(output, "{def}")?;
        writeln!(output)?;
    }

    for proto in &exported_protos {
        let decl = format_function_declaration(proto);
        writeln!(output, "{decl};")?;
    }

    writeln!(output)?;
    writeln!(output, "#ifdef __cplusplus")?;
    writeln!(output, "}}")?;
    writeln!(output, "#endif")?;
    writeln!(output)?;

    Ok(output)
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
            if name.is_empty() {
                base.to_string()
            } else {
                format!("{base} {name}")
            }
        }
        LMIRTypeKind::Float(float_ty) => {
            let base = match float_ty {
                LMIRFloatType::F32 => "float",
                LMIRFloatType::F64 => "double",
            };
            if name.is_empty() {
                base.to_string()
            } else {
                format!("{base} {name}")
            }
        }
        LMIRTypeKind::Pointer { .. } => {
            if name.is_empty() {
                "void*".to_string()
            } else {
                format!("void* {name}")
            }
        }
        LMIRTypeKind::Unit => {
            if name.is_empty() {
                "void".to_string()
            } else {
                format!("void {name}")
            }
        }
        LMIRTypeKind::Struct {
            name: struct_name, ..
        } => {
            if name.is_empty() {
                format!("struct {struct_name}")
            } else {
                format!("struct {struct_name} {name}")
            }
        }
        LMIRTypeKind::Union {
            name: union_name, ..
        } => {
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
        LMIRTypeKind::Vector { .. } | LMIRTypeKind::ABIAggregate { .. } => {
            panic!("ABI-only type cannot be emitted in a C header: {ty:?}")
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
    let return_type = if let Some(ref tb) = proto.temp_buffer {
        lmir_type_to_c(tb, None)
    } else {
        lmir_type_to_c(&proto.return_type, None)
    };

    let params: Vec<String> = proto
        .params
        .iter()
        .enumerate()
        .filter(|(i, _)| !(proto.temp_buffer.is_some() && *i == 0))
        .map(|(i, param)| {
            let name = param
                .name
                .as_ref()
                .map(CXIdent::as_string)
                .unwrap_or(format!("arg{i}"));
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
