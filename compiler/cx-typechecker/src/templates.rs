use crate::environment::{TCEnvironment, TCTemplateRequest};
use crate::type_mapping::{contextualize_fn_prototype, contextualize_type};
use cx_parsing_data::preparse::templates::CXTemplatePrototype;
use cx_typechecker_data::cx_types::{CXFunctionPrototype, CXTemplateInput, CXType, CXTypeKind};
use cx_util::identifier::CXIdent;
use cx_util::mangling::mangle_destructor;

pub(crate) type Overwrites = Vec<(String, CXType)>;

pub(crate) fn add_templated_types(
    env: &mut TCEnvironment,
    prototype: &CXTemplatePrototype,
    input: &CXTemplateInput,
) -> Overwrites {
    let mut overwrites = Vec::new();

    for (ident, arg_type) in prototype.types.iter().zip(input.args.iter()) {
        if let Some(existing) = env.realized_types.insert(ident.clone(), arg_type.clone()) {
            overwrites.push((ident.clone(), existing));
        }
    }

    overwrites
}

pub(crate) fn restore_template_overwrites(env: &mut TCEnvironment, overwrites: Overwrites) {
    for (ident, arg_type) in overwrites.into_iter() {
        env.realized_types.insert(ident, arg_type);
    }
}

pub fn mangle_template_name(name: &str, input: &CXTemplateInput) -> String {
    let mut mangled_name = String::from("_t");
    
    for arg in &input.args {
        mangled_name.push_str(&type_mangle(arg));
    }
    
    mangled_name.push('_');
    mangled_name.push_str(name);

    mangled_name
}

pub(crate) fn instantiate_type_template(
    env: &mut TCEnvironment,
    name: &str,
    input: &CXTemplateInput,
) -> Option<CXType> {
    let mangled_name = mangle_template_name(name, input);

    if let Some(type_) = env.get_type(&mangled_name) {
        return Some(type_.clone());
    }

    let template = env
        .base_data
        .type_data
        .get_template(name)?
        .template
        .resource
        .clone();
    let shell = template.shell.clone();

    let overwrites = add_templated_types(env, &template.prototype, input);

    let mut instantiated = contextualize_type(env, &shell)?;
    instantiated.map_name(|name| mangle_template_name(name, &input));

    env.realized_types
        .insert(mangled_name.clone(), instantiated.clone());

    let destructor_name = mangle_destructor(name);

    if env
        .base_data
        .fn_data
        .get_template(&destructor_name)
        .is_some()
    {
        instantiate_function_template(env, &destructor_name, input);
    }

    restore_template_overwrites(env, overwrites);

    Some(instantiated)
}

pub(crate) fn instantiate_function_template(
    env: &mut TCEnvironment,
    name: &str,
    input: &CXTemplateInput,
) -> Option<CXFunctionPrototype> {
    let mangled_name = mangle_template_name(name, input);
    
    if env.base_data.fn_data.standard.contains_key(&mangled_name) {
        return env.get_func(&mangled_name);
    }

    let cache = env.base_data.fn_data.get_template(name)?;
    let module_origin = cache.template.external_module.clone();
    let template = cache.template.resource.clone();

    let overwrites = add_templated_types(env, &template.prototype, input);

    let mut instantiated = contextualize_fn_prototype(env, &template.shell.clone())?;
    instantiated.name = CXIdent::from(mangled_name.as_str());

    env.realized_fns.insert(mangled_name.clone(), instantiated);
    env.requests.push(TCTemplateRequest {
        module_origin: module_origin.clone(),
        name: name.to_string(),
        input: input.clone(),
    });

    restore_template_overwrites(env, overwrites);

    env.get_func(&mangled_name)
}

pub fn type_mangle(ty: &CXType) -> String {
    let mut mangled = String::new();

    match &ty.kind {
        CXTypeKind::PointerTo { inner_type, .. } => {
            mangled.push('P');
            mangled.push_str(&type_mangle(inner_type));
        }
        CXTypeKind::StrongPointer { inner_type, is_array } => {
            mangled.push('S');
            if *is_array {
                mangled.push('A');
            } else {
                mangled.push('P');
            }
            mangled.push_str(&type_mangle(inner_type));
        },
        CXTypeKind::MemoryReference(inner_type) => {
            mangled.push('R');
            mangled.push_str(&type_mangle(inner_type));
        }
        CXTypeKind::Opaque { size, .. } => {
            mangled.push('O');
            mangled.push_str(&size.to_string());
        },
        CXTypeKind::Array { size, inner_type } => {
            mangled.push('A');
            mangled.push_str(&size.to_string());
            mangled.push('_');
            mangled.push_str(&type_mangle(inner_type));
        },
        CXTypeKind::VariableLengthArray { _type, .. } => {
            mangled.push_str("V_a");
            mangled.push_str(&type_mangle(_type));
        },
        CXTypeKind::Function { prototype } => {
            mangled.push('F');
            mangled.push_str(&type_mangle(&prototype.return_type));
            for param in &prototype.params {
                mangled.push_str(&type_mangle(&param._type));
            }
            mangled.push(prototype.var_args as u8 as char);
        }
        CXTypeKind::Structured { fields, .. } => {
            mangled.push('S');
            mangled.push_str(&fields.len().to_string());
            mangled.push('_');
            for field in fields {
                mangled.push_str(&type_mangle(&field.1));
            }
        }
        CXTypeKind::Union { variants, .. } => {
            mangled.push('U');
            mangled.push_str(&variants.len().to_string());
            mangled.push('_');
            for variant in variants {
                mangled.push_str(&type_mangle(&variant.1));
            }
        }
        CXTypeKind::TaggedUnion { variants, .. } => {
            mangled.push('T');
            mangled.push_str(&variants.len().to_string());
            mangled.push('_');
            for variant in variants {
                mangled.push_str(&type_mangle(&variant.1));
            }
        }
        CXTypeKind::Integer { bytes, signed } => {
            mangled.push('I');
            mangled.push_str(&bytes.to_string());
            mangled.push(if *signed { 's' } else { 'u' });
        }
        CXTypeKind::Float { bytes } => {
            mangled.push('F');
            mangled.push_str(&bytes.to_string());
        }
        CXTypeKind::Bool => {
            mangled.push('B');
        }
        CXTypeKind::Unit => {
            mangled.push('v');
        }
    }
    
    return mangled;
}