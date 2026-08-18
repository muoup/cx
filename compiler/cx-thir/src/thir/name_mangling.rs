use crate::thir::data::{THIRType, THIRTypeKind};
use crate::thir::r#type::THIRField;
use crate::type_context::THIRTypeContext;
use cx_hir::registry::{ExportNameMode, GlobalSymbolRegistry};
use cx_util::namespace::QualifiedName;

pub fn mangle_qualified_name(
    global_registry: &GlobalSymbolRegistry,
    name: &QualifiedName,
) -> String {
    if name.namespace.is_root()
        || global_registry.export_name_mode(&name.namespace) == ExportNameMode::Root
    {
        return name.name.as_str().to_string();
    }

    mangle_namespace_symbol(name)
}

pub fn mangle_namespace_symbol(name: &QualifiedName) -> String {
    cx_util::namespace::mangle_namespace_symbol(name)
}

pub fn base_mangle_templated_name<'a>(
    definitions: &impl THIRTypeContext,
    name: &str,
    template_args: impl ExactSizeIterator<Item = &'a THIRType>,
) -> String {
    let mut mangled = String::from("_T");
    push_component(&mut mangled, template_args.len().to_string().as_str());
    push_component(&mut mangled, name);
    for arg in template_args {
        let argument = type_mangle(definitions, arg);
        push_component(&mut mangled, argument.as_str());
    }
    mangled
}

pub fn base_mangle_member(
    definitions: &impl THIRTypeContext,
    name: &str,
    member_type: &THIRType,
) -> String {
    let mut mangled = String::from("_M");
    let member_type = type_mangle(definitions, member_type);
    push_component(&mut mangled, member_type.as_str());
    push_component(&mut mangled, name);
    mangled
}

pub(crate) fn type_mangle(definitions: &impl THIRTypeContext, ty: &THIRType) -> String {
    if let Some(name) = ty.strong_identifier() {
        let mut mangled = String::from("n");
        push_component(&mut mangled, name);
        return mangled;
    }

    match &ty.kind {
        THIRTypeKind::Integer { _type, signed } => {
            format!("i{}{}", if *signed { 's' } else { 'u' }, _type)
        }
        THIRTypeKind::Float { _type } => {
            format!("f{}", _type)
        }
        THIRTypeKind::Str => "s".to_owned(),
        THIRTypeKind::Undefined => "u".to_owned(),
        THIRTypeKind::Void => "v".to_owned(),
        THIRTypeKind::PointerTo { inner_type } => {
            let inner_type = definitions.resolve_type_id(*inner_type);
            let mut mangled = String::from("p");
            let inner_type = type_mangle(definitions, inner_type);
            push_component(&mut mangled, inner_type.as_str());
            mangled
        }
        THIRTypeKind::MemoryReference {
            inner_type,
            bitfield,
        } => {
            let mut mangled = String::from("r");
            if let Some(bitfield) = bitfield {
                mangled.push('1');
                push_component(&mut mangled, bitfield.bit_offset.to_string().as_str());
                push_component(&mut mangled, bitfield.bit_width.to_string().as_str());
                let storage_type = type_mangle(
                    definitions,
                    definitions.resolve_type_id(bitfield.storage_type),
                );
                push_component(&mut mangled, storage_type.as_str());
                push_component(&mut mangled, if bitfield.signed { "1" } else { "0" });
            } else {
                mangled.push('0');
            }
            let inner_type = type_mangle(definitions, definitions.resolve_type_id(*inner_type));
            push_component(&mut mangled, inner_type.as_str());
            mangled
        }
        THIRTypeKind::Opaque { size, alignment } => {
            let mut mangled = String::from("o");
            push_component(&mut mangled, size.to_string().as_str());
            push_component(&mut mangled, alignment.to_string().as_str());
            mangled
        }
        THIRTypeKind::Array {
            length: size,
            inner_type,
        } => {
            let mut mangled = String::from("a");
            push_component(&mut mangled, size.to_string().as_str());
            let inner_type = type_mangle(definitions, definitions.resolve_type_id(*inner_type));
            push_component(&mut mangled, inner_type.as_str());
            mangled
        }
        THIRTypeKind::Function { signature } => {
            let mut mangled = String::from("f");
            let return_type = type_mangle(definitions, &signature.return_type);
            push_component(&mut mangled, return_type.as_str());
            push_component(&mut mangled, signature.params.len().to_string().as_str());
            for param in &signature.params {
                let param_type = type_mangle(definitions, &param._type);
                push_component(&mut mangled, param_type.as_str());
            }
            push_component(&mut mangled, if signature.var_args { "1" } else { "0" });
            mangled
        }
        THIRTypeKind::Structured { fields } => {
            let mut mangled = String::from("s");
            push_move_attributes(&mut mangled, ty);
            push_aggregate_fields(&mut mangled, definitions, fields);
            mangled
        }
        THIRTypeKind::Union { variants } => {
            let mut mangled = String::from("u");
            push_move_attributes(&mut mangled, ty);
            push_aggregate_fields(&mut mangled, definitions, variants);
            mangled
        }
        THIRTypeKind::TaggedUnion { variants } => {
            let mut mangled = String::from("t");
            push_move_attributes(&mut mangled, ty);
            push_aggregate_fields(&mut mangled, definitions, variants);
            mangled
        }
    }
}

fn push_move_attributes(mangled: &mut String, ty: &THIRType) {
    mangled.push(if ty.attributes.semantics.is_nocopy() {
        'C'
    } else {
        'c'
    });
    mangled.push(if ty.attributes.semantics.is_nodrop() {
        'D'
    } else {
        'd'
    });
    mangled.push(if ty.attributes.unsafe_move { 'M' } else { 'm' });
}

fn push_aggregate_fields(
    mangled: &mut String,
    definitions: &impl THIRTypeContext,
    fields: &[THIRField],
) {
    push_component(mangled, fields.len().to_string().as_str());
    for field in fields {
        let field_id = field.ty();
        if matches!(field, THIRField::Bitfield { .. }) {
            mangled.push('b');
        } else {
            mangled.push('f');
        }
        let field_type = definitions.resolve_type_id(field_id);
        let field_type = type_mangle(definitions, field_type);
        push_component(mangled, field_type.as_str());
    }
}

fn push_component(mangled: &mut String, component: &str) {
    mangled.push_str(component.len().to_string().as_str());
    mangled.push('_');
    mangled.push_str(component);
}

#[cfg(test)]
mod tests {
    use super::base_mangle_templated_name;
    use crate::thir::r#type::{THIRIntType, THIRType, THIRTypeID, THIRTypeKind};
    use crate::type_context::THIRTypeContext;
    use cx_target::ArchitectureConfig;

    struct TestTypes {
        types: Vec<THIRType>,
    }

    impl THIRTypeContext for TestTypes {
        fn architecture(&self) -> &ArchitectureConfig {
            static ARCHITECTURE: ArchitectureConfig = ArchitectureConfig::new(8, 8);
            &ARCHITECTURE
        }

        fn resolve_type_id(&self, id: THIRTypeID) -> &THIRType {
            &self.types[id.index()]
        }
    }

    #[test]
    fn template_mangling_distinguishes_name_and_argument_boundaries() {
        let integer = THIRType::from(THIRTypeKind::Integer {
            _type: THIRIntType::I32,
            signed: true,
        });
        let pointer = THIRType::from(THIRTypeKind::PointerTo {
            inner_type: THIRTypeID::new(0),
        });
        let types = TestTypes {
            types: vec![integer.clone()],
        };

        let pointer_argument = base_mangle_templated_name(&types, "foo", std::iter::once(&pointer));
        let integer_argument =
            base_mangle_templated_name(&types, "fooP", std::iter::once(&integer));

        assert_ne!(pointer_argument, integer_argument);
    }
}
