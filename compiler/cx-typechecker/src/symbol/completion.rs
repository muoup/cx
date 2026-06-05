use cx_ast::ast::{
    expression::{CXExprKind, CXExpression},
    function::{CXFunctionKind, CXFunctionPrototype},
    modifiers::VisibilityMode,
    template::CXTemplateInput,
    types::{CXField, CXType, CXTypeKind, PredeclarationType},
};
use cx_util::{CXError, CXResult, identifier::CXIdent, namespace::QualifiedName};

use cx_mir::{
    mir::{
        data::{
            MIRFunctionPrototype, MIRFunctionSignature, MIRMoveAttributes, MIRParameter,
            MIRTemplateInput,
        },
        name_mangling::{base_mangle_member, base_mangle_standard, base_mangle_static_member},
        r#type::{MIRField, MIRType, MIRTypeKind},
    },
    symbol::MIRSymbol,
    type_context::MIRTypeContext,
};

use crate::{
    EnvironmentNamespace,
    symbol::{registry::MIRSymbolRegistry, resolution::{apply_template, resolve_symbol}},
};

pub fn complete_template_input(
    symbols: &mut MIRSymbolRegistry,
    namespace: &EnvironmentNamespace,
    input: &CXTemplateInput,
) -> CXResult<MIRTemplateInput> {
    let args = input
        .params
        .iter()
        .map(|param| complete_type(symbols, namespace, param))
        .collect::<CXResult<Vec<_>>>()?;

    Ok(MIRTemplateInput { args })
}

pub fn complete_type(
    symbols: &mut MIRSymbolRegistry,
    namespace: &EnvironmentNamespace,
    ty: &CXType,
) -> CXResult<MIRType> {
    let mut completed = match &ty.kind {
        CXTypeKind::Identifier {
            name,
            predeclaration,
            template_input,
        } => complete_identifier_type(symbols, namespace, name, *predeclaration, template_input)?,

        CXTypeKind::ExplicitSizedArray(inner, size) => {
            let inner_type = complete_type(symbols, namespace, inner)?;
            let id = symbols.generate_type_id(inner_type);

            MIRTypeKind::Array {
                inner_type: id,
                length: literal_array_size(size)?,
            }
            .into()
        }

        CXTypeKind::ImplicitSizedArray(inner) => {
            let inner_type = complete_type(symbols, namespace, inner)?;
            let id = symbols.generate_type_id(inner_type);

            MIRTypeKind::PointerTo { inner_type: id }.into()
        }

        CXTypeKind::MemoryReference { inner_type } => {
            let inner_type = complete_type(symbols, namespace, inner_type)?;
            let id = symbols.generate_type_id(inner_type);

            MIRTypeKind::MemoryReference {
                inner_type: id,
                bitfield: None,
            }
            .into()
        }

        CXTypeKind::PointerTo { inner_type } => {
            let inner_type = complete_type(symbols, namespace, inner_type)?;
            let id = symbols.generate_type_id(inner_type);

            MIRTypeKind::PointerTo { inner_type: id }.into()
        }

        CXTypeKind::Structured {
            name,
            attributes,
            fields,
        } => make_aggregate_type(
            symbols,
            namespace,
            ty,
            name.clone(),
            Some(attributes),
            fields,
            |fields| MIRTypeKind::Structured { fields },
        )?,

        CXTypeKind::Union { name, fields } => make_aggregate_type(
            symbols,
            namespace,
            ty,
            name.clone(),
            None,
            fields,
            |variants| MIRTypeKind::Union { variants },
        )?,

        CXTypeKind::TaggedUnion {
            name,
            attributes,
            variants,
        } => make_aggregate_type(
            symbols,
            namespace,
            ty,
            Some(name.clone()),
            Some(attributes),
            variants,
            |variants| MIRTypeKind::TaggedUnion { variants },
        )?,

        CXTypeKind::FunctionPointer { prototype } => {
            let prototype = complete_prototype(symbols, namespace, prototype)?;
            MIRTypeKind::Function {
                signature: Box::new(prototype.signature),
            }
            .into()
        }
    };

    completed.specifiers = ty.specifiers;
    Ok(completed)
}

pub fn complete_prototype(
    symbols: &mut MIRSymbolRegistry,
    namespace: &EnvironmentNamespace,
    prototype: &CXFunctionPrototype,
) -> CXResult<MIRFunctionPrototype> {
    let return_type = complete_type(symbols, &namespace, &prototype.return_type)?;
    let params = prototype
        .params
        .iter()
        .map(|param| {
            Ok(MIRParameter {
                name: param.name.clone(),
                _type: complete_type(symbols, &namespace, &param._type)?,
            })
        })
        .collect::<CXResult<Vec<_>>>()?;
    let name = completed_function_name(symbols, &namespace, &prototype.kind)?;

    Ok(MIRFunctionPrototype {
        name,
        linkage: prototype.linkage,
        signature: MIRFunctionSignature {
            return_type,
            params,
            var_args: prototype.var_args,
            contract: prototype.contract.clone(),
        },
    })
}

fn complete_identifier_type(
    symbols: &mut MIRSymbolRegistry,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    predeclaration: PredeclarationType,
    template_input: &Option<CXTemplateInput>,
) -> CXResult<MIRType> {
    if let Some(_ty) = symbols.get_preresolved_symbol(name) {
        return Ok(_ty
            .as_type_id()
            .map(|id| symbols.resolve_type_id(id).clone())
            .unwrap()); // unfailable
    }

    let id = symbols.reserve_type_id();
    symbols.insert_type_symbol(name.clone(), id);

    let alias_name = symbols.resolve_qualified_alias(name);

    let Some(symbol) = symbols.get_global_registry().resolve(alias_name.as_ref()) else {
        if predeclaration != PredeclarationType::None {
            return Ok(MIRTypeKind::Undefined.into());
        }

        return CXError::create_result(format!("Type not found: {name}"));
    };
    
    let mir_symbol = resolve_symbol(symbols, &name.namespace, &name.name, &symbol)?;

    let symbol = if let Some(input) = template_input {
        let input = complete_template_input(symbols, namespace, input)?;
        apply_template(symbols, &mir_symbol, input)?.ok_or_else(|| {
            CXError::create_boxed(format!("Type '{name}' does not accept template arguments"))
        })?
    } else {
        mir_symbol
    };

    match symbol {
        MIRSymbol::Type(id) => Ok(symbols.resolve_type_id(id).clone()),
        MIRSymbol::Template { .. } => CXError::create_result(format!(
            "Template type '{name}' requires explicit template arguments"
        )),
        _ => CXError::create_result(format!("Symbol '{name}' is not a type")),
    }
}

fn make_aggregate_type<F>(
    symbols: &mut MIRSymbolRegistry,
    namespace: &EnvironmentNamespace,
    ty: &CXType,
    name: Option<CXIdent>,
    attributes: Option<&cx_ast::ast::types::CXStructAttributes>,
    fields: &[CXField],
    kind_ctor: F,
) -> CXResult<MIRType>
where
    F: FnOnce(Vec<MIRField>) -> MIRTypeKind,
{
    let fields = fields
        .iter()
        .map(|field| complete_field(symbols, namespace, field))
        .collect::<CXResult<Vec<_>>>()?;
    let move_attributes = attributes
        .map(|attributes| MIRMoveAttributes {
            nocopy: attributes.nocopy || attributes.nodrop,
            nodrop: attributes.nodrop,
        })
        .unwrap_or_default();

    Ok(MIRType {
        visibility: VisibilityMode::Private,
        specifiers: ty.specifiers,
        move_attributes,
        strong_identifier: name
            .as_ref()
            .map(|name| QualifiedName::new(namespace.clone(), name.clone())),
        debug_name: name,
        template_info: None,
        kind: kind_ctor(fields),
    })
}

fn complete_field(
    symbols: &mut MIRSymbolRegistry,
    namespace: &EnvironmentNamespace,
    field: &CXField,
) -> CXResult<MIRField> {
    match field {
        CXField::Standard { name, _type } => {
            let ty = complete_type(symbols, namespace, _type)?;
            let id = symbols.generate_type_id(ty);

            Ok(MIRField::standard(name.clone(), id))
        }

        CXField::Bitfield {
            name,
            integer_type,
            width,
        } => {
            let ty = complete_type(symbols, namespace, integer_type)?;
            let id = symbols.generate_type_id(ty);

            Ok(MIRField::Bitfield {
                name: name.clone(),
                integer_type_id: id,
                width: *width,
            })
        }
    }
}

fn completed_function_name(
    symbols: &mut MIRSymbolRegistry,
    namespace: &EnvironmentNamespace,
    kind: &CXFunctionKind,
) -> CXResult<CXIdent> {
    let name = match kind {
        CXFunctionKind::Standard(name) => base_mangle_standard(
            symbols.get_global_registry(),
            &QualifiedName::new(namespace.clone(), name.clone()),
        ),
        CXFunctionKind::MemberFunction {
            member_type, name, ..
        } => {
            let member_type = complete_type(symbols, namespace, &member_type.as_type())?;
            base_mangle_member(symbols, name.as_str(), &member_type)
        }
        CXFunctionKind::StaticMemberFunction { member_type, name } => {
            let member_type = complete_type(symbols, namespace, &member_type.as_type())?;
            base_mangle_static_member(symbols, name.as_str(), &member_type)
        }
    };

    Ok(CXIdent::new(name))
}

fn literal_array_size(expr: &CXExpression) -> CXResult<usize> {
    let CXExprKind::IntLiteral { val, .. } = expr.kind else {
        return CXError::create_result("Array size must be an integer literal");
    };

    if val < 0 {
        return CXError::create_result("Array size cannot be negative");
    }

    Ok(val as usize)
}
