use cx_ast::ast::{
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
    environment::TypeEnvironment,
    symbol::resolution::{apply_template, resolve_symbol}, type_checking::{constexpr::constexpr_evaluate, typechecker::typecheck_expr},
};

pub fn complete_template_input(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    input: &CXTemplateInput,
) -> CXResult<MIRTemplateInput> {
    let args = input
        .params
        .iter()
        .map(|param| complete_type(env, namespace, param))
        .collect::<CXResult<Vec<_>>>()?;

    Ok(MIRTemplateInput { args })
}

pub fn complete_type(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ty: &CXType,
) -> CXResult<MIRType> {
    let mut completed = match &ty.kind {
        CXTypeKind::Identifier {
            name,
            predeclaration,
            template_input,
        } => complete_identifier_type(env, namespace, name, *predeclaration, template_input)?,

        CXTypeKind::ExplicitSizedArray(inner, size) => {
            let inner_type = complete_type(env, namespace, inner)?;
            let size = typecheck_expr(env, namespace, size, None)
                .and_then(|v| v.standard_ready_coerce(env, size.token_range()))
                .and_then(|v| constexpr_evaluate(env, v))
                .and_then(|v| {
                    v.get_integer().ok_or_else(|| {
                        CXError::create_boxed("Array size must be an integer literal")
                    })
                })?;
            let id = env.symbols.generate_type_id(inner_type);

            MIRTypeKind::Array {
                inner_type: id,
                length: size as usize,
            }
            .into()
        }

        CXTypeKind::ImplicitSizedArray(inner) => {
            let inner_type = complete_type(env, namespace, inner)?;
            let id = env.symbols.generate_type_id(inner_type);

            MIRTypeKind::PointerTo { inner_type: id }.into()
        }

        CXTypeKind::MemoryReference { inner_type } => {
            let inner_type = complete_type(env, namespace, inner_type)?;
            let id = env.symbols.generate_type_id(inner_type);

            MIRTypeKind::MemoryReference {
                inner_type: id,
                bitfield: None,
            }
            .into()
        }

        CXTypeKind::PointerTo { inner_type } => {
            let inner_type = complete_type(env, namespace, inner_type)?;
            let id = env.symbols.generate_type_id(inner_type);

            MIRTypeKind::PointerTo { inner_type: id }.into()
        }

        CXTypeKind::Structured {
            name,
            attributes,
            fields,
        } => make_aggregate_type(
            env,
            namespace,
            ty,
            name.clone(),
            Some(attributes),
            fields,
            |fields| MIRTypeKind::Structured { fields },
        )?,

        CXTypeKind::Union { name, fields } => {
            make_aggregate_type(env, namespace, ty, name.clone(), None, fields, |variants| {
                MIRTypeKind::Union { variants }
            })?
        }

        CXTypeKind::TaggedUnion {
            name,
            attributes,
            variants,
        } => make_aggregate_type(
            env,
            namespace,
            ty,
            Some(name.clone()),
            Some(attributes),
            variants,
            |variants| MIRTypeKind::TaggedUnion { variants },
        )?,

        CXTypeKind::FunctionPointer { prototype } => {
            let prototype = complete_prototype(env, namespace, prototype)?;
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
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    prototype: &CXFunctionPrototype,
) -> CXResult<MIRFunctionPrototype> {
    let return_type = complete_type(env, &namespace, &prototype.return_type)?;
    let params = prototype
        .params
        .iter()
        .map(|param| {
            Ok(MIRParameter {
                name: param.name.clone(),
                _type: complete_type(env, &namespace, &param._type)?,
            })
        })
        .collect::<CXResult<Vec<_>>>()?;
    let name = completed_function_name(env, &namespace, &prototype.kind)?;

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
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    predeclaration: PredeclarationType,
    template_input: &Option<CXTemplateInput>,
) -> CXResult<MIRType> {
    if let Some(_ty) = env.symbols.get_preresolved_symbol(name) {
        return Ok(_ty
            .as_type_id()
            .map(|id| env.symbols.resolve_type_id(id).clone())
            .unwrap()); // unfailable
    }

    let alias_name = env.symbols.resolve_qualified_alias(name);

    let Some(symbol) = env
        .symbols
        .get_global_registry()
        .resolve(alias_name.as_ref())
    else {
        if predeclaration != PredeclarationType::None {
            return Ok(MIRTypeKind::Undefined.into());
        }

        return CXError::create_result(format!("Type not found: {name}"));
    };

    let prereserved_id = env.symbols.reserve_type_id();
    env.symbols.insert_type_symbol(name.clone(), prereserved_id);
    env.symbols
        .overwrite_type_id(prereserved_id, MIRTypeKind::Undefined.into());

    let mir_symbol = resolve_symbol(env, &name.namespace, &name.name, &symbol)?;

    let symbol = if let Some(input) = template_input {
        let input = complete_template_input(env, namespace, input)?;
        apply_template(env, &mir_symbol, input)?.ok_or_else(|| {
            CXError::create_boxed(format!("Type '{name}' does not accept template arguments"))
        })?
    } else {
        mir_symbol
    };

    match symbol {
        MIRSymbol::Type(id) => env
            .symbols
            .undo_type_id(id)
            .map(|ty| {
                env.symbols.overwrite_type_id(prereserved_id, ty.clone());
                ty
            })
            .ok_or_else(|| unreachable!("Type ID {id:?} was not found in the registry")),
        MIRSymbol::Template { .. } => CXError::create_result(format!(
            "Template type '{name}' requires explicit template arguments"
        )),
        _ => CXError::create_result(format!("Symbol '{name}' is not a type")),
    }
}

fn make_aggregate_type<F>(
    env: &mut TypeEnvironment,
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
        .map(|field| complete_field(env, namespace, field))
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
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    field: &CXField,
) -> CXResult<MIRField> {
    match field {
        CXField::Standard { name, _type } => {
            let ty = complete_type(env, namespace, _type)?;
            let id = env.symbols.generate_type_id(ty);

            Ok(MIRField::standard(name.clone(), id))
        }

        CXField::Bitfield {
            name,
            integer_type,
            width,
        } => {
            let ty = complete_type(env, namespace, integer_type)?;
            let id = env.symbols.generate_type_id(ty);

            Ok(MIRField::Bitfield {
                name: name.clone(),
                integer_type_id: id,
                width: *width,
            })
        }
    }
}

fn completed_function_name(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    kind: &CXFunctionKind,
) -> CXResult<CXIdent> {
    let name = match kind {
        CXFunctionKind::Standard(name) => base_mangle_standard(
            env.symbols.get_global_registry(),
            &QualifiedName::new(namespace.clone(), name.clone()),
        ),
        CXFunctionKind::MemberFunction {
            member_type, name, ..
        } => {
            let member_type = complete_type(env, namespace, &member_type.as_type())?;
            base_mangle_member(&env.symbols, name.as_str(), &member_type)
        }
        CXFunctionKind::StaticMemberFunction { member_type, name } => {
            let member_type = complete_type(env, namespace, &member_type.as_type())?;
            base_mangle_static_member(&env.symbols, name.as_str(), &member_type)
        }
    };

    Ok(CXIdent::new(name))
}
