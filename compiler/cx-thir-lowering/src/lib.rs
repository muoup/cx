use cx_log::CXResult;
use cx_mir::MIRUnit;
use cx_thir::THIRUnit;

pub mod builder;
mod lowering;

pub use builder::MIRBuilder;

pub fn generate_mir(thir: &THIRUnit) -> CXResult<MIRUnit> {
    let mut builder = MIRBuilder::new(thir);
    lowering::lower_unit(&mut builder, thir)?;
    let mut mir = builder.finish();
    mir.compute_layouts().map_err(|error| {
        cx_log::error::CXErr::new(
            cx_log::error::message::CXStdErrMessage::error("MIRLayoutError", error.to_string()),
            cx_log::error::context::CXInternalContext::error(
                "MIR layout calculation failed during MIR generation",
            ),
        )
    })?;
    Ok(mir)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use cx_ast::ast::modifiers::CXLinkageMode;
    use cx_mir::{
        MIRConstant, MIRGlobalState, MIRInstrKind, MIRParameterID, MIRPlace, MIRTypeID, MIRValue,
    };
    use cx_thir::{
        registry::THIRDecomposedRegistry,
        thir::{
            data::{THIRFnPrototype, THIRFnSignature, THIRFunction, THIRParameter},
            expression::{SymbolValueOrigin, THIRExpression, THIRExpressionKind, THIRLocalID},
            global::{MIRGlobalVarKind, MIRGlobalVariable},
            r#type::{THIRIntType, THIRType, THIRTypeID, THIRTypeKind},
        },
        EnvironmentNamespace, THIRUnit,
    };
    use cx_util::{identifier::CXIdent, namespace::QualifiedName};

    use super::generate_mir;

    fn test_registry() -> (THIRDecomposedRegistry, THIRType, THIRType) {
        let unit_id = THIRTypeID(0);
        let int_id = THIRTypeID(1);
        let mut unit = THIRType::unit();
        unit.lookup_identifier = Some(QualifiedName::new_raw(CXIdent::from("void")));
        let mut int = THIRType::from(THIRTypeKind::Integer {
            _type: THIRIntType::I32,
            signed: true,
        });
        int.lookup_identifier = Some(QualifiedName::new_raw(CXIdent::from("int")));
        let registry = THIRDecomposedRegistry::new(
            Default::default(),
            HashMap::from([(unit_id, unit.clone()), (int_id, int.clone())]),
            HashMap::from([("void".to_owned(), unit_id), ("int".to_owned(), int_id)]),
            2,
        );
        (registry, unit, int)
    }

    fn expression(kind: THIRExpressionKind, ty: THIRType) -> THIRExpression {
        THIRExpression {
            kind,
            _type: ty,
            ..THIRExpression::default()
        }
    }

    fn unit_with_body(return_type: THIRType, body: THIRExpression) -> THIRUnit {
        let (registry, _, _) = test_registry();
        let prototype = THIRFnPrototype::new(
            "mir_test",
            CXLinkageMode::Standard,
            THIRFnSignature {
                return_type,
                ..THIRFnSignature::default()
            },
        );
        THIRUnit {
            source_namespace: EnvironmentNamespace::root(),
            functions: vec![THIRFunction { prototype, body }],
            global_variables: Vec::new(),
            registry,
        }
    }

    #[test]
    fn lowers_return_value_into_displayable_valid_mir() {
        let (_, unit_type, int_type) = test_registry();
        let value = expression(THIRExpressionKind::IntLiteral(42), int_type.clone());
        let body = expression(
            THIRExpressionKind::Return {
                postcondition: None,
                value: Some(Box::new(value)),
                cleanups: Vec::new(),
            },
            unit_type,
        );

        let mir = generate_mir(&unit_with_body(int_type, body))
            .unwrap_or_else(|error| panic!("{}", error.message()));
        mir.validate().unwrap();
        assert_eq!(
            mir.functions[0].prototype.signature.return_type,
            Some(MIRTypeID::from_raw(1))
        );
        let display = mir.to_string();
        assert!(display.contains("return 42:i32"));
        assert!(display.contains("fn mir_test"));
    }

    #[test]
    fn lowers_cleanup_before_return_terminator() {
        let (_, unit_type, _) = test_registry();
        let cleanup = expression(
            THIRExpressionKind::RegionCreate {
                _type: unit_type.clone(),
                initial_value: None,
            },
            unit_type.clone(),
        );
        let body = expression(
            THIRExpressionKind::Return {
                postcondition: None,
                value: None,
                cleanups: vec![cleanup],
            },
            unit_type.clone(),
        );

        let mir = generate_mir(&unit_with_body(unit_type, body))
            .unwrap_or_else(|error| panic!("{}", error.message()));
        mir.validate().unwrap();
        let instructions = &mir.functions[0].blocks[0].instrs;
        assert!(matches!(
            instructions[instructions.len() - 2].kind,
            MIRInstrKind::Create { .. }
        ));
        assert!(matches!(
            instructions.last().unwrap().kind,
            MIRInstrKind::Return { .. }
        ));
    }

    #[test]
    fn parameters_are_places_without_entry_copies() {
        let (registry, unit_type, int_type) = test_registry();
        let local_id = THIRLocalID(0);
        let name = CXIdent::from("value");
        let value = expression(
            THIRExpressionKind::Variable {
                name: name.clone(),
                local_id: Some(local_id),
                location: SymbolValueOrigin::Local,
            },
            int_type.clone(),
        );
        let body = expression(
            THIRExpressionKind::Return {
                postcondition: None,
                value: Some(Box::new(value)),
                cleanups: Vec::new(),
            },
            unit_type,
        );
        let prototype = THIRFnPrototype::new(
            "parameter_place",
            CXLinkageMode::Standard,
            THIRFnSignature {
                return_type: int_type.clone(),
                params: vec![THIRParameter {
                    name: Some(name),
                    local_id: Some(local_id),
                    _type: int_type,
                }],
                ..THIRFnSignature::default()
            },
        );
        let unit = THIRUnit {
            source_namespace: EnvironmentNamespace::root(),
            functions: vec![THIRFunction { prototype, body }],
            global_variables: Vec::new(),
            registry,
        };

        let mir = generate_mir(&unit).unwrap_or_else(|error| panic!("{}", error.message()));
        mir.validate().unwrap();
        assert!(mir.functions[0].places.is_empty());
        assert!(matches!(
            mir.functions[0].blocks[0].instrs[0].kind,
            MIRInstrKind::Return {
                value: Some(MIRValue::Place(MIRPlace::Parameter(id)))
            } if id == MIRParameterID::new(0)
        ));
    }

    #[test]
    fn region_move_stays_attached_to_its_consumer() {
        let (registry, unit_type, int_type) = test_registry();
        let local_id = THIRLocalID(0);
        let name = CXIdent::from("value");
        let source = expression(
            THIRExpressionKind::Variable {
                name: name.clone(),
                local_id: Some(local_id),
                location: SymbolValueOrigin::Local,
            },
            int_type.clone(),
        );
        let moved = expression(
            THIRExpressionKind::RegionMove {
                source: Box::new(source),
            },
            int_type.clone(),
        );
        let body = expression(
            THIRExpressionKind::Return {
                postcondition: None,
                value: Some(Box::new(moved)),
                cleanups: Vec::new(),
            },
            unit_type,
        );
        let prototype = THIRFnPrototype::new(
            "inline_move",
            CXLinkageMode::Standard,
            THIRFnSignature {
                return_type: int_type.clone(),
                params: vec![THIRParameter {
                    name: Some(name),
                    local_id: Some(local_id),
                    _type: int_type,
                }],
                ..THIRFnSignature::default()
            },
        );
        let unit = THIRUnit {
            source_namespace: EnvironmentNamespace::root(),
            functions: vec![THIRFunction { prototype, body }],
            global_variables: Vec::new(),
            registry,
        };

        let mir = generate_mir(&unit).unwrap_or_else(|error| panic!("{}", error.message()));
        mir.validate().unwrap();
        assert!(matches!(
            mir.functions[0].blocks[0].instrs[0].kind,
            MIRInstrKind::Return {
                value: Some(MIRValue::Move(MIRPlace::Parameter(id)))
            } if id == MIRParameterID::new(0)
        ));
    }

    #[test]
    fn string_literals_become_readonly_global_constants() {
        let (registry, _, _) = test_registry();
        let unit = THIRUnit {
            source_namespace: EnvironmentNamespace::root(),
            functions: Vec::new(),
            global_variables: vec![MIRGlobalVariable {
                kind: MIRGlobalVarKind::StringLiteral {
                    name: CXIdent::from("literal"),
                    value: "hello".into(),
                },
                is_mutable: false,
                linkage: CXLinkageMode::Static,
            }],
            registry,
        };

        let mir = generate_mir(&unit).unwrap_or_else(|error| panic!("{}", error.message()));
        mir.validate().unwrap();
        assert!(!mir.globals[0].is_mutable);
        assert_eq!(
            mir.globals[0].state,
            MIRGlobalState::Initialized(MIRConstant::String("hello".into()))
        );
    }
}
