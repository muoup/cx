use cx_log::CXResult;
use cx_mir::MIRUnit;
use cx_thir::THIRUnit;

pub mod builder;
mod lowering;

pub use builder::MIRBuilder;

/// Lowers target-independent THIR into semantic MIR.
///
/// This pass deliberately does not classify ABI values, calculate target
/// layouts, or compute liveness. Resolved cleanup expressions and semantic
/// ownership effects such as initialization, moves, and leaks are preserved in
/// MIR, while lexical lifetime-end markers lower to no operation.
pub fn generate_mir(thir: &THIRUnit) -> CXResult<MIRUnit> {
    let mut builder = MIRBuilder::new(thir);
    lowering::lower_unit(&mut builder, thir)?;
    Ok(builder.finish())
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use cx_ast::ast::modifiers::CXLinkageMode;
    use cx_mir::{MIRGlobalInitializer, MIRInstrKind, MIRParameterID, MIRPlace, MIRValue};
    use cx_thir::{
        EnvironmentNamespace, THIRUnit,
        registry::THIRDecomposedRegistry,
        thir::{
            data::{THIRFnPrototype, THIRFnSignature, THIRFunction, THIRParameter},
            expression::{SymbolValueOrigin, THIRExpression, THIRExpressionKind, THIRLocalID},
            global::{MIRGlobalVarKind, MIRGlobalVariable},
            r#type::{THIRIntType, THIRType, THIRTypeKind},
        },
    };
    use cx_util::identifier::CXIdent;

    use super::generate_mir;

    fn expression(kind: THIRExpressionKind, ty: THIRType) -> THIRExpression {
        THIRExpression {
            kind,
            _type: ty,
            ..THIRExpression::default()
        }
    }

    fn unit_with_body(return_type: THIRType, body: THIRExpression) -> THIRUnit {
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
            registry: THIRDecomposedRegistry::new(Default::default(), HashMap::new()),
        }
    }

    #[test]
    fn lowers_return_value_into_displayable_valid_mir() {
        let int_type = THIRType::from(THIRTypeKind::Integer {
            _type: THIRIntType::I32,
            signed: true,
        });
        let value = expression(THIRExpressionKind::IntLiteral(42), int_type.clone());
        let body = expression(
            THIRExpressionKind::Return {
                postcondition: None,
                value: Some(Box::new(value)),
                cleanups: Vec::new(),
            },
            THIRType::unit(),
        );

        let mir = generate_mir(&unit_with_body(int_type, body))
            .unwrap_or_else(|error| panic!("{}", error.message()));
        mir.validate().unwrap();
        let display = mir.to_string();
        assert!(display.contains("return 42:i32"));
        assert!(display.contains("mir v0"));
    }

    #[test]
    fn lowers_cleanup_before_return_terminator() {
        let cleanup = expression(
            THIRExpressionKind::RegionCreate {
                _type: THIRType::unit(),
                initial_value: None,
            },
            THIRType::unit(),
        );
        let body = expression(
            THIRExpressionKind::Return {
                postcondition: None,
                value: None,
                cleanups: vec![cleanup],
            },
            THIRType::unit(),
        );

        let mir = generate_mir(&unit_with_body(THIRType::unit(), body))
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
        let int_type = THIRType::from(THIRTypeKind::Integer {
            _type: THIRIntType::I32,
            signed: true,
        });
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
            THIRType::unit(),
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
            registry: THIRDecomposedRegistry::new(Default::default(), HashMap::new()),
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
        let int_type = THIRType::from(THIRTypeKind::Integer {
            _type: THIRIntType::I32,
            signed: true,
        });
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
            THIRType::unit(),
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
            registry: THIRDecomposedRegistry::new(Default::default(), HashMap::new()),
        };

        let mir = generate_mir(&unit).unwrap_or_else(|error| panic!("{}", error.message()));
        mir.validate().unwrap();
        assert!(mir.functions[0].places.is_empty());
        assert!(matches!(
            mir.functions[0].blocks[0].instrs[0].kind,
            MIRInstrKind::Return {
                value: Some(MIRValue::Move(MIRPlace::Parameter(id)))
            } if id == MIRParameterID::new(0)
        ));
    }

    #[test]
    fn string_literals_become_readonly_global_bytes() {
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
            registry: THIRDecomposedRegistry::new(Default::default(), HashMap::new()),
        };

        let mir = generate_mir(&unit).unwrap_or_else(|error| panic!("{}", error.message()));
        mir.validate().unwrap();
        assert!(!mir.globals[0].is_mutable);
        assert_eq!(
            mir.globals[0].initializer,
            Some(MIRGlobalInitializer::Bytes(
                b"hello".to_vec().into_boxed_slice()
            ))
        );
    }
}
