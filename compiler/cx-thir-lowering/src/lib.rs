use cx_log::CXResult;
use cx_mir::MIRUnit;
use cx_thir::THIRUnit;

pub mod builder;
mod lowering;

pub use builder::MIRBuilder;

/// Lowers target-independent THIR into semantic MIR.
///
/// This pass deliberately does not classify ABI values, calculate target
/// layouts, or compute liveness. THIR lifetime markers and resolved cleanup
/// expressions are preserved as explicit MIR instructions instead.
pub fn generate_mir(thir: &THIRUnit) -> CXResult<MIRUnit> {
    let mut builder = MIRBuilder::new(thir);
    lowering::lower_unit(&mut builder, thir)?;
    Ok(builder.finish())
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use cx_ast::ast::modifiers::CXLinkageMode;
    use cx_mir::MIRInstrKind;
    use cx_thir::{
        EnvironmentNamespace, THIRUnit,
        registry::THIRDecomposedRegistry,
        thir::{
            data::{THIRFnPrototype, THIRFnSignature, THIRFunction},
            expression::{THIRExpression, THIRExpressionKind},
            r#type::{THIRIntType, THIRType, THIRTypeKind},
        },
    };

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
            MIRInstrKind::CreatePlace { .. }
        ));
        assert!(matches!(
            instructions.last().unwrap().kind,
            MIRInstrKind::Return { .. }
        ));
    }
}
