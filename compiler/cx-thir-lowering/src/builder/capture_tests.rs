use super::*;
use crate::lowering::capture::capture_expression;
use cx_mir::{MIRConstant, MIRIntType};
use cx_thir::thir::expression::THIRExpressionKind;

fn builder(registry: &THIRDecomposedRegistry, safe: bool) -> MIRBuilder<'_> {
    let mut builder = MIRBuilder {
        types: MIRTypeRegistryBuilder::new(ArchitectureConfig::native()),
        module: MIRModuleBuilder::new(),
        registry,
        function: None,
    };
    let ty = builder.types.intern(MIRType::new(
        MIRTypeKind::Integer {
            ty: MIRIntType::I32,
            signed: true,
        },
        None,
    ));
    builder.start_custom_function(
        MIRFunction::new(
            MIRFunctionID::new(0),
            MIRFnPrototype::new(
                MIRFnSignature::new(
                    CXIdent::from("caller"),
                    None,
                    Vec::new(),
                    ty,
                    MIRFunctionMode::Runtime,
                    false,
                    safe,
                ),
                LinkageMode::Static,
            ),
            None,
        ),
        None,
    );
    builder.set_source_range(TokenRange::error("caller expression"));
    builder
}

fn registry() -> THIRDecomposedRegistry {
    THIRDecomposedRegistry::new(
        ArchitectureConfig::native(),
        HashMap::new(),
        HashMap::new(),
        0,
    )
}

fn integer(value: i64) -> THIRExpression {
    THIRExpression {
        kind: THIRExpressionKind::IntLiteral(value),
        _type: THIRType::from(THIRTypeKind::Integer {
            _type: THIRIntType::I32,
            signed: true,
        }),
        token_range: TokenRange::internal(),
    }
}

#[test]
fn capture_preserves_safe_context() {
    let registry = registry();
    for safe in [false, true] {
        let mut builder = builder(&registry, safe);
        let captured = capture_expression(&mut builder, &integer(42))
            .unwrap_or_else(|error| panic!("{}", error.message()));

        assert_eq!(captured.prototype().signature.safe, safe);
        assert_eq!(builder.fun().id(), MIRFunctionID::new(0));
        assert_eq!(builder.fun().prototype().signature.safe, safe);
        assert_eq!(
            builder.source_range(),
            &TokenRange::error("caller expression")
        );
    }
}

#[test]
fn capture_restores_function_after_expression_error() {
    let registry = registry();
    let mut builder = builder(&registry, true);
    let local = THIRLocalID::fresh();
    builder
        .fun_mut()
        .bind_local(local, MIRValue::Constant(MIRConstant::Bool(true)));
    let expression = THIRExpression {
        kind: THIRExpressionKind::Variable {
            name: CXIdent::from("missing"),
            local_id: THIRLocalID::fresh(),
        },
        ..integer(0)
    };

    let error = capture_expression(&mut builder, &expression).expect_err("missing local");

    assert_eq!(error.code(), "MX055");
    assert_eq!(
        builder.try_fun().map(|function| function.id()),
        Some(MIRFunctionID::new(0)),
        "expression lowering failure must restore the enclosing function"
    );
    assert!(matches!(
        builder.fun().local(local),
        Some(MIRValue::Constant(MIRConstant::Bool(true)))
    ));
    assert_eq!(
        builder.source_range(),
        &TokenRange::error("caller expression")
    );
}

#[test]
fn capture_restores_function_after_type_error() {
    let registry = registry();
    let mut builder = builder(&registry, true);
    let expression = THIRExpression {
        _type: THIRType::from(THIRTypeKind::Array {
            length: Box::new(integer(-1)),
            inner_type: THIRTypeID::new(0),
        }),
        ..THIRExpression::default()
    };

    let error = capture_expression(&mut builder, &expression).expect_err("negative array length");

    assert_eq!(error.code(), "M0073");
    assert_eq!(
        builder.try_fun().map(|function| function.id()),
        Some(MIRFunctionID::new(0)),
        "type lowering failure must restore the enclosing function"
    );
    assert_eq!(
        builder.source_range(),
        &TokenRange::error("caller expression")
    );
}
