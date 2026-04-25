use cx_ast::ast::VisibilityMode;
use cx_mir::mir::data::{MIRIntegerType, MIRType, MIRTypeContext, MIRTypeId, MIRTypeKind};
use cx_mir::mir::r#type::MIRField;
use cx_util::identifier::CXIdent;

fn u32_type() -> MIRType {
    MIRType {
        kind: MIRTypeKind::Integer {
            _type: MIRIntegerType::I32,
            signed: false,
        },
        ..Default::default()
    }
}

#[test]
fn contextual_eq_resolves_nested_type_ids() {
    let mut definitions = MIRTypeContext::default();
    let left_inner = definitions.intern(u32_type());
    let right_inner = definitions.intern(u32_type());

    let left = MIRType {
        kind: MIRTypeKind::PointerTo {
            inner_type: left_inner,
        },
        ..Default::default()
    };
    let right = MIRType {
        kind: MIRTypeKind::PointerTo {
            inner_type: right_inner,
        },
        ..Default::default()
    };

    assert!(left.contextual_eq(&right, &definitions));
}

#[test]
fn contextual_eq_rejects_different_strong_identifiers() {
    let mut definitions = MIRTypeContext::default();
    let field_type = definitions.intern(u32_type());

    let left = MIRType {
        strong_identifier: Some(CXIdent::new("Left")),
        kind: MIRTypeKind::Structured {
            fields: vec![MIRField::standard("value".to_string(), field_type)],
        },
        ..Default::default()
    };
    let right = MIRType {
        strong_identifier: Some(CXIdent::new("Right")),
        kind: MIRTypeKind::Structured {
            fields: vec![MIRField::standard("value".to_string(), field_type)],
        },
        ..Default::default()
    };

    assert!(!left.contextual_eq(&right, &definitions));
}

#[test]
fn contextual_eq_ignores_visibility() {
    let left = MIRType {
        visibility: VisibilityMode::Private,
        ..u32_type()
    };
    let right = MIRType {
        visibility: VisibilityMode::Public,
        ..u32_type()
    };

    assert!(left.contextual_eq(&right, &MIRTypeContext::default()));
}

#[test]
fn contextual_eq_handles_recursive_structures() {
    let mut definitions = MIRTypeContext::default();
    definitions.insert(
        MIRTypeId(1),
        MIRType {
            kind: MIRTypeKind::Structured {
                fields: vec![MIRField::standard("next".to_string(), MIRTypeId(3))],
            },
            ..Default::default()
        },
    );
    definitions.insert(
        MIRTypeId(2),
        MIRType {
            kind: MIRTypeKind::Structured {
                fields: vec![MIRField::standard("next".to_string(), MIRTypeId(4))],
            },
            ..Default::default()
        },
    );
    definitions.insert(
        MIRTypeId(3),
        MIRType {
            kind: MIRTypeKind::PointerTo {
                inner_type: MIRTypeId(1),
            },
            ..Default::default()
        },
    );
    definitions.insert(
        MIRTypeId(4),
        MIRType {
            kind: MIRTypeKind::PointerTo {
                inner_type: MIRTypeId(2),
            },
            ..Default::default()
        },
    );

    let left = definitions.get(MIRTypeId(1)).unwrap();
    let right = definitions.get(MIRTypeId(2)).unwrap();

    assert!(left.contextual_eq(right, &definitions));
}
