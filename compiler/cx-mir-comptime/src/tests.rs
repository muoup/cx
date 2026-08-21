use std::collections::HashMap;

use cx_mir::{
    MIRBinaryOp, MIRBlockTarget, MIRConstant, MIRFnPrototype, MIRFnSignature, MIRFunction,
    MIRFunctionDefinition, MIRFunctionID, MIRFunctionMode, MIRGlobalID, MIRGlobalKind,
    MIRGlobalState, MIRGlobalVariable, MIRInstrKind, MIRIntBinaryOp, MIRIntType, MIRType,
    MIRTypeID, MIRTypeKind, MIRTypeLayout, MIRTypeRegistryBuilder, MIRUnit, MIRValue,
    MIRValueAggregateOp,
};
use cx_target::ArchitectureConfig;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{MIRComptimeEngine, materialize_globals};

fn types() -> (MIRTypeRegistryBuilder, MIRTypeID, MIRTypeID, MIRTypeID) {
    let mut types = MIRTypeRegistryBuilder::new(ArchitectureConfig::native());
    let unit = types.intern(MIRType::new(
        MIRTypeKind::Void,
        Some(MIRTypeLayout {
            size: 0,
            alignment: 1,
        }),
    ));
    let integer = types.intern(MIRType::new(
        MIRTypeKind::Integer {
            ty: MIRIntType::I32,
            signed: true,
        },
        Some(MIRTypeLayout {
            size: 4,
            alignment: 4,
        }),
    ));
    let byte = types.intern(MIRType::new(
        MIRTypeKind::Integer {
            ty: MIRIntType::I8,
            signed: false,
        },
        Some(MIRTypeLayout {
            size: 1,
            alignment: 1,
        }),
    ));
    (types, unit, integer, byte)
}

fn function(
    id: MIRFunctionID,
    return_type: MIRTypeID,
    definition: MIRFunctionDefinition,
) -> MIRFunction {
    MIRFunction::defined(
        id,
        MIRFnPrototype::new(
            MIRFnSignature::new(CXIdent::new(format!("test_{id}")), Vec::new(), return_type),
            LinkageMode::Static,
        ),
        definition,
        MIRFunctionMode::ComptimeOnly,
    )
}

fn unit(
    types: MIRTypeRegistryBuilder,
    functions: impl IntoIterator<Item = (MIRFunctionID, MIRFunction)>,
    globals: impl IntoIterator<Item = (MIRGlobalID, MIRGlobalVariable)>,
    global_order: Vec<MIRGlobalID>,
) -> MIRUnit {
    MIRUnit::new(
        types,
        functions.into_iter().collect::<HashMap<_, _>>(),
        globals.into_iter().collect::<HashMap<_, _>>(),
        global_order,
    )
}

fn integer(value: i128) -> MIRConstant {
    MIRConstant::Integer {
        value,
        ty: MIRIntType::I32,
        signed: true,
    }
}

#[test]
fn evaluates_scalar_instruction() {
    let (types, _, integer_type, _) = types();
    let mut definition = MIRFunctionDefinition::new();
    let entry = definition.add_block();
    let result = definition.add_register(integer_type, None);
    definition
        .block_mut(entry)
        .unwrap()
        .push(MIRInstrKind::BinOp {
            out: result,
            op: MIRBinaryOp::Integer {
                ty: MIRIntType::I32,
                signed: true,
                op: MIRIntBinaryOp::Add,
            },
            lhs: MIRValue::Constant(integer(7)),
            rhs: MIRValue::Constant(integer(5)),
        });
    definition
        .block_mut(entry)
        .unwrap()
        .push(MIRInstrKind::Return {
            value: Some(MIRValue::Register(result)),
        });
    let mir = unit(
        types,
        [(
            MIRFunctionID::new(0),
            function(MIRFunctionID::new(0), integer_type, definition),
        )],
        [],
        vec![],
    );

    let value = MIRComptimeEngine::new(&mir)
        .evaluate(MIRFunctionID::new(0), &[])
        .unwrap();
    assert_eq!(value, integer(12));
}

#[test]
fn evaluates_left_shift_instruction() {
    let (types, _, integer_type, _) = types();
    let mut definition = MIRFunctionDefinition::new();
    let entry = definition.add_block();
    let result = definition.add_register(integer_type, None);
    definition
        .block_mut(entry)
        .unwrap()
        .push(MIRInstrKind::BinOp {
            out: result,
            op: MIRBinaryOp::Integer {
                ty: MIRIntType::I32,
                signed: false,
                op: MIRIntBinaryOp::ShiftLeft,
            },
            lhs: MIRValue::Constant(integer(1)),
            rhs: MIRValue::Constant(integer(8)),
        });
    definition
        .block_mut(entry)
        .unwrap()
        .push(MIRInstrKind::Return {
            value: Some(MIRValue::Register(result)),
        });
    let mir = unit(
        types,
        [(
            MIRFunctionID::new(0),
            function(MIRFunctionID::new(0), integer_type, definition),
        )],
        [],
        vec![],
    );

    let value = MIRComptimeEngine::new(&mir)
        .evaluate(MIRFunctionID::new(0), &[])
        .unwrap();
    assert_eq!(
        value,
        MIRConstant::Integer {
            value: 256,
            ty: MIRIntType::I32,
            signed: false,
        }
    );
}

#[test]
fn evaluates_aggregate_instruction() {
    let (mut types, _, integer_type, _) = types();
    let array_type = types.intern(MIRType::new(
        MIRTypeKind::Array {
            length: 2,
            inner: integer_type,
        },
        Some(MIRTypeLayout {
            size: 8,
            alignment: 4,
        }),
    ));
    let mut definition = MIRFunctionDefinition::new();
    let entry = definition.add_block();
    let result = definition.add_register(array_type, None);
    definition
        .block_mut(entry)
        .unwrap()
        .push(MIRInstrKind::AggregateOp(cx_mir::MIRAggregateOp::Value {
            out: result,
            op: MIRValueAggregateOp::Construct {
                ty: array_type,
                fields: vec![
                    (0, MIRValue::Constant(integer(2))),
                    (1, MIRValue::Constant(integer(3))),
                ],
            },
        }));
    definition
        .block_mut(entry)
        .unwrap()
        .push(MIRInstrKind::Return {
            value: Some(MIRValue::Register(result)),
        });
    let mir = unit(
        types,
        [(
            MIRFunctionID::new(0),
            function(MIRFunctionID::new(0), array_type, definition),
        )],
        [],
        vec![],
    );

    assert_eq!(
        MIRComptimeEngine::new(&mir)
            .evaluate(MIRFunctionID::new(0), &[])
            .unwrap(),
        MIRConstant::Aggregate {
            ty: array_type,
            fields: vec![(0, integer(2)), (1, integer(3))],
        }
    );
}

#[test]
fn evaluates_branch_block_parameter() {
    let (types, _, integer_type, _) = types();
    let mut definition = MIRFunctionDefinition::new();
    let entry = definition.add_block();
    let then_block = definition.add_block();
    let else_block = definition.add_block();
    let result = definition
        .add_block_param(then_block, integer_type, None)
        .unwrap();
    definition
        .block_mut(entry)
        .unwrap()
        .push(MIRInstrKind::Branch {
            cond: MIRValue::Constant(MIRConstant::Bool(true)),
            true_target: MIRBlockTarget::with_args(
                then_block,
                vec![MIRValue::Constant(integer(9))],
            ),
            false_target: MIRBlockTarget::new(else_block),
        });
    definition
        .block_mut(then_block)
        .unwrap()
        .push(MIRInstrKind::Return {
            value: Some(MIRValue::Register(result)),
        });
    definition
        .block_mut(else_block)
        .unwrap()
        .push(MIRInstrKind::Return {
            value: Some(MIRValue::Constant(integer(0))),
        });
    let mir = unit(
        types,
        [(
            MIRFunctionID::new(0),
            function(MIRFunctionID::new(0), integer_type, definition),
        )],
        [],
        vec![],
    );

    assert_eq!(
        MIRComptimeEngine::new(&mir)
            .evaluate(MIRFunctionID::new(0), &[])
            .unwrap(),
        integer(9)
    );
}

#[test]
fn materializes_global_relocation_in_order() {
    let (mut types, _, integer_type, _) = types();
    let pointer_type = types.intern(MIRType::new(
        MIRTypeKind::PointerTo {
            inner: integer_type,
        },
        Some(MIRTypeLayout {
            size: ArchitectureConfig::native().pointer_size(),
            alignment: ArchitectureConfig::native().pointer_alignment(),
        }),
    ));
    let target = MIRGlobalID::new(0);
    let alias = MIRGlobalID::new(1);
    let mut definition = MIRFunctionDefinition::new();
    let entry = definition.add_block();
    definition
        .block_mut(entry)
        .unwrap()
        .push(MIRInstrKind::Return {
            value: Some(MIRValue::Place(cx_mir::MIRPlace::Global(target))),
        });
    let init = MIRFunctionID::new(0);
    let globals = [
        (
            target,
            MIRGlobalVariable::variable(
                target,
                CXIdent::new("target"),
                integer_type,
                LinkageMode::Static,
                false,
            ),
        ),
        (
            alias,
            MIRGlobalVariable::new(
                alias,
                CXIdent::new("alias"),
                LinkageMode::Static,
                MIRGlobalKind::Variable {
                    ty: pointer_type,
                    state: MIRGlobalState::Initializer(init),
                    is_nodrop: false,
                    is_mutable: false,
                },
            ),
        ),
    ];
    let mir = unit(
        types,
        [(init, function(init, pointer_type, definition))],
        globals,
        vec![target, alias],
    );
    let mut mir = mir;
    materialize_globals(&mut mir).unwrap();

    assert!(matches!(
        &mir.global(alias).unwrap().kind,
        MIRGlobalKind::Variable {
            state: MIRGlobalState::Initialized(MIRConstant::Global { global, .. }),
            ..
        } if *global == target
    ));
}

#[test]
fn rejects_runtime_function_evaluation() {
    let (types, _, integer_type, _) = types();
    let mut definition = MIRFunctionDefinition::new();
    let entry = definition.add_block();
    definition
        .block_mut(entry)
        .unwrap()
        .push(MIRInstrKind::Return {
            value: Some(MIRValue::Constant(integer(1))),
        });
    let mut runtime = function(MIRFunctionID::new(0), integer_type, definition);
    let (id, prototype, definition, _) = runtime.into_definition_with_mode();
    runtime = MIRFunction::defined(id, prototype, definition, MIRFunctionMode::Runtime);
    let mir = unit(types, [(id, runtime)], [], vec![]);

    assert!(MIRComptimeEngine::new(&mir).evaluate(id, &[]).is_err());
}
