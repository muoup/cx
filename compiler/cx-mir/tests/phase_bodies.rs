use cx_mir::visit::{MIRVisitRole, MIRVisitor, MIRVisitorMut, walk_value_mut};
use cx_mir::*;
use cx_tokens::TokenRange;
use std::convert::Infallible;

fn source() -> TokenRange {
    TokenRange::Source {
        file: "phase.cx".into(),
        byte_start: 12,
        byte_end: 24,
    }
}

#[test]
fn runtime_conversion_rejects_unresolved_operations_at_their_source() {
    let mut body = MIRStagedBody::new();
    let block = body.add_block();
    body.push_instr_at(
        block,
        MIRStagedInstrKind::Move {
            out: MIRRegister::new(0),
            value: MIRValue::Register(MIRRegister::new(1)),
        },
        source(),
    );
    let error = body.into_runtime().unwrap_err();
    assert!(matches!(error.kind, MIRStagedInstrKind::Move { .. }));
    assert_eq!(error.token_range.source_bounds(), source().source_bounds());
}

#[test]
fn conversion_preserves_scopes_places_and_block_parameters() {
    let mut body = MIRStagedBody::new();
    let block = body.add_block_named("entry");
    let scope = body.add_scope(source());
    let ty = MIRTypeID::new(0);
    let place = body.add_place(ty, None, true, scope);
    let parameter = body.add_block_param(block, ty, None);
    body.push_instr_at(
        block,
        MIRInstrKind::Return {
            value: Some(MIRValue::Register(parameter)),
        }
        .into(),
        source(),
    );
    let body = body.into_runtime().unwrap();
    assert_eq!(body.places()[0].scope, scope);
    assert!(body.places()[0].nodrop);
    assert_eq!(MIRPlace::FunctionLocal(body.places()[0].id), place);
    assert_eq!(body.block(block).unwrap().params, vec![parameter]);
    assert!(matches!(
        body.block(block).unwrap().terminator().unwrap().kind,
        MIRInstrKind::Return { .. }
    ));
    assert_eq!(
        body.block(block).unwrap().instrs[0]
            .token_range
            .source_bounds(),
        source().source_bounds()
    );
}

#[test]
fn template_continuations_are_not_cfg_successors() {
    let instruction = MIRInstr::new(
        MIRStagedInstrKind::Use {
            value: MIRValue::Register(MIRRegister::new(3)),
            targets: MIRStagedTargets {
                return_target: Some(MIRBasicBlockID::new(4)),
                ..Default::default()
            },
        },
        source(),
    );
    assert_eq!(instruction.successors().count(), 0);
    struct Continuations(Vec<MIRBasicBlockID>);
    impl<'ir> MIRVisitor<'ir> for Continuations {
        type Error = Infallible;
        fn continuation(&mut self, block: &MIRBasicBlockID) -> Result<(), Infallible> {
            self.0.push(*block);
            Ok(())
        }
    }
    let mut visitor = Continuations(Vec::new());
    instruction.visit(&mut visitor).unwrap();
    assert_eq!(visitor.0, vec![MIRBasicBlockID::new(4)]);
}

#[test]
fn substitution_distinguishes_inputs_from_definitions_and_rewrites_edges() {
    struct Rewrite;
    impl<'ir> MIRVisitorMut<'ir> for Rewrite {
        type Error = Infallible;
        fn value(&mut self, value: &'ir mut MIRValue) -> Result<(), Infallible> {
            if matches!(value, MIRValue::Register(register) if register.index() == 0) {
                *value = MIRValue::Constant(MIRConstant::Integer {
                    value: 7,
                    ty: MIRIntType::I32,
                    signed: true,
                });
                Ok(())
            } else {
                walk_value_mut(self, value)
            }
        }
        fn register(
            &mut self,
            register: &mut MIRRegister,
            role: MIRVisitRole,
        ) -> Result<(), Infallible> {
            if role == MIRVisitRole::Define {
                *register = MIRRegister::new(register.index() + 10);
            }
            Ok(())
        }
        fn block(&mut self, block: &mut MIRBasicBlockID) -> Result<(), Infallible> {
            *block = MIRBasicBlockID::new(block.index() + 20);
            Ok(())
        }
    }
    let mut assignment = MIRInstr::new(
        MIRInstrKind::Assign {
            target: MIRTarget::Register(MIRRegister::new(0)),
            value: MIRValue::Register(MIRRegister::new(0)),
            ty: MIRTypeID::new(0),
        },
        source(),
    );
    assignment.visit_mut(&mut Rewrite).unwrap();
    assert!(matches!(assignment.kind, MIRInstrKind::Assign {
        target: MIRTarget::Register(register), value: MIRValue::Constant(MIRConstant::Integer { value: 7, .. }), ..
    } if register.index() == 10));
    let mut jump = MIRInstr::new(
        MIRInstrKind::Jump {
            target: MIRBlockTarget::with_args(
                MIRBasicBlockID::new(1),
                vec![MIRValue::Register(MIRRegister::new(0))],
            ),
        },
        source(),
    );
    jump.visit_mut(&mut Rewrite).unwrap();
    assert_eq!(
        jump.successors().collect::<Vec<_>>(),
        vec![MIRBasicBlockID::new(21)]
    );
    assert!(
        matches!(jump.kind, MIRInstrKind::Jump { target } if matches!(target.args[0], MIRValue::Constant(_)))
    );
}

#[test]
fn comptime_and_template_completion_have_distinct_boundaries() {
    let mut body = MIRStagedBody::new();
    let block = body.add_block();
    body.push_instr_at(
        block,
        MIRStagedInstrKind::Exit {
            value: MIRValue::Constant(MIRConstant::Unit),
        },
        source(),
    );
    assert!(body.into_comptime().is_err());
    let mut body = MIRStagedBody::new();
    let block = body.add_block();
    body.push_instr_at(
        block,
        MIRComptimeOp::Call {
            out: None,
            callee: MIRValue::Constant(MIRConstant::Function(MIRFunctionID::new(0))),
            args: vec![],
        }
        .into(),
        source(),
    );
    assert!(body.clone().into_runtime().is_err());
    assert!(matches!(
        body.into_comptime().unwrap().block(block).unwrap().instrs[0].kind,
        MIRComptimeInstrKind::Comptime(MIRComptimeOp::Call { out: None, .. })
    ));
}
