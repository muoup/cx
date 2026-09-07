use cx_log::CXResult;
use cx_mir::{MIRBlockTarget, MIRInstrKind, MIRStagedExitKind, MIRStagedTargets, MIRTypeKind, MIRValue, ty::interface::MTRegistry};

use crate::{builder::MIRBuilder, lowering::control_flow::auto_cleanup};

pub(crate) fn targets(builder: &mut MIRBuilder<'_>) -> CXResult<MIRStagedTargets> {
    let current = builder.fun().current_block();
    let root = builder.fun().scope_stack().first().unwrap().id();
    let mut targets = MIRStagedTargets::default();
    let result = (|| {
        let block = builder.fun_mut().new_block("staged.return_cleanup");
        targets.return_target = Some(block);
        let ty = builder.outer_return_type;
        let value = ty.filter(|ty| !matches!(builder.types().kind(*ty), Ok(MIRTypeKind::Void)))
            .map(|ty| MIRValue::Register(builder.fun_mut().block_param(block, ty, None)));
        builder.fun_mut().set_current_block(block);
        auto_cleanup(builder, root)?;
        builder.emit(MIRInstrKind::Return { value });

        for kind in [MIRStagedExitKind::Break, MIRStagedExitKind::Continue] {
            let destination = builder.fun().exit_target(kind);
            let block = builder.fun_mut().new_block("staged.loop_cleanup");
            match kind {
                MIRStagedExitKind::Break => targets.break_target = Some(block),
                MIRStagedExitKind::Continue => targets.continue_target = Some(block),
            }
            builder.fun_mut().set_current_block(block);
            auto_cleanup(builder, destination.map_or(root, |(scope, _)| scope))?;
            builder.emit(match destination {
                Some((_, block)) => MIRInstrKind::Jump { target: MIRBlockTarget::new(block) },
                None => MIRInstrKind::StagedExit { kind },
            });
        }

        let destination = builder.fun().scope_stack().iter().rev()
            .find_map(|scope| scope.yield_target.map(|block| (scope.id(), block)));
        let ty = destination.and_then(|(_, block)| builder.fun().body().block(block))
            .and_then(|block| block.params.first())
            .and_then(|register| builder.fun().register_type(*register))
            .or(builder.outer_yield_type)
            .filter(|ty| !matches!(builder.types().kind(*ty), Ok(MIRTypeKind::Void)));
        let block = builder.fun_mut().new_block("staged.yield_cleanup");
        targets.yield_target = Some(block);
        let value = ty.map(|ty| MIRValue::Register(builder.fun_mut().block_param(block, ty, None)));
        builder.fun_mut().set_current_block(block);
        auto_cleanup(builder, destination.map_or(root, |(scope, _)| scope))?;
        builder.emit(match destination {
            Some((_, block)) => MIRInstrKind::Jump { target: MIRBlockTarget::with_args(block, value.into_iter().collect()) },
            None => MIRInstrKind::StagedYield { value, ty },
        });
        Ok(targets)
    })();
    builder.fun_mut().set_current_block(current);
    result
}
