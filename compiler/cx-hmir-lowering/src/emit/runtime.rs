use cx_mir::{MIRInstructionKind, MIRPlaceID, expr::instruction::MIRInvalidationKind};
use cx_tokens::TokenRange;

use crate::env::function::{LocalLoweringContext, runtime::RuntimeScope};

pub fn open_scope(context: &mut LocalLoweringContext, span: &TokenRange) -> usize {
    let runtime = context.runtime_mut();
    let scopes = runtime.scopes_mut();
    let parent = scopes.top();

    scopes.push(RuntimeScope::new(false), Some(parent), Bindings::default())
}

pub fn close_scope(context: &mut LocalLoweringContext, span: &TokenRange) {
    let index = context.runtime().scopes().top();
    drop_scope(context, index, span);
    context.runtime_mut().scopes_mut().pop();
}

pub fn drop_scope(context: &mut LocalLoweringContext, index: usize, span: &TokenRange) {
    let Some(scope) = context.runtime().scopes().control(index).mir() else {
        return;
    };

    let places = context
        .runtime()
        .builder()
        .places()
        .iter()
        .enumerate()
        .filter(|(_, place)| place.scope() == scope)
        .map(|(index, _)| MIRPlaceID::new(index))
        .collect::<Vec<_>>();

    for place in places.into_iter().rev() {
        let invalidate = MIRInstructionKind::Invalidate {
            place,
            kind: MIRInvalidationKind::Drop,
        };
        context.runtime_mut().emit(invalidate, span.clone());
    }
}
