use cx_hmir::value::HMIRValue;
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::env::function::LocalLoweringContext;

// Emits a coercion step once its rule has decided that it applies
pub(crate) trait CoercionFn:
    FnOnce(&mut LocalLoweringContext, HMIRValue, &TokenRange) -> CXResult<HMIRValue>
{
}

impl<F> CoercionFn for F where
    F: FnOnce(&mut LocalLoweringContext, HMIRValue, &TokenRange) -> CXResult<HMIRValue>
{
}

// A coercion step that applies, but has not yet been emitted. Holding one without invoking it emits nothing,
// so it also answers whether a value converts.
pub(crate) struct Pending {
    apply: Box<dyn CoercionFn>,
}

impl Pending {
    pub fn new(apply: impl CoercionFn + 'static) -> Self {
        Self {
            apply: Box::new(apply),
        }
    }

    pub fn invoke(
        self,
        context: &mut LocalLoweringContext,
        value: HMIRValue,
        span: &TokenRange,
    ) -> CXResult<HMIRValue> {
        (self.apply)(context, value, span)
    }
}

// Why a rule that would otherwise apply does not
#[derive(Debug, Clone, Copy)]
pub(crate) enum Obstacle {
    Uncopyable,
}

// A rule only inspects types and values to decide, so a rule that does not apply leaves nothing behind
pub(crate) enum Coercion {
    Pending(Pending),
    Unapplied(Option<Obstacle>),
}

impl Coercion {
    pub fn apply(apply: impl CoercionFn + 'static) -> Self {
        Coercion::Pending(Pending::new(apply))
    }

    pub fn unapplied() -> Self {
        Coercion::Unapplied(None)
    }

    pub fn obstructed(obstacle: Obstacle) -> Self {
        Coercion::Unapplied(Some(obstacle))
    }

    // Tries the next rule when this one does not apply, keeping the first obstacle met
    pub fn or_else(self, rule: impl FnOnce() -> Coercion) -> Coercion {
        match self {
            Coercion::Unapplied(cause) => match rule() {
                Coercion::Unapplied(next) => Coercion::Unapplied(cause.or(next)),
                pending => pending,
            },
            pending => pending,
        }
    }
}
