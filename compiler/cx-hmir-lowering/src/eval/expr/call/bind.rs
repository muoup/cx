use cx_hmir::{HMIRBodyContext, HMIRFnSignature, expr::HMIRExprID, ty::HMIRTypeID};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::function::LocalLoweringContext,
    log::hmir_error,
    symbol::structure::{self, Parameter},
    value::{self, HMIRValue},
};

// What a call gives one parameter of its callee
pub(crate) enum Slot {
    // A value an earlier, curried call bound
    Bound(HMIRValue),
    Argument(HMIRExprID),
    // Nothing, for a parameter that has a default
    Default,
    // Nothing; the call is partial
    Open,
}

pub(crate) struct Binding {
    pub slots: Vec<Slot>,
    // The arguments past the parameters of a variadic callee
    pub variadic: Vec<HMIRExprID>,
}

impl Binding {
    pub(crate) fn is_partial(&self) -> bool {
        self.slots.iter().any(|slot| matches!(slot, Slot::Open))
    }
}

// Only a callee that names a function knows the names and the defaults of its parameters
pub(crate) fn parameters(
    context: &LocalLoweringContext,
    callee: &HMIRValue,
    signature: &HMIRFnSignature<HMIRTypeID>,
) -> Vec<Parameter> {
    if let Some(def) = value::as_function(callee) {
        let def = context.env().symbols().resolve_definition(def);
        return structure::parameters(def, context.visible_through());
    }
    let unnamed = |param| Parameter {
        name: None,
        comptime: param,
        defaulted: false,
    };
    signature.params().iter().map(|param| unnamed(param.comptime())).collect()
}

// Binds the parameters of a callee, in this order, from:
//   1. 'applied', the values a curried callee has bound;
//   2. 'named', each to the comptime parameter of its name;
//   3. 'args', in order, to the parameters still open that have no default.
// A call that leaves a parameter without a default open is partial, which only a call whose every argument
// binds a comptime parameter may be. Any other call leaves each parameter still open to its default.
pub(crate) fn bind(
    parameters: &[Parameter],
    variadic: bool,
    name: &str,
    body: &HMIRBodyContext,
    applied: Vec<Option<HMIRValue>>,
    args: &[HMIRExprID],
    named: &[(CXIdent, HMIRExprID)],
    span: &TokenRange,
) -> CXResult<Binding> {
    let mut applied = applied.into_iter();
    let mut slots = parameters
        .iter()
        .map(|_| applied.next().flatten().map_or(Slot::Open, Slot::Bound))
        .collect::<Vec<_>>();

    for (argument, expr) in named {
        let named = |parameter: &Parameter| {
            parameter.comptime && parameter.name.as_ref() == Some(argument)
        };
        let arg_span = body.expr(*expr).span();
        let Some(index) = parameters.iter().position(named) else {
            let unknown = (format!("'{name}'"), argument.to_string());
            return Err(hmir_error(arg_span, &typecheck::UNKNOWN_PARAMETER, unknown));
        };
        if !matches!(slots[index], Slot::Open) {
            let duplicate = (format!("argument '{argument}'"), format!("call to '{name}'"));
            return Err(hmir_error(arg_span, &typecheck::DUPLICATE_ITEM, duplicate));
        }
        slots[index] = Slot::Argument(*expr);
    }

    let positional = (0..slots.len())
        .filter(|index| matches!(slots[*index], Slot::Open) && !parameters[*index].defaulted)
        .collect::<Vec<_>>();
    let count = || {
        let counts = (format!("'{name}'"), positional.len(), args.len(), variadic);
        hmir_error(span, &typecheck::ARGUMENT_COUNT, counts)
    };
    if args.len() > positional.len() && !variadic {
        return Err(count());
    }
    for (index, expr) in positional.iter().zip(args) {
        slots[*index] = Slot::Argument(*expr);
    }

    if args.len() < positional.len() {
        let supplied = !args.is_empty() || !named.is_empty();
        let curries = parameters
            .iter()
            .zip(&slots)
            .all(|(parameter, slot)| parameter.comptime || !matches!(slot, Slot::Argument(_)));
        if !supplied || !curries {
            return Err(count());
        }
    } else {
        for slot in &mut slots {
            if matches!(slot, Slot::Open) {
                *slot = Slot::Default;
            }
        }
    }

    Ok(Binding {
        slots,
        variadic: args.iter().skip(positional.len()).copied().collect(),
    })
}
