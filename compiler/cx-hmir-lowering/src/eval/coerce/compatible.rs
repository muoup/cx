use cx_hmir::{
    HMIRFnSignature,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};

use crate::env::HMIREnvironment;

// Whether a pointer to 'from' converts implicitly to a pointer to 'to': either side is 'void', or the two are
// compatible. Qualifiers describe the place pointed to, which 'same_type' already looks through.
pub(crate) fn pointees(env: &HMIREnvironment, from: HMIRTypeID, to: HMIRTypeID) -> bool {
    let void = |ty| matches!(env.resolve_kind(ty), HMIRTypeKind::Void);

    void(from) || void(to) || compatible(env, from, to) || discards_result(env, from, to)
}

// CX: a function also converts to one that discards its result, unless the result is @nodrop
fn discards_result(env: &HMIREnvironment, from: HMIRTypeID, to: HMIRTypeID) -> bool {
    let (HMIRTypeKind::Function(from), HMIRTypeKind::Function(to)) =
        (env.resolve_kind(from), env.resolve_kind(to))
    else {
        return false;
    };
    let result = *from.return_type();

    matches!(env.resolve_kind(*to.return_type()), HMIRTypeKind::Void)
        && !env
            .resolve_type_id(result)
            .semantics()
            .move_semantics
            .is_nodrop()
        && same_parameters(env, from, to)
}

// C11 6.2.7: the same type, up to arrays of unknown length and function types realized more than once
pub(crate) fn compatible(env: &HMIREnvironment, a: HMIRTypeID, b: HMIRTypeID) -> bool {
    if env.same_type(a, b) {
        return true;
    }

    match (env.resolve_kind(a), env.resolve_kind(b)) {
        (HMIRTypeKind::PointerTo(a), HMIRTypeKind::PointerTo(b)) => compatible(env, *a, *b),
        (
            HMIRTypeKind::Array {
                element: a,
                length: a_length,
            },
            HMIRTypeKind::Array {
                element: b,
                length: b_length,
            },
        ) => {
            let lengths = a_length.is_none() || b_length.is_none() || a_length == b_length;
            lengths && compatible(env, *a, *b)
        }
        (HMIRTypeKind::Function(a), HMIRTypeKind::Function(b)) => {
            same_parameters(env, a, b) && compatible(env, *a.return_type(), *b.return_type())
        }
        _ => false,
    }
}

fn same_parameters(
    env: &HMIREnvironment,
    a: &HMIRFnSignature<HMIRTypeID>,
    b: &HMIRFnSignature<HMIRTypeID>,
) -> bool {
    a.is_variadic() == b.is_variadic()
        && a.params().len() == b.params().len()
        && a.params()
            .iter()
            .zip(b.params())
            .all(|(a, b)| compatible(env, *a.ty(), *b.ty()))
}
