use cx_hmir::{HMIRAggregateKind, HMIRPattern};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    eval::{EvalFrame, eval},
    module::variant_index,
    program::Program,
    staging_error,
    ty::TypeID,
};

pub(crate) fn match_cases<'a>(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    ty: TypeID,
    patterns: impl Iterator<Item = &'a HMIRPattern>,
    span: &TokenRange,
) -> CXResult<Vec<Option<i128>>> {
    let variants = cx
        .types()
        .nominal_of(ty)
        .filter(|nominal| nominal.kind() == HMIRAggregateKind::TaggedUnion)
        .map(|nominal| {
            nominal
                .fields()
                .iter()
                .map(|field| field.name().cloned())
                .collect::<Vec<_>>()
        });
    let mut cases = Vec::new();
    let mut caught = false;
    let covered = |cases: &[Option<i128>]| {
        variants
            .as_ref()
            .is_some_and(|names| (0..names.len()).all(|index| cases.contains(&Some(index as i128))))
    };
    let unreachable = || {
        staging_error(span, &typecheck::UNREACHABLE_MATCH_ARM, ())
    };
    for pattern in patterns {
        if caught || covered(&cases) {
            return Err(unreachable());
        }
        let case = match pattern {
            HMIRPattern::Binding(_) => {
                caught = true;
                cases.push(None);
                continue;
            }
            HMIRPattern::Integer(value) if variants.is_none() => *value as i128,
            HMIRPattern::Value(expected) if variants.is_none() => eval(
                cx,
                frame,
                *expected,
                Some(ty),
            )?
            .as_int()
            .ok_or_else(|| {
                staging_error(span, &typecheck::VALUE_PATTERN_CONSTANT, ())
            })?,
            HMIRPattern::Variant { name, .. } if variants.is_some() => {
                variant_index(cx, ty, name, span)? as i128
            }
            HMIRPattern::Float(_) => {
                return Err(staging_error(span, &typecheck::FLOATING_CASE, ()));
            }
            _ => {
                return Err(staging_error(span, &typecheck::INVALID_PATTERN, ()));
            }
        };
        if cases.contains(&Some(case)) {
            return Err(unreachable());
        }
        cases.push(Some(case));
    }
    if !caught && !covered(&cases) {
        let missing = variants
            .iter()
            .flatten()
            .enumerate()
            .filter(|(index, _)| !cases.contains(&Some(*index as i128)))
            .filter_map(|(_, name)| name.as_ref().map(ToString::to_string))
            .collect::<Vec<_>>();
        let missing = (!missing.is_empty()).then(|| missing.join(", "));
        return Err(staging_error(span, &typecheck::NONEXHAUSTIVE_MATCH, missing));
    }
    Ok(cases)
}
