use std::ops::Range;

use chl_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use chl_log::CHLResult;
use chl_tokens::TokenRange;

use crate::{env::HMIREnvironment, eval::types};

use super::codec::{layout, record_layout};

pub(super) fn inspect(
    env: &mut HMIREnvironment,
    owner: HMIRTypeID,
    start: usize,
    target: HMIRTypeID,
    range: &Range<usize>,
    span: &TokenRange,
) -> CHLResult<(bool, bool)> {
    if start == range.start && env.same_type(owner, target) {
        return Ok((true, false));
    }
    if start >= range.end || start + layout(env, owner, span)?.size() <= range.start {
        return Ok((false, false));
    }
    let mut overlaps = false;
    let mut visit = |env: &mut HMIREnvironment, ty, offset| {
        let (declared, opaque) = inspect(env, ty, offset, target, range, span)?;
        overlaps |= opaque;
        Ok(declared)
    };
    match env.resolve_kind(owner).clone() {
        HMIRTypeKind::Array { element, length } => {
            let stride = layout(env, element, span)?.size();
            if stride != 0 {
                let first = range.start.saturating_sub(start) / stride;
                let end = (range.end - start).div_ceil(stride).min(length as usize);
                for index in first..end {
                    if visit(env, element, start + index * stride)? {
                        return Ok((true, false));
                    }
                }
            }
        }
        HMIRTypeKind::Struct { .. }
        | HMIRTypeKind::Union { .. }
        | HMIRTypeKind::TaggedUnion { .. } => {
            for field in record_layout(env, owner, span)?.0 {
                if !env.is_unbounded_array(field.ty())
                    && visit(env, field.ty(), start + field.offset())?
                {
                    return Ok((true, false));
                }
            }
        }
        _ => overlaps = types::opaque_slot(env, owner),
    }
    Ok((false, overlaps))
}
