use cx_mir::{MIRBlockTarget, MIRConstant, MIRInstrKind, MIRValue};

pub(crate) fn branch<'a>(
    condition: &MIRConstant,
    true_target: &'a MIRBlockTarget,
    false_target: &'a MIRBlockTarget,
) -> Result<&'a MIRBlockTarget, String> {
    let condition = match condition {
        MIRConstant::Bool(value) => *value,
        MIRConstant::Integer { value, .. } => *value != 0,
        MIRConstant::Null { .. } => false,
        MIRConstant::Global { .. }
        | MIRConstant::GlobalOffset { .. }
        | MIRConstant::Function(_) => true,
        _ => return Err(format!("value {condition:?} is not a branch condition")),
    };
    Ok(if condition { true_target } else { false_target })
}

pub(crate) fn switch<'a>(
    instruction: &'a MIRInstrKind,
    value: &MIRConstant,
) -> Result<Option<&'a MIRBlockTarget>, String> {
    let MIRInstrKind::IntSwitch { cases, default, .. } = instruction else {
        return Err("invalid instruction passed to integer switch helper".to_owned());
    };
    if let Some((_, target)) = cases.iter().find(|(case, _)| case == value) {
        return Ok(Some(target));
    }
    Ok(default.as_ref())
}

pub(crate) fn target_arguments(target: &MIRBlockTarget) -> impl Iterator<Item = &MIRValue> {
    target.args.iter()
}
