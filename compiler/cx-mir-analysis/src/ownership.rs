use std::collections::{BTreeMap, BTreeSet};

use cx_mir::{
    MIRAggregateOp, MIRAssignTarget, MIRBasicBlockID, MIRFunction, MIRInstrKind, MIRPlace,
    MIRPlaceAggregateOp, MIRUnit, MIRValue, MIRValueAggregateOp,
};

use crate::types::MIRAnalysisError;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PlaceState {
    Uninitialized,
    Available,
    Moved,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct OwnershipState {
    places: BTreeMap<MIRPlace, PlaceState>,
    projections: BTreeMap<MIRPlace, MIRPlace>,
}

impl OwnershipState {
    fn new(function: &MIRFunction) -> Self {
        let projections = function
            .definition()
            .into_iter()
            .flat_map(|definition| definition.blocks())
            .flat_map(|block| block.instrs.iter())
            .filter_map(|instruction| match &instruction.kind {
                MIRInstrKind::AggregateOp(MIRAggregateOp::Place { out, op }) => {
                    let base = match op {
                        MIRPlaceAggregateOp::Field { base, .. }
                        | MIRPlaceAggregateOp::Index { base, .. }
                        | MIRPlaceAggregateOp::Variant { base, .. } => base,
                    };
                    Some((*out, *base))
                }
                _ => None,
            })
            .collect();

        Self {
            places: BTreeMap::new(),
            projections,
        }
    }

    fn get(&self, place: &MIRPlace) -> Option<&PlaceState> {
        self.places.get(place)
    }

    fn insert(&mut self, place: MIRPlace, state: PlaceState) {
        self.places.insert(place, state);
    }

    fn remove(&mut self, place: &MIRPlace) {
        self.places.remove(place);
    }

    fn mark_moved(&mut self, place: MIRPlace) {
        self.insert(place, PlaceState::Moved);
        let mut current = place;
        while let Some(base) = self.projections.get(&current).copied() {
            self.insert(base, PlaceState::Moved);
            current = base;
        }
    }
}

/// Checks path-sensitive ownership and `@nodrop` discharge after MIR has
/// established the actual control-flow graph.
pub(crate) fn check(unit: &MIRUnit) -> Result<(), MIRAnalysisError> {
    for function in unit.functions() {
        check_function(unit, function)?;
    }
    Ok(())
}

fn check_function(unit: &MIRUnit, function: &MIRFunction) -> Result<(), MIRAnalysisError> {
    let Some(definition) = function.definition() else {
        return Ok(());
    };
    let entry = definition.entry();
    if entry.index() >= definition.blocks().len() {
        return Ok(());
    }

    let mut entries = vec![None; definition.blocks().len()];
    entries[entry.index()] = Some(initial_state(function));

    loop {
        let mut changed = false;

        for block in definition.blocks() {
            let Some(state) = entries[block.id.index()].clone() else {
                continue;
            };
            let state = transfer_block(unit, function, block, state, false)?;

            let Some(terminator) = block.instrs.last() else {
                continue;
            };
            for target in terminator.successors() {
                if target.index() >= entries.len() {
                    continue;
                }
                let Some(slot) = entries.get_mut(target.index()) else {
                    continue;
                };
                changed |= merge_entry(
                    unit,
                    function,
                    block.id,
                    block.instrs.len().saturating_sub(1),
                    slot,
                    &state,
                )?;
            }
        }

        if !changed {
            break;
        }
    }

    for block in definition.blocks() {
        let Some(state) = entries[block.id.index()].clone() else {
            continue;
        };
        transfer_block(unit, function, block, state, true)?;
    }

    Ok(())
}

fn initial_state(function: &MIRFunction) -> OwnershipState {
    let mut state = OwnershipState::new(function);
    for (index, _) in function.prototype().signature.params.iter().enumerate() {
        state.insert(
            MIRPlace::Parameter(cx_mir::MIRParameterID::new(index)),
            PlaceState::Available,
        );
    }
    state
}

fn merge_entry(
    unit: &MIRUnit,
    function: &MIRFunction,
    block: MIRBasicBlockID,
    instruction: usize,
    slot: &mut Option<OwnershipState>,
    incoming: &OwnershipState,
) -> Result<bool, MIRAnalysisError> {
    let Some(existing) = slot else {
        *slot = Some(incoming.clone());
        return Ok(true);
    };

    let keys = existing
        .places
        .keys()
        .chain(incoming.places.keys())
        .copied()
        .collect::<BTreeSet<_>>();
    let mut merged = OwnershipState {
        places: BTreeMap::new(),
        projections: existing.projections.clone(),
    };
    for place in keys {
        let existing_state = existing
            .get(&place)
            .copied()
            .unwrap_or(PlaceState::Uninitialized);
        let incoming_state = incoming
            .get(&place)
            .copied()
            .unwrap_or(PlaceState::Uninitialized);

        if is_nodrop(function, place)
            && matches!(existing_state, PlaceState::Available)
                != matches!(incoming_state, PlaceState::Available)
        {
            return Err(ownership_error(
                function,
                block,
                instruction,
                None,
                place,
                format!(
                    "@nodrop place '{}' is moved on only some control-flow paths",
                    place_name(unit, function, place)
                ),
            ));
        }

        let state = merge_state(existing_state, incoming_state);
        if state != PlaceState::Uninitialized {
            merged.insert(place, state);
        }
    }

    if *existing != merged {
        *existing = merged;
        Ok(true)
    } else {
        Ok(false)
    }
}

fn merge_state(left: PlaceState, right: PlaceState) -> PlaceState {
    if left == right {
        return left;
    }
    PlaceState::Moved
}

fn is_nodrop(function: &MIRFunction, place: MIRPlace) -> bool {
    match place {
        MIRPlace::FunctionLocal(id) => function
            .definition()
            .and_then(|definition| definition.place(id))
            .is_some_and(|declaration| declaration.nodrop),
        MIRPlace::Parameter(id) => function
            .prototype()
            .signature
            .params
            .get(id.index())
            .is_some_and(|parameter| parameter.nodrop),
        MIRPlace::Global(_) => false,
    }
}

fn transfer_block(
    unit: &MIRUnit,
    function: &MIRFunction,
    block: &cx_mir::MIRBasicBlock,
    mut state: OwnershipState,
    diagnose: bool,
) -> Result<OwnershipState, MIRAnalysisError> {
    for (instruction_index, instruction) in block.instrs.iter().enumerate() {
        transfer_instruction(
            unit,
            function,
            block.id,
            instruction_index,
            &instruction.kind,
            &mut state,
            diagnose,
        )?;
    }
    Ok(state)
}

fn transfer_instruction(
    unit: &MIRUnit,
    function: &MIRFunction,
    block: cx_mir::MIRBasicBlockID,
    instruction: usize,
    kind: &MIRInstrKind,
    state: &mut OwnershipState,
    diagnose: bool,
) -> Result<(), MIRAnalysisError> {
    let definition = function
        .definition()
        .expect("ownership analysis reached a MIR declaration");
    match kind {
        MIRInstrKind::ScopeEnter { .. } => {}
        MIRInstrKind::ScopeExit { scope } => {
            for declaration in definition.places() {
                if declaration.scope != *scope {
                    continue;
                }

                let place = MIRPlace::FunctionLocal(declaration.id);
                if declaration.nodrop
                    && matches!(state.get(&place), Some(PlaceState::Available))
                    && diagnose
                {
                    return Err(ownership_error(
                        function,
                        block,
                        instruction,
                        Some(*scope),
                        place,
                        format!(
                            "@nodrop place '{}' is not moved or leaked before scope exit",
                            place_name(unit, function, place)
                        ),
                    ));
                }
                state.remove(&place);
            }
        }
        MIRInstrKind::Initialize { place } | MIRInstrKind::Create { out: place, .. } => {
            set_available(state, *place);
        }
        MIRInstrKind::Leak { place } => {
            consume(unit, function, block, instruction, *place, state, diagnose)?;
        }
        MIRInstrKind::Assign { target, value, .. } => {
            use_value(unit, function, block, instruction, value, state, diagnose)?;
            if let MIRAssignTarget::Place(dest) = target {
                set_available(state, *dest);
            }
        }
        MIRInstrKind::AddressOf { place, .. } => {
            use_place(unit, function, block, instruction, *place, state, diagnose)?;
        }
        MIRInstrKind::Dereference { out, pointer, .. } => {
            use_value(unit, function, block, instruction, pointer, state, diagnose)?;
            set_available(state, *out);
        }
        MIRInstrKind::AggregateOp(operation) => match operation {
            MIRAggregateOp::Place { out, op } => {
                match op {
                    MIRPlaceAggregateOp::Field { base, .. }
                    | MIRPlaceAggregateOp::Variant { base, .. } => {
                        use_place(unit, function, block, instruction, *base, state, diagnose)?
                    }
                    MIRPlaceAggregateOp::Index { base, index, .. } => {
                        use_place(unit, function, block, instruction, *base, state, diagnose)?;
                        use_value(unit, function, block, instruction, index, state, diagnose)?;
                    }
                }
                set_available(state, *out);
            }
            MIRAggregateOp::Value { out: _, op } => match op {
                MIRValueAggregateOp::Discriminant { value, .. }
                | MIRValueAggregateOp::Variant { value, .. }
                | MIRValueAggregateOp::ProjectVariant { value, .. } => {
                    use_value(unit, function, block, instruction, value, state, diagnose)?
                }
                MIRValueAggregateOp::Construct { fields, .. } => {
                    for (_, value) in fields {
                        use_value(unit, function, block, instruction, value, state, diagnose)?;
                    }
                }
            },
        },
        MIRInstrKind::Call { callee, args, .. } => {
            use_value(unit, function, block, instruction, callee, state, diagnose)?;
            for arg in args {
                use_value(unit, function, block, instruction, arg, state, diagnose)?;
            }
        }
        MIRInstrKind::VaStart { list, last } => {
            use_value(unit, function, block, instruction, list, state, diagnose)?;
            use_value(unit, function, block, instruction, last, state, diagnose)?;
        }
        MIRInstrKind::VaEnd { list } => {
            use_value(unit, function, block, instruction, list, state, diagnose)?;
        }
        MIRInstrKind::VaArg { list, .. } => {
            use_value(unit, function, block, instruction, list, state, diagnose)?;
        }
        MIRInstrKind::BinOp { lhs, rhs, .. } => {
            use_value(unit, function, block, instruction, lhs, state, diagnose)?;
            use_value(unit, function, block, instruction, rhs, state, diagnose)?;
        }
        MIRInstrKind::UnOp { operand, .. }
        | MIRInstrKind::Coerce { operand, .. }
        | MIRInstrKind::Assert {
            condition: operand, ..
        }
        | MIRInstrKind::Assume { condition: operand } => {
            use_value(unit, function, block, instruction, operand, state, diagnose)?;
        }
        MIRInstrKind::Return { value } => {
            if let Some(value) = value {
                use_value(unit, function, block, instruction, value, state, diagnose)?;
            }
            check_function_exit(unit, function, block, instruction, state, diagnose)?;
        }
        MIRInstrKind::Jump { target } => {
            for value in &target.args {
                use_value(unit, function, block, instruction, value, state, diagnose)?;
            }
        }
        MIRInstrKind::Branch {
            cond,
            true_target,
            false_target,
        } => {
            use_value(unit, function, block, instruction, cond, state, diagnose)?;
            for value in true_target.args.iter().chain(&false_target.args) {
                use_value(unit, function, block, instruction, value, state, diagnose)?;
            }
        }
        MIRInstrKind::IntSwitch {
            value,
            cases,
            default,
        } => {
            use_value(unit, function, block, instruction, value, state, diagnose)?;
            for (_, target) in cases {
                for value in &target.args {
                    use_value(unit, function, block, instruction, value, state, diagnose)?;
                }
            }
            if let Some(target) = default {
                for value in &target.args {
                    use_value(unit, function, block, instruction, value, state, diagnose)?;
                }
            }
        }
        MIRInstrKind::VariantSwitch {
            subject,
            cases,
            default,
            ..
        } => {
            if let MIRValue::Move(place) = subject {
                consume(unit, function, block, instruction, *place, state, diagnose)?;
            } else {
                use_value(unit, function, block, instruction, subject, state, diagnose)?;
            }
            for (_, target) in cases {
                for value in &target.args {
                    use_value(unit, function, block, instruction, value, state, diagnose)?;
                }
            }
            if let Some(target) = default {
                for value in &target.args {
                    use_value(unit, function, block, instruction, value, state, diagnose)?;
                }
            }
        }
        MIRInstrKind::MakeStaged { captures, .. } => {
            for value in captures {
                use_value(unit, function, block, instruction, value, state, diagnose)?;
            }
        }
        MIRInstrKind::ApplyStaged { staged, args, .. } => {
            use_value(unit, function, block, instruction, staged, state, diagnose)?;
            for value in args {
                use_value(unit, function, block, instruction, value, state, diagnose)?;
            }
        }
        MIRInstrKind::StagedReturn { value } => {
            use_value(unit, function, block, instruction, value, state, diagnose)?;
        }
        MIRInstrKind::StagedMove { value, .. } => {
            use_value(unit, function, block, instruction, value, state, diagnose)?;
        }
        MIRInstrKind::StagedUse { value } => {
            use_value(unit, function, block, instruction, value, state, diagnose)?;
        }
        MIRInstrKind::Unreachable => {}
    }
    Ok(())
}

fn use_value(
    unit: &MIRUnit,
    function: &MIRFunction,
    block: cx_mir::MIRBasicBlockID,
    instruction: usize,
    value: &MIRValue,
    state: &mut OwnershipState,
    diagnose: bool,
) -> Result<(), MIRAnalysisError> {
    match value {
        MIRValue::PlaceRef(place) | MIRValue::Copy(place) => {
            use_place(unit, function, block, instruction, *place, state, diagnose)
        }
        MIRValue::Move(place) => {
            consume(unit, function, block, instruction, *place, state, diagnose)
        }
        MIRValue::Register(_) | MIRValue::Constant(_) => Ok(()),
    }
}

fn use_place(
    unit: &MIRUnit,
    function: &MIRFunction,
    block: cx_mir::MIRBasicBlockID,
    instruction: usize,
    place: MIRPlace,
    state: &mut OwnershipState,
    diagnose: bool,
) -> Result<(), MIRAnalysisError> {
    if matches!(place, MIRPlace::Global(_)) {
        return Ok(());
    }

    match state
        .get(&place)
        .copied()
        .unwrap_or(PlaceState::Uninitialized)
    {
        PlaceState::Available => Ok(()),
        PlaceState::Moved => ownership_failure(
            unit,
            function,
            block,
            instruction,
            place,
            "used after it was moved",
            diagnose,
        ),
        PlaceState::Uninitialized => ownership_failure(
            unit,
            function,
            block,
            instruction,
            place,
            "used before it was initialized",
            diagnose,
        ),
    }
}

fn consume(
    unit: &MIRUnit,
    function: &MIRFunction,
    block: cx_mir::MIRBasicBlockID,
    instruction: usize,
    place: MIRPlace,
    state: &mut OwnershipState,
    diagnose: bool,
) -> Result<(), MIRAnalysisError> {
    if matches!(place, MIRPlace::Global(_)) {
        return Ok(());
    }

    let current = state
        .get(&place)
        .copied()
        .unwrap_or(PlaceState::Uninitialized);
    let result = match current {
        PlaceState::Available => Ok(()),
        PlaceState::Moved => ownership_failure(
            unit,
            function,
            block,
            instruction,
            place,
            "moved more than once",
            diagnose,
        ),
        PlaceState::Uninitialized => ownership_failure(
            unit,
            function,
            block,
            instruction,
            place,
            "moved before it was initialized",
            diagnose,
        ),
    };

    state.mark_moved(place);
    result
}

fn set_available(state: &mut OwnershipState, place: MIRPlace) {
    if !matches!(place, MIRPlace::Global(_)) {
        state.insert(place, PlaceState::Available);
    }
}

fn check_function_exit(
    unit: &MIRUnit,
    function: &MIRFunction,
    block: cx_mir::MIRBasicBlockID,
    instruction: usize,
    state: &OwnershipState,
    diagnose: bool,
) -> Result<(), MIRAnalysisError> {
    if !diagnose {
        return Ok(());
    }

    let definition = function
        .definition()
        .expect("ownership analysis reached a MIR declaration");
    let root_scope = definition.scopes().first().map(|scope| scope.id);

    for declaration in definition.places() {
        if !declaration.nodrop {
            continue;
        }
        let place = MIRPlace::FunctionLocal(declaration.id);
        if let Some(PlaceState::Available) = state.get(&place).copied() {
            return Err(ownership_error(
                function,
                block,
                instruction,
                Some(declaration.scope),
                place,
                format!(
                    "@nodrop place '{}' is not moved or leaked before function exit",
                    place_name(unit, function, place)
                ),
            ));
        }
    }

    for (index, parameter) in function.prototype().signature.params.iter().enumerate() {
        if !parameter.nodrop {
            continue;
        }

        let place = MIRPlace::Parameter(cx_mir::MIRParameterID::new(index));
        if matches!(state.get(&place), Some(PlaceState::Available)) {
            return Err(ownership_error(
                function,
                block,
                instruction,
                root_scope,
                place,
                format!(
                    "@nodrop parameter '{}' is not moved or leaked before function exit",
                    place_name(unit, function, place)
                ),
            ));
        }
    }

    Ok(())
}

fn ownership_failure(
    unit: &MIRUnit,
    function: &MIRFunction,
    block: cx_mir::MIRBasicBlockID,
    instruction: usize,
    place: MIRPlace,
    reason: &'static str,
    diagnose: bool,
) -> Result<(), MIRAnalysisError> {
    if diagnose {
        Err(ownership_error(
            function,
            block,
            instruction,
            None,
            place,
            format!("place '{}' {reason}", place_name(unit, function, place)),
        ))
    } else {
        Ok(())
    }
}

fn ownership_error(
    function: &MIRFunction,
    block: cx_mir::MIRBasicBlockID,
    instruction: usize,
    scope: Option<cx_mir::MIRScopeID>,
    place: MIRPlace,
    message: String,
) -> MIRAnalysisError {
    MIRAnalysisError::OwnershipViolation {
        function: function.id(),
        block,
        instruction,
        scope,
        place,
        function_name: function.prototype().signature.display_name().to_string(),
        message,
    }
}

fn place_name(unit: &MIRUnit, function: &MIRFunction, place: MIRPlace) -> String {
    match place {
        MIRPlace::FunctionLocal(id) => function
            .definition()
            .and_then(|definition| definition.place(id))
            .and_then(|declaration| declaration.debug_name.as_ref())
            .map(ToString::to_string)
            .unwrap_or_else(|| "temporary".to_string()),
        MIRPlace::Parameter(id) => function
            .prototype()
            .signature
            .params
            .get(id.index())
            .and_then(|parameter| parameter.name.as_ref())
            .map(ToString::to_string)
            .unwrap_or_else(|| "parameter".to_string()),
        MIRPlace::Global(id) => unit
            .global(id)
            .map(|global| global.name.to_string())
            .unwrap_or_else(|| "global".to_string()),
    }
}
