use std::collections::HashSet;

use crate::{mir::r#type::MIRTypeId, type_context::MIRTypeContext};

#[derive(Default)]
pub struct TypeComparisonState {
    compared_ids: HashSet<TypeIdPair>,
}

impl TypeComparisonState {
    pub(crate) fn compare_type_ids_once(&mut self, left: MIRTypeId, right: MIRTypeId) -> bool {
        self.compared_ids.insert(TypeIdPair::new(left, right))
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct TypeIdPair {
    left: MIRTypeId,
    right: MIRTypeId,
}

impl TypeIdPair {
    fn new(left: MIRTypeId, right: MIRTypeId) -> Self {
        if left <= right {
            Self { left, right }
        } else {
            Self {
                left: right,
                right: left,
            }
        }
    }
}

pub trait TypeContextEqual<Context: MIRTypeContext + ?Sized> {
    fn compare(&self, other: &Self, definitions: &Context, state: &mut TypeComparisonState)
        -> bool;

    fn contextual_eq(&self, other: &Self, definitions: &Context) -> bool {
        let mut state = TypeComparisonState::default();
        self.compare(other, definitions, &mut state)
    }
}

pub(crate) fn compare_ordered<T, Context>(
    left: &[T],
    right: &[T],
    definitions: &Context,
    state: &mut TypeComparisonState,
) -> bool
where
    T: TypeContextEqual<Context>,
    Context: MIRTypeContext + ?Sized,
{
    left.len() == right.len()
        && left
            .iter()
            .zip(right.iter())
            .all(|(left, right)| left.compare(right, definitions, state))
}
