use std::collections::HashSet;

use crate::{thir::r#type::THIRTypeID, type_context::THIRTypeContext};

#[derive(Default)]
pub struct TypeComparisonState {
    compared_ids: HashSet<TypeIdPair>,
}

impl TypeComparisonState {
    pub(crate) fn compare_type_ids_once(&mut self, left: THIRTypeID, right: THIRTypeID) -> bool {
        self.compared_ids.insert(TypeIdPair::new(left, right))
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct TypeIdPair {
    left: THIRTypeID,
    right: THIRTypeID,
}

impl TypeIdPair {
    fn new(left: THIRTypeID, right: THIRTypeID) -> Self {
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

pub trait TypeContextEqual<Context: THIRTypeContext + ?Sized> {
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
    Context: THIRTypeContext + ?Sized,
{
    left.len() == right.len()
        && left
            .iter()
            .zip(right.iter())
            .all(|(left, right)| left.compare(right, definitions, state))
}
