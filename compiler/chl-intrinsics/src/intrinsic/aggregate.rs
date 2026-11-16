use crate::{intrinsic::IntrinsicArg, mapper::IntrinsicMapper};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AggregateIntrinsic<V, T> {
    SumIndex { value: V, sum: T },
    SumVariant { value: V, variant: usize, sum: T },
    Init { ty: T, fields: Vec<(usize, V)> },
    StructField { base: V, field: usize, aggregate: T },
    ArrayIndex { base: V, index: V, element: T },
}

impl<V, T> AggregateIntrinsic<V, T> {
    pub fn path(&self) -> &'static str {
        match self {
            Self::SumIndex { .. } => "aggregate.sum_index",
            Self::SumVariant { .. } => "aggregate.sum_variant",
            Self::Init { .. } => "aggregate.init",
            Self::StructField { .. } => "aggregate.struct_field",
            Self::ArrayIndex { .. } => "aggregate.array_index",
        }
    }

    pub fn args(&self) -> Vec<IntrinsicArg<'_, V, T>> {
        match self {
            Self::SumIndex { value, sum } => {
                vec![IntrinsicArg::Value(value), IntrinsicArg::Type(sum)]
            }
            Self::SumVariant {
                value,
                variant,
                sum,
            } => vec![
                IntrinsicArg::Value(value),
                IntrinsicArg::Index(*variant),
                IntrinsicArg::Type(sum),
            ],
            Self::Init { ty, fields } => std::iter::once(IntrinsicArg::Type(ty))
                .chain(fields.iter().flat_map(|(field, value)| {
                    [IntrinsicArg::Index(*field), IntrinsicArg::Value(value)]
                }))
                .collect(),
            Self::StructField {
                base,
                field,
                aggregate,
            } => vec![
                IntrinsicArg::Value(base),
                IntrinsicArg::Index(*field),
                IntrinsicArg::Type(aggregate),
            ],
            Self::ArrayIndex {
                base,
                index,
                element,
            } => vec![
                IntrinsicArg::Value(base),
                IntrinsicArg::Value(index),
                IntrinsicArg::Type(element),
            ],
        }
    }

    pub(crate) fn try_map<M: IntrinsicMapper<V, T>>(
        &self,
        mapper: &mut M,
    ) -> Result<AggregateIntrinsic<M::Value, M::Type>, M::Error> {
        Ok(match self {
            Self::SumIndex { value, sum } => AggregateIntrinsic::SumIndex {
                value: mapper.value(value)?,
                sum: mapper.ty(sum)?,
            },
            Self::SumVariant {
                value,
                variant,
                sum,
            } => AggregateIntrinsic::SumVariant {
                value: mapper.value(value)?,
                variant: *variant,
                sum: mapper.ty(sum)?,
            },
            Self::Init { ty, fields } => AggregateIntrinsic::Init {
                ty: mapper.ty(ty)?,
                fields: fields
                    .iter()
                    .map(|(field, value)| Ok((*field, mapper.value(value)?)))
                    .collect::<Result<_, _>>()?,
            },
            Self::StructField {
                base,
                field,
                aggregate,
            } => AggregateIntrinsic::StructField {
                base: mapper.value(base)?,
                field: *field,
                aggregate: mapper.ty(aggregate)?,
            },
            Self::ArrayIndex {
                base,
                index,
                element,
            } => AggregateIntrinsic::ArrayIndex {
                base: mapper.value(base)?,
                index: mapper.value(index)?,
                element: mapper.ty(element)?,
            },
        })
    }
}
