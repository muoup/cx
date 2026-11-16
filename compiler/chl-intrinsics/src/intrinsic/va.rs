use crate::{intrinsic::IntrinsicArg, mapper::IntrinsicMapper};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VAIntrinsic<V, T> {
    Start { list: V, last: V },
    End { list: V },
    Arg { list: V, ty: T },
}

impl<V, T> VAIntrinsic<V, T> {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Start { .. } => "va.start",
            Self::End { .. } => "va.end",
            Self::Arg { .. } => "va.arg",
        }
    }

    pub fn has_result(&self) -> bool {
        matches!(self, Self::Arg { .. })
    }

    pub fn args(&self) -> Vec<IntrinsicArg<'_, V, T>> {
        match self {
            Self::Start { list, last } => {
                vec![IntrinsicArg::Value(list), IntrinsicArg::Value(last)]
            }
            Self::End { list } => vec![IntrinsicArg::Value(list)],
            Self::Arg { list, ty } => vec![IntrinsicArg::Value(list), IntrinsicArg::Type(ty)],
        }
    }

    pub(crate) fn try_map<M: IntrinsicMapper<V, T>>(
        &self,
        mapper: &mut M,
    ) -> Result<VAIntrinsic<M::Value, M::Type>, M::Error> {
        Ok(match self {
            Self::Start { list, last } => VAIntrinsic::Start {
                list: mapper.value(list)?,
                last: mapper.value(last)?,
            },
            Self::End { list } => VAIntrinsic::End {
                list: mapper.value(list)?,
            },
            Self::Arg { list, ty } => VAIntrinsic::Arg {
                list: mapper.value(list)?,
                ty: mapper.ty(ty)?,
            },
        })
    }
}
