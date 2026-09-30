use crate::{intrinsic::IntrinsicArg, mapper::IntrinsicMapper};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InternalIntrinsic<V, T> {
    Bitcast {
        value: V,
        target: T,
    },
    Assert {
        condition: V,
        message: Option<String>,
    },
    Assume {
        condition: V,
    },
}

impl<V, T> InternalIntrinsic<V, T> {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Bitcast { .. } => "internal.bitcast",

            Self::Assert { .. } => "internal.assert",
            Self::Assume { .. } => "internal.assume",
        }
    }

    pub fn has_result(&self) -> bool {
        matches!(self, Self::Bitcast { .. })
    }

    pub fn args(&self) -> Vec<IntrinsicArg<'_, V, T>> {
        match self {
            Self::Bitcast { value, target } => {
                vec![IntrinsicArg::Value(value), IntrinsicArg::Type(target)]
            }
            Self::Assert { condition, message } => {
                let mut args = vec![IntrinsicArg::Value(condition)];
                if let Some(message) = message {
                    args.push(IntrinsicArg::String(message));
                }
                args
            }
            Self::Assume { condition } => vec![IntrinsicArg::Value(condition)],
        }
    }

    pub(crate) fn try_map<M: IntrinsicMapper<V, T>>(
        &self,
        mapper: &mut M,
    ) -> Result<InternalIntrinsic<M::Value, M::Type>, M::Error> {
        Ok(match self {
            Self::Bitcast { value, target } => InternalIntrinsic::Bitcast {
                value: mapper.value(value)?,
                target: mapper.ty(target)?,
            },
            Self::Assert { condition, message } => InternalIntrinsic::Assert {
                condition: mapper.value(condition)?,
                message: message.clone(),
            },
            Self::Assume { condition } => InternalIntrinsic::Assume {
                condition: mapper.value(condition)?,
            },
        })
    }
}
