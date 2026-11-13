use crate::{intrinsic::IntrinsicArg, mapper::IntrinsicMapper, source::IntrinsicSource};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PointerOffsetOp {
    Add,
    Sub,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PointerCompareOp {
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PointerIntrinsic<V, T> {
    ToInt {
        ptr: V,
        target: T,
    },
    Offset {
        op: PointerOffsetOp,
        ptr: V,
        offset: V,
    },
    Diff {
        lhs: V,
        rhs: V,
        element: T,
    },
    Compare {
        op: PointerCompareOp,
        lhs: V,
        rhs: V,
    },
}

impl PointerOffsetOp {
    pub const ALL: [Self; 2] = [Self::Add, Self::Sub];

    pub fn path(self) -> &'static str {
        match self {
            Self::Add => "pointer.add",
            Self::Sub => "pointer.sub",
        }
    }
}

impl PointerCompareOp {
    pub const ALL: [Self; 6] = [Self::Eq, Self::Neq, Self::Lt, Self::Leq, Self::Gt, Self::Geq];

    pub fn path(self) -> &'static str {
        match self {
            Self::Eq => "pointer.eq",
            Self::Neq => "pointer.neq",
            Self::Lt => "pointer.lt",
            Self::Leq => "pointer.leq",
            Self::Gt => "pointer.gt",
            Self::Geq => "pointer.geq",
        }
    }
}

impl<V, T> PointerIntrinsic<V, T> {
    pub fn path(&self) -> &'static str {
        match self {
            Self::ToInt { .. } => "pointer.to_int",
            Self::Offset { op, .. } => op.path(),
            Self::Diff { .. } => "pointer.diff",
            Self::Compare { op, .. } => op.path(),
        }
    }

    pub fn args(&self) -> Vec<IntrinsicArg<'_, V, T>> {
        match self {
            Self::ToInt { ptr, target } => {
                vec![IntrinsicArg::Value(ptr), IntrinsicArg::Type(target)]
            }
            Self::Offset { ptr, offset, .. } => {
                vec![IntrinsicArg::Value(ptr), IntrinsicArg::Value(offset)]
            }
            Self::Diff { lhs, rhs, element } => vec![
                IntrinsicArg::Value(lhs),
                IntrinsicArg::Value(rhs),
                IntrinsicArg::Type(element),
            ],
            Self::Compare { lhs, rhs, .. } => {
                vec![IntrinsicArg::Value(lhs), IntrinsicArg::Value(rhs)]
            }
        }
    }

    pub(crate) fn from_path<S: IntrinsicSource<V, T>>(
        path: &str,
        source: &mut S,
    ) -> Result<Option<Self>, S::Error> {
        if let Some(op) = PointerOffsetOp::ALL.into_iter().find(|op| op.path() == path) {
            let (ptr, offset) = (source.value()?, source.value()?);
            return Ok(Some(Self::Offset { op, ptr, offset }));
        }
        if let Some(op) = PointerCompareOp::ALL.into_iter().find(|op| op.path() == path) {
            let (lhs, rhs) = (source.value()?, source.value()?);
            return Ok(Some(Self::Compare { op, lhs, rhs }));
        }
        Ok(Some(match path {
            "pointer.to_int" => Self::ToInt {
                ptr: source.value()?,
                target: source.ty()?,
            },
            "pointer.diff" => Self::Diff {
                lhs: source.value()?,
                rhs: source.value()?,
                element: source.ty()?,
            },
            _ => return Ok(None),
        }))
    }

    pub(crate) fn try_map<M: IntrinsicMapper<V, T>>(
        &self,
        mapper: &mut M,
    ) -> Result<PointerIntrinsic<M::Value, M::Type>, M::Error> {
        Ok(match self {
            Self::ToInt { ptr, target } => PointerIntrinsic::ToInt {
                ptr: mapper.value(ptr)?,
                target: mapper.ty(target)?,
            },
            Self::Offset { op, ptr, offset } => PointerIntrinsic::Offset {
                op: *op,
                ptr: mapper.value(ptr)?,
                offset: mapper.value(offset)?,
            },
            Self::Diff { lhs, rhs, element } => PointerIntrinsic::Diff {
                lhs: mapper.value(lhs)?,
                rhs: mapper.value(rhs)?,
                element: mapper.ty(element)?,
            },
            Self::Compare { op, lhs, rhs } => PointerIntrinsic::Compare {
                op: *op,
                lhs: mapper.value(lhs)?,
                rhs: mapper.value(rhs)?,
            },
        })
    }
}
