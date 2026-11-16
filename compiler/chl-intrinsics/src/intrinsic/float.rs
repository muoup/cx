use crate::{intrinsic::IntrinsicArg, mapper::IntrinsicMapper, source::IntrinsicSource};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatBinaryOp {
    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Geq,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FloatIntrinsic<V, T> {
    Neg { value: V },
    Binary { op: FloatBinaryOp, lhs: V, rhs: V },
    ToInt { value: V, target: T, signed: bool },
    Cast { value: V, target: T },
}

impl FloatBinaryOp {
    pub const ALL: [Self; 10] = [
        Self::Add,
        Self::Sub,
        Self::Mul,
        Self::Div,
        Self::Eq,
        Self::Neq,
        Self::Lt,
        Self::Le,
        Self::Gt,
        Self::Geq,
    ];

    pub fn path(self) -> &'static str {
        match self {
            Self::Add => "float.add",
            Self::Sub => "float.sub",
            Self::Mul => "float.mul",
            Self::Div => "float.div",
            Self::Eq => "float.eq",
            Self::Neq => "float.neq",
            Self::Lt => "float.lt",
            Self::Le => "float.le",
            Self::Gt => "float.gt",
            Self::Geq => "float.geq",
        }
    }
}

impl<V, T> FloatIntrinsic<V, T> {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Neg { .. } => "float.neg",
            Self::Binary { op, .. } => op.path(),
            Self::ToInt { .. } => "float.to_int",
            Self::Cast { .. } => "float.cast",
        }
    }

    pub fn args(&self) -> Vec<IntrinsicArg<'_, V, T>> {
        match self {
            Self::Neg { value } => vec![IntrinsicArg::Value(value)],
            Self::Binary { lhs, rhs, .. } => {
                vec![IntrinsicArg::Value(lhs), IntrinsicArg::Value(rhs)]
            }
            Self::ToInt {
                value,
                target,
                signed,
            } => vec![
                IntrinsicArg::Value(value),
                IntrinsicArg::Type(target),
                IntrinsicArg::Bool(*signed),
            ],
            Self::Cast { value, target } => {
                vec![IntrinsicArg::Value(value), IntrinsicArg::Type(target)]
            }
        }
    }

    pub(crate) fn from_path<S: IntrinsicSource<V, T>>(
        path: &str,
        source: &mut S,
    ) -> Result<Option<Self>, S::Error> {
        if let Some(op) = FloatBinaryOp::ALL.into_iter().find(|op| op.path() == path) {
            let (lhs, rhs) = (source.value()?, source.value()?);
            return Ok(Some(Self::Binary { op, lhs, rhs }));
        }
        Ok(Some(match path {
            "float.neg" => Self::Neg {
                value: source.value()?,
            },
            "float.to_int" => Self::ToInt {
                value: source.value()?,
                target: source.ty()?,
                signed: source.bool()?,
            },
            "float.cast" => Self::Cast {
                value: source.value()?,
                target: source.ty()?,
            },
            _ => return Ok(None),
        }))
    }

    pub(crate) fn try_map<M: IntrinsicMapper<V, T>>(
        &self,
        mapper: &mut M,
    ) -> Result<FloatIntrinsic<M::Value, M::Type>, M::Error> {
        Ok(match self {
            Self::Neg { value } => FloatIntrinsic::Neg {
                value: mapper.value(value)?,
            },
            Self::Binary { op, lhs, rhs } => FloatIntrinsic::Binary {
                op: *op,
                lhs: mapper.value(lhs)?,
                rhs: mapper.value(rhs)?,
            },
            Self::ToInt {
                value,
                target,
                signed,
            } => FloatIntrinsic::ToInt {
                value: mapper.value(value)?,
                target: mapper.ty(target)?,
                signed: *signed,
            },
            Self::Cast { value, target } => FloatIntrinsic::Cast {
                value: mapper.value(value)?,
                target: mapper.ty(target)?,
            },
        })
    }
}
