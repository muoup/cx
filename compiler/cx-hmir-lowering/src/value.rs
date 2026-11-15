pub mod address;
pub mod immediate;
pub mod place;
pub mod staged;

use cx_hmir::{HMIRDefID, ty::HMIRTypeID};
use cx_intrinsics::Intrinsic;
use cx_util::{dense_id, unsafe_float::FloatWrapper};

use crate::{
    emit::comptime,
    env::HMIREnvironment,
    eval::types,
    value::{
        address::{Addressable, HMIRAddress},
        immediate::{HMIRImmValue, HMIRImmediate},
        place::HMIRPlaceID,
    },
};

dense_id!(HMIRRegisterID, "$");

pub type HMIRValIntrinsic = Intrinsic<HMIRValue, HMIRTypeID>;

pub fn unit(_: &HMIREnvironment) -> HMIRValue {
    HMIRValue::new(HMIRTypeID::new(0), HMIRImmValue::Unit)
}

pub fn int(env: &HMIREnvironment, value: i128, ty: HMIRTypeID) -> HMIRValue {
    let value = comptime::int::normalize(value, types::int_type(env, ty));
    HMIRValue::new(ty, HMIRImmValue::Int(value))
}

pub fn float(env: &HMIREnvironment, value: f64, ty: HMIRTypeID) -> HMIRValue {
    let value = comptime::float::round(value, types::float_type(env, ty).width());
    HMIRValue::new(ty, HMIRImmValue::Float(FloatWrapper::from(value)))
}

pub fn as_type(value: &HMIRValue) -> Option<HMIRTypeID> {
    match value.kind() {
        HMIRValueKind::Immediate(HMIRImmValue::Type(ty)) => Some(*ty),
        _ => None,
    }
}

pub fn as_int(value: &HMIRValue) -> Option<i128> {
    match value.kind() {
        HMIRValueKind::Immediate(HMIRImmValue::Int(int)) => Some(*int),
        _ => None,
    }
}

pub fn describe(env: &HMIREnvironment, value: &HMIRValue) -> Option<String> {
    match value.kind() {
        HMIRValueKind::Immediate(HMIRImmValue::Type(ty)) => Some(types::name(env, *ty)),
        HMIRValueKind::Immediate(HMIRImmValue::Int(int)) => Some(int.to_string()),
        _ => None,
    }
}

fn as_addressable(value: &HMIRValue) -> Option<&Addressable> {
    match value.kind() {
        HMIRValueKind::Immediate(HMIRImmValue::Address(address)) if address.offset() == 0 => {
            Some(address.base())
        }
        _ => None,
    }
}

pub fn as_function(value: &HMIRValue) -> Option<HMIRDefID> {
    match as_addressable(value)? {
        Addressable::Def(function) => Some(*function),
        _ => None,
    }
}

pub fn as_literal(value: &HMIRValue) -> Option<&str> {
    match as_addressable(value)? {
        Addressable::Literal(string) => Some(string),
        _ => None,
    }
}

pub fn as_place(value: &HMIRValue) -> Option<HMIRPlaceID> {
    match value.kind() {
        HMIRValueKind::Reference(reference) if reference.offset() == 0 => Some(*reference.base()),
        _ => None,
    }
}

pub fn application(ty: HMIRTypeID, application: HMIRApplication<HMIRValue>) -> HMIRValue {
    let kind = match application.clone().try_map(HMIRImmediate::try_from) {
        Ok(immediate) => HMIRValueKind::Immediate(HMIRImmValue::Application(immediate)),
        Err(_) => HMIRValueKind::Application(application),
    };
    HMIRValue::new(ty, kind)
}

pub fn into_application(value: HMIRValue) -> Result<HMIRApplication<HMIRValue>, HMIRValue> {
    let ty = value.ty();
    match value.take_kind() {
        HMIRValueKind::Application(application) => Ok(application),
        HMIRValueKind::Immediate(HMIRImmValue::Application(application)) => {
            let args = application
                .args
                .into_iter()
                .map(|arg| arg.map(Into::into))
                .collect();
            Ok(HMIRApplication {
                callee: Box::new((*application.callee).into()),
                args,
            })
        }
        kind => Err(HMIRValue::new(ty, kind)),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HMIRApplication<V> {
    pub callee: Box<V>,
    pub args: Vec<Option<V>>,
}

impl<V> HMIRApplication<V> {
    pub fn try_map<U, E>(
        self,
        mut map: impl FnMut(V) -> Result<U, E>,
    ) -> Result<HMIRApplication<U>, E> {
        let callee = map(*self.callee)?;
        let args = self
            .args
            .into_iter()
            .map(|arg| arg.map(&mut map).transpose())
            .collect::<Result<_, _>>()?;
        Ok(HMIRApplication {
            callee: Box::new(callee),
            args,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HMIRValue {
    ty: HMIRTypeID,
    kind: HMIRValueKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRValueKind {
    /// Well-formed representation of a full value in any context, which is the form a comptime evaluation
    /// converges to when it hands its value back to runtime
    Immediate(HMIRImmValue),

    Application(HMIRApplication<HMIRValue>),

    /// An address into storage local to the function evaluating it, whether runtime or comptime; it is never
    /// well-formed outside of that function, so a comptime value holding one cannot converge
    Reference(HMIRAddress<HMIRPlaceID>),

    /// Intermediate computed values of a function, non-addressable
    Register(HMIRRegisterID),
}

impl HMIRValue {
    pub fn new(ty: HMIRTypeID, kind: impl Into<HMIRValueKind>) -> Self {
        Self {
            ty,
            kind: kind.into(),
        }
    }

    pub fn with_ty(mut self, ty: HMIRTypeID) -> Self {
        self.ty = ty;
        self
    }

    pub fn ty(&self) -> HMIRTypeID {
        self.ty
    }

    pub fn kind(&self) -> &HMIRValueKind {
        &self.kind
    }

    pub fn take_kind(self) -> HMIRValueKind {
        self.kind
    }
}

impl From<HMIRImmValue> for HMIRValueKind {
    fn from(value: HMIRImmValue) -> Self {
        HMIRValueKind::Immediate(value)
    }
}

impl From<HMIRRegisterID> for HMIRValueKind {
    fn from(value: HMIRRegisterID) -> Self {
        HMIRValueKind::Register(value)
    }
}

impl From<HMIRAddress<HMIRPlaceID>> for HMIRValueKind {
    fn from(value: HMIRAddress<HMIRPlaceID>) -> Self {
        HMIRValueKind::Reference(value)
    }
}
