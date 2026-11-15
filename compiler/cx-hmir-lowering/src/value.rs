pub mod address;
pub mod application;
pub mod immediate;
pub mod place;
pub mod staged;

use cx_hmir::{HMIRDefID, ty::HMIRTypeID};
use cx_intrinsics::Intrinsic;
use cx_util::{dense_id, unsafe_float::FloatWrapper};

use crate::{
    emit::comptime,
    env::HMIREnvironment,
    eval::{staged::adapts, types},
    value::{
        address::{Addressable, HMIRAddress},
        application::HMIRApplication,
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

pub fn as_application(value: &HMIRValue) -> Option<HMIRApplication<HMIRValue>> {
    match value.kind() {
        HMIRValueKind::Application(application) => Some(application.clone()),
        HMIRValueKind::Immediate(HMIRImmValue::Application(application)) => {
            let (callee, args) = application.clone().into_parts();
            let args = args.into_iter().map(|arg| arg.map(Into::into)).collect();
            Some(HMIRApplication::new(callee.into(), args))
        }
        _ => None,
    }
}

pub fn unapplied(env: &HMIREnvironment, value: HMIRValue) -> (HMIRValue, Vec<Option<HMIRValue>>) {
    if let Some(application) = as_application(&value)
        && !adapts(env, application.callee().ty(), value.ty())
    {
        return application.into_parts();
    }
    (value, Vec::new())
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
