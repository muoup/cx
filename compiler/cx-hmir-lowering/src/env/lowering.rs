use std::collections::HashMap;

use cx_hmir::{
    HMIRBody, HMIRFnSignature,
    body::HMIRLocalID,
    expr::constant::HMIRConstant,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    unit::function::HMIRFnPrototype,
    value::{HMIRPlace, HMIRSlotID, HMIRValue, HMIRValueKind},
};
use cx_log::CXResult;
use cx_mir::{
    MIRBindable, MIRFunction, MIRInstructionKind, MIRInternalIntrinsic, MIRRegister,
    MIRRegisterDecl, MIRScopeID, MIRTarget, MIRValue,
};
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::MIRFunctionBuilder},
    lower::{constant::mir_constant, types::lower_prototype},
};

#[derive(Debug)]
pub struct FnLoweringContext<'env, 'global, 'hmir> {
    env: &'env mut HMIREnvironment<'global, 'hmir>,
    signature: &'hmir HMIRFnSignature,
    builder: MIRFunctionBuilder,

    locals: HashMap<HMIRLocalID, HMIRValue>,
    slots: Vec<MIRBindable>,
    open_scopes: Vec<MIRScopeID>,
}

impl<'env, 'global, 'hmir> FnLoweringContext<'env, 'global, 'hmir> {
    pub fn new(
        env: &'env mut HMIREnvironment<'global, 'hmir>,
        body: &HMIRBody,
        prototype: &'hmir HMIRFnPrototype,
    ) -> CXResult<Self> {
        let mir_prototype = lower_prototype(env, body, prototype)?;

        Ok(Self {
            env,
            signature: prototype.signature(),
            builder: MIRFunctionBuilder::new(mir_prototype),

            locals: HashMap::new(),
            slots: Vec::new(),
            open_scopes: Vec::new(),
        })
    }

    pub fn env(&self) -> &HMIREnvironment<'global, 'hmir> {
        self.env
    }

    pub fn env_mut(&mut self) -> &mut HMIREnvironment<'global, 'hmir> {
        self.env
    }

    pub fn signature(&self) -> &'hmir HMIRFnSignature {
        self.signature
    }

    pub fn builder(&self) -> &MIRFunctionBuilder {
        &self.builder
    }

    pub fn builder_mut(&mut self) -> &mut MIRFunctionBuilder {
        &mut self.builder
    }

    pub fn insert_local(&mut self, id: HMIRLocalID, value: HMIRValue) {
        self.locals.insert(id, value);
    }

    pub fn get_local(&self, id: &HMIRLocalID) -> Option<&HMIRValue> {
        self.locals.get(id)
    }

    pub fn bind(&mut self, ty: HMIRTypeID, binding: MIRBindable) -> HMIRValue {
        self.slots.push(binding);
        let slot = HMIRSlotID::new(self.slots.len() - 1);

        HMIRValue::place(ty, HMIRPlace::Local(slot))
    }

    pub fn slot(&self, slot: HMIRSlotID) -> &MIRBindable {
        &self.slots[slot.index()]
    }

    pub fn open_scopes(&self) -> &[MIRScopeID] {
        &self.open_scopes
    }

    pub fn push_scope(&mut self, scope: MIRScopeID) {
        self.open_scopes.push(scope);
    }

    pub fn pop_scope(&mut self) -> MIRScopeID {
        self.open_scopes
            .pop()
            .expect("Attempt to pop a scope when none is open")
    }

    pub fn materialize(&mut self, value: &HMIRValue, span: &TokenRange) -> CXResult<MIRValue> {
        Ok(match value.kind() {
            HMIRValueKind::Constant(HMIRConstant::Address { place, path, .. }) => {
                self.address(*place, path, value.ty(), span)?
            }
            HMIRValueKind::Constant(constant) => {
                MIRValue::Constant(mir_constant(self.env, constant, span)?)
            }
            HMIRValueKind::Place(HMIRPlace::Local(slot)) => match self.slot(*slot) {
                MIRBindable::Register(register) => MIRValue::Register(*register),
                MIRBindable::Place(place) => MIRValue::PlaceRef(*place),
            },
            HMIRValueKind::Place(HMIRPlace::Global(_)) => {
                todo!("global places: globals are not lowered yet")
            }
            HMIRValueKind::Indirect(pointer) => MIRValue::Register(self.reference(pointer)),
        })
    }

    pub fn target(&self, value: &HMIRValue) -> Option<MIRTarget> {
        match value.kind() {
            HMIRValueKind::Constant(_) => None,
            HMIRValueKind::Place(HMIRPlace::Local(slot)) => match self.slot(*slot) {
                MIRBindable::Register(_) => None,
                MIRBindable::Place(place) => Some(MIRTarget::Place(*place)),
            },
            HMIRValueKind::Place(HMIRPlace::Global(_)) => {
                todo!("global places: globals are not lowered yet")
            }
            HMIRValueKind::Indirect(pointer) => Some(MIRTarget::Indirect(self.reference(pointer))),
        }
    }

    fn reference(&self, pointer: &HMIRValue) -> MIRRegister {
        match pointer.kind() {
            HMIRValueKind::Place(HMIRPlace::Local(slot)) => match self.slot(*slot) {
                MIRBindable::Register(register) => *register,
                MIRBindable::Place(_) => {
                    unreachable!("an indirect value addresses through a register")
                }
            },
            _ => todo!("indirect value through a subobject or global address"),
        }
    }

    fn address(
        &mut self,
        place: HMIRPlace,
        path: &[usize],
        ty: HMIRTypeID,
        span: &TokenRange,
    ) -> CXResult<MIRValue> {
        if !path.is_empty() {
            todo!("address of a subobject: aggregates are not lowered yet");
        }

        let HMIRPlace::Local(slot) = place else {
            todo!("global places: globals are not lowered yet");
        };
        let MIRBindable::Place(place) = *self.slot(slot) else {
            unreachable!("only a place has an address");
        };

        if matches!(
            self.env.resolve_type_id(ty).kind(),
            HMIRTypeKind::ReferenceTo(_)
        ) {
            return Ok(MIRValue::PlaceRef(place));
        }

        let ty = self.env.lower_type(ty, span)?;
        let out = self
            .builder
            .allocate_register(MIRRegisterDecl::new(ty, None));
        let intrinsic = MIRInternalIntrinsic::PlaceAddress {
            out: MIRTarget::Register(out),
            place,
        };
        self.builder.emit(
            MIRInstructionKind::IntrinsicOp(intrinsic.into()),
            span.clone(),
        );

        Ok(MIRValue::Register(out))
    }

    pub fn finish(self) -> MIRFunction {
        self.builder.finish()
    }
}
