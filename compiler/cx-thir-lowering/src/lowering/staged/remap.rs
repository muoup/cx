use std::collections::{HashMap, HashSet};

use cx_log::CXResult;
use cx_log::catalogue::mir as catalogue;
use cx_log::error::CXError;
use cx_mir::visit::{MIRVisitRole, MIRVisitorMut, MIRWalk};
use cx_mir::{
    MIRAggregateOp, MIRBasicBlockID, MIRBlockTarget, MIRInstrKind, MIRPlaceID, MIRRegister,
    MIRScopeID, MIRTarget, MIRValue,
};
use cx_tokens::TokenRange;

use crate::log::mir_error;

pub(super) struct Remap<'a> {
    pub registers: &'a HashMap<MIRRegister, MIRValue>,
    pub places: &'a HashMap<MIRPlaceID, MIRPlaceID>,
    pub omitted_places: &'a HashSet<MIRPlaceID>,
    pub blocks: &'a HashMap<MIRBasicBlockID, MIRBasicBlockID>,
    pub block_params: &'a HashMap<MIRBasicBlockID, Vec<bool>>,
    pub scopes: &'a HashMap<MIRScopeID, MIRScopeID>,
    pub range: &'a TokenRange,
}

impl Remap<'_> {
    pub(super) fn value(&self, value: &MIRValue) -> CXResult<MIRValue> {
        Ok(match value {
            MIRValue::Register(register) => {
                self.registers.get(register).cloned().ok_or_else(|| {
                    mir_error(
                        self.range,
                        (
                            &catalogue::MISSING_MAPPING,
                            (
                                format!("register {:?}", register),
                                "template register".into(),
                            ),
                        ),
                    )
                })?
            }
            MIRValue::Reference(MIRTarget::Place(id)) if self.omitted_places.contains(id) => {
                MIRValue::Constant(cx_mir::MIRConstant::Unit)
            }
            MIRValue::Reference(target) => MIRValue::Reference(self.storage(*target)?),
            MIRValue::Constant(value) => MIRValue::Constant(value.clone()),
        })
    }

    fn storage(&self, target: MIRTarget) -> CXResult<MIRTarget> {
        match target {
            MIRTarget::Place(place) => Ok(MIRTarget::Place(self.place(place)?)),
            MIRTarget::Global(_) => Ok(target),
            MIRTarget::Indirect(register) => match self.registers.get(&register) {
                Some(MIRValue::Register(register)) => Ok(MIRTarget::Indirect(*register)),
                Some(MIRValue::Reference(target)) => Ok(*target),
                Some(MIRValue::Constant(cx_mir::MIRConstant::Global {
                    global, offset: 0, ..
                })) => Ok(MIRTarget::Global(*global)),
                _ => Err(mir_error(
                    self.range,
                    (
                        &catalogue::MISSING_MAPPING,
                        ("indirect target".into(), "template register".into()),
                    ),
                )),
            },
        }
    }

    fn place(&self, id: MIRPlaceID) -> CXResult<MIRPlaceID> {
        self.places.get(&id).copied().ok_or_else(|| {
            mir_error(
                self.range,
                (
                    &catalogue::MISSING_MAPPING,
                    ("place".into(), "template place".into()),
                ),
            )
        })
    }

    fn target(&self, target: &MIRBlockTarget) -> CXResult<MIRBlockTarget> {
        let block = *self.blocks.get(&target.block).ok_or_else(|| {
            mir_error(
                self.range,
                (
                    &catalogue::MISSING_MAPPING,
                    ("block".into(), "template block".into()),
                ),
            )
        })?;
        let params = self.block_params.get(&target.block);
        Ok(MIRBlockTarget::with_args(
            block,
            target
                .args
                .iter()
                .enumerate()
                .filter(|(index, _)| {
                    params
                        .and_then(|params| params.get(*index))
                        .copied()
                        .unwrap_or(false)
                })
                .map(|(_, value)| self.value(value))
                .collect::<CXResult<Vec<_>>>()?,
        ))
    }

    fn register(&self, register: MIRRegister) -> CXResult<MIRRegister> {
        match self.registers.get(&register) {
            Some(MIRValue::Register(register)) => Ok(*register),
            _ => Err(mir_error(
                self.range,
                (
                    &catalogue::MISSING_MAPPING,
                    ("output register".into(), "template output register".into()),
                ),
            )),
        }
    }

    fn omitted_place(&self, place: MIRPlaceID) -> bool {
        self.omitted_places.contains(&place)
    }

    fn omitted_register(&self, register: MIRRegister) -> bool {
        matches!(
            self.registers.get(&register),
            Some(MIRValue::Constant(cx_mir::MIRConstant::Unit))
        )
    }

    pub(super) fn omitted(&self, kind: &MIRInstrKind) -> bool {
        match kind {
            MIRInstrKind::Initialize { place }
            | MIRInstrKind::Bind { place, .. }
            | MIRInstrKind::Store {
                target: MIRTarget::Place(place),
                ..
            }
            | MIRInstrKind::Invalidate { place, .. } => self.omitted_place(*place),
            MIRInstrKind::Copy { out, .. }
            | MIRInstrKind::Let { out, .. }
            | MIRInstrKind::Intrinsic(cx_mir::MIRIntrinsic::VaArg { out, .. })
            | MIRInstrKind::BinOp { out, .. }
            | MIRInstrKind::UnOp { out, .. }
            | MIRInstrKind::Coerce { out, .. } => self.omitted_register(*out),
            MIRInstrKind::AggregateOp(MIRAggregateOp::Target { out, .. }) => {
                self.omitted_register(*out)
            }
            MIRInstrKind::AggregateOp(MIRAggregateOp::Value { out, .. }) => {
                self.omitted_register(*out)
            }
            _ => false,
        }
    }

    pub(super) fn instruction(&self, kind: &MIRInstrKind) -> CXResult<MIRInstrKind> {
        let mut mapped = kind.clone();
        if let MIRInstrKind::Call { out, .. } = &mut mapped
            && out.as_ref().is_some_and(|out| self.omitted_register(*out))
        {
            *out = None;
        }
        let mut visitor = StructuralRemapper { remap: self };
        mapped.visit_mut(&mut visitor)?;
        Ok(mapped)
    }
}

struct StructuralRemapper<'a> {
    remap: &'a Remap<'a>,
}

impl MIRVisitorMut<'_> for StructuralRemapper<'_> {
    type Error = CXError;

    fn register(
        &mut self,
        register: &mut MIRRegister,
        role: MIRVisitRole,
    ) -> Result<(), Self::Error> {
        if matches!(role, MIRVisitRole::Define | MIRVisitRole::Write) {
            *register = self.remap.register(*register)?;
        }
        Ok(())
    }

    fn storage(&mut self, target: &mut MIRTarget, _role: MIRVisitRole) -> Result<(), Self::Error> {
        *target = self.remap.storage(*target)?;
        Ok(())
    }

    fn place(&mut self, place: &mut MIRPlaceID, _role: MIRVisitRole) -> Result<(), Self::Error> {
        *place = self.remap.place(*place)?;
        Ok(())
    }

    fn value(&mut self, value: &mut MIRValue) -> Result<(), Self::Error> {
        let source = std::mem::replace(value, MIRValue::Constant(cx_mir::MIRConstant::Unit));
        *value = self.remap.value(&source)?;
        Ok(())
    }

    fn target(&mut self, target: &mut MIRBlockTarget) -> Result<(), Self::Error> {
        let source = target.clone();
        *target = self.remap.target(&source)?;
        Ok(())
    }

    fn block(&mut self, block: &mut MIRBasicBlockID) -> Result<(), Self::Error> {
        *block = *self.remap.blocks.get(block).ok_or_else(|| {
            mir_error(
                self.remap.range,
                (
                    &catalogue::MISSING_MAPPING,
                    ("block".into(), "template block".into()),
                ),
            )
        })?;
        Ok(())
    }

    fn scope(&mut self, scope: &mut MIRScopeID) -> Result<(), Self::Error> {
        *scope = *self.remap.scopes.get(scope).ok_or_else(|| {
            mir_error(
                self.remap.range,
                (
                    &catalogue::MISSING_MAPPING,
                    ("scope".into(), "template scope".into()),
                ),
            )
        })?;
        Ok(())
    }
}
