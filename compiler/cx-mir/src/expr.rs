use cx_util::identifier::CXIdent;

use crate::ty::MIRType;

#[derive(Debug, Clone)]
pub struct MIRPlace(CXIdent);

#[derive(Debug, Clone)]
pub enum MIROperand {
    Place(MIRPlace),
    Temp(CXIdent)
}

#[derive(Debug, Clone)]
pub struct MIRLivenessMarker(CXIdent);

#[derive(Debug, Clone)]
pub struct MIRBasicBlock(CXIdent);

pub struct MIRInstr {
    kind: MIRInstrKind
}

#[derive(Debug, Clone)]
pub enum MIRInstrKind {
    TrackLive(MIRLivenessMarker),
    LeakLive(MIRLivenessMarker),

    CreatePlace {
        out: MIRPlace,
        ty: MIRType,
    },
    
    CopyInto {
        dest: MIRPlace,
        src: MIROperand,
        ty: MIRType
    },
}