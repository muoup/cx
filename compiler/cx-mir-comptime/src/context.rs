use cx_mir::{
    MIRConstant, MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalKind, ty::interface::MTRegistry,
};

pub trait ComptimeContext {
    type Registry: MTRegistry;
    
    fn resolve(&self, id: MIRFunctionID) -> Option<&MIRFunction>;
    fn types(&self) -> &Self::Registry;

    fn global_constant(&self, _id: MIRGlobalID) -> Option<MIRConstant> {
        None
    }

    fn global_initializer(&self, _id: MIRGlobalID) -> Option<MIRFunctionID> {
        None
    }

    fn global_kind(&self, _id: MIRGlobalID) -> Option<MIRGlobalKind> {
        None
    }
}
