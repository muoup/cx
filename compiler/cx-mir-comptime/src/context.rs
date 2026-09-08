use cx_mir::{
    MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalVariable, ty::interface::MTRegistry,
};

pub trait ComptimeContext {
    type Registry: MTRegistry;

    fn resolve(&self, id: MIRFunctionID) -> Option<&MIRFunction>;
    fn types(&self) -> &Self::Registry;

    fn global(&self, _id: MIRGlobalID) -> Option<&MIRGlobalVariable>;
    fn global_initializer(&self, _id: MIRGlobalID) -> Option<MIRFunctionID>;
}
