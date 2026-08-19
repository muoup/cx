use cx_target::ArchitectureConfig;

use crate::{MIRIntType, MIRType, MIRTypeID};

pub trait MTRegistry {
    fn architecture(&self) -> &ArchitectureConfig;
    fn definition(&self, id: MIRTypeID) -> Option<&MIRType>;
    fn find(&self, ty: &MIRType) -> Option<MIRTypeID>;
    fn debug_name(&self, id: MIRTypeID) -> Option<&str>;
    
    fn unit(&self) -> MIRTypeID {
        MIRTypeID::new(0)
    }

    fn pointer_integer_type(&self) -> MIRIntType {
        MIRIntType::from_bytes(self.architecture().pointer_size() as u8)
            .expect("ArchitectureConfig guarantees a supported pointer size")
    }
}