use cx_bytecode_data::{BCInstruction, BCInstructionKind, BCValue, types::BCType};
use cx_typechecker_data::mir::expression::MIRInstruction;
use cx_util::CXResult;

use crate::builder::MIRBuilder;

pub fn lower_instruction(
    builder: &mut MIRBuilder,
    instruction: &MIRInstruction
) -> CXResult<BCValue> {
    match instruction {
        MIRInstruction::CreateStackRegion { result, _type } => {
            let bc_type = builder.convert_cx_type(_type)?;
            
            builder.add_instruction(
                BCInstructionKind::Allocate { 
                    alignment: bc_type.alignment(),
                    _type: bc_type,
                },
                BCType::default_pointer(),
                Some(result.clone()),
            )
        },
        
        _ => todo!()
    }
}