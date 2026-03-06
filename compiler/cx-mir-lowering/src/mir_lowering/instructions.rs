//! Global variable lowering

use cx_lmir::{LMIRGlobalType, LMIRGlobalValue};
use cx_mir::mir::program::{MIRGlobalVarKind, MIRGlobalVariable};
use cx_util::CXResult;

use crate::builder::LMIRBuilder;

/// Lower a MIR global variable to LMIR
pub fn lower_global_value(
    builder: &mut LMIRBuilder,
    global: &MIRGlobalVariable,
) -> CXResult<LMIRGlobalValue> {
    let bc_linkage = builder.convert_linkage(global.linkage);

    match &global.kind {
        MIRGlobalVarKind::StringLiteral { name, value } => Ok(LMIRGlobalValue {
            name: name.clone(),
            _type: LMIRGlobalType::StringLiteral(value.clone()),
            linkage: bc_linkage,
        }),

        MIRGlobalVarKind::Variable {
            name,
            _type,
            initializer,
        } => {
            let bc_type = builder.convert_cx_type(_type);
            let bc_initializer = *initializer;

            Ok(LMIRGlobalValue {
                name: name.clone(),
                _type: LMIRGlobalType::Variable {
                    _type: bc_type,
                    initial_value: bc_initializer,
                },
                linkage: bc_linkage,
            })
        }
    }
}
