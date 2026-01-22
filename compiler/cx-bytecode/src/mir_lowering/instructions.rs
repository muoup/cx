//! Global variable lowering

use cx_bytecode_data::{BCGlobalType, BCGlobalValue};
use cx_typechecker_data::mir::program::{MIRGlobalVarKind, MIRGlobalVariable};
use cx_util::CXResult;

use crate::builder::BCBuilder;

/// Lower a MIR global variable to bytecode
pub fn lower_global_value(
    builder: &mut BCBuilder,
    global: &MIRGlobalVariable,
) -> CXResult<BCGlobalValue> {
    let bc_linkage = builder.convert_linkage(global.linkage);

    match &global.kind {
        MIRGlobalVarKind::StringLiteral { name, value } => Ok(BCGlobalValue {
            name: name.clone(),
            _type: BCGlobalType::StringLiteral(value.clone()),
            linkage: bc_linkage,
        }),

        MIRGlobalVarKind::Variable {
            name,
            _type,
            initializer,
        } => {
            let bc_type = builder.convert_cx_type(_type);
            let bc_initializer = *initializer;

            Ok(BCGlobalValue {
                name: name.clone(),
                _type: BCGlobalType::Variable {
                    _type: bc_type,
                    initial_value: bc_initializer,
                },
                linkage: bc_linkage,
            })
        }
    }
}
