use cx_mir_data::{MIRGlobalType, MIRGlobalValue, MIRValue, MIRInstructionKind, types::MIRType};
use cx_typechecker_data::ast::TCExpr;
use cx_util::identifier::CXIdent;

use crate::{builder::MIRBuilder, instruction_gen::generate_instruction};

static CONTRACT_VIOLATION_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

fn next_contract_violation_id() -> usize {
    CONTRACT_VIOLATION_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}

pub fn add_contract_verification(
    builder: &mut MIRBuilder,
    function_name: &str,
    clause: &TCExpr,
    clause_name: &str,
) -> Option<()> {
    let mir_expr = generate_instruction(builder, clause)?;
    let msg_name = CXIdent::from(format!("__contract_violation_msg_{}", next_contract_violation_id()).as_str());
    
    let index = builder.add_global_variable(
        MIRGlobalValue {
            name: msg_name.clone(),
            _type: MIRGlobalType::StringLiteral(format!("'{}' violation of function '{}'", clause_name, function_name)),
            linkage: cx_mir_data::LinkageType::Static,
        }
    ); 
    
    builder.add_instruction(
        MIRInstructionKind::CompilerAssertion {
            condition: mir_expr,
            message: MIRValue::Global(index),
        },
        MIRType::unit()
    )?;
    
    Some(())
}