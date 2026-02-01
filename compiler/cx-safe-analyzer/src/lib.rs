use std::collections::HashMap;

use cx_mir::mir::program::{MIRFunction, MIRUnit};
use cx_safe_ir::ast::FMIRFunction;
use cx_util::CXResult;

use crate::mir_conversion::{convert_mir, environment::FMIREnvironment};

pub(crate) mod mir_conversion;

pub type FMIRAnalysisPass<'a> = &'a dyn Fn(&FMIRContext, FMIRFunction) -> FMIRFunction;

pub struct FMIRContext {
    env: FMIREnvironment,
    functions: HashMap<String, FMIRFunction>,
}

impl FMIRContext {
    pub fn new() -> Self {
        FMIRContext {
            env: FMIREnvironment::new(),
            functions: HashMap::new(),
        }
    }
    
    pub fn new_from(mir: &MIRUnit) -> CXResult<Self> {
        let mut context = FMIRContext::new();
        
        for function in mir.functions.iter() {
            if !function.prototype.contract.safe {
                continue;
            }
        
            context.consume_mir_function(function)?;
        }
        
        todo!()
    }
    
    pub fn consume_mir_function(&mut self, mir_function: &MIRFunction) -> CXResult<()> {
        let fmir_function = convert_mir(&mut self.env, mir_function);
        
        self.functions.insert(
            mir_function.prototype.name.as_string(),
            fmir_function.clone(),
        );
        
        Ok(())
    }

    pub fn apply_analysis_pass(&mut self, pass: FMIRAnalysisPass) {
        self.functions = self
            .functions
            .iter()
            .map(|(name, func)| (name.clone(), pass(self, func.clone())))
            .collect::<HashMap<_, _>>();
    }
    
    pub fn apply_standard_analysis_passes(&mut self) {
        // TODO: implement standard analysis passes
        ()
    }

    pub fn drain_functions(&mut self) -> Vec<(String, FMIRFunction)> {
        self.functions.drain().collect()
    }
}
