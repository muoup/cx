use cx_target::ArchitectureConfig;

#[derive(Debug, Clone, Copy)]
pub struct HMIRSettings {
    architecture: ArchitectureConfig,
    require_explicit_return: bool,
}

impl HMIRSettings {
    pub fn new(architecture: ArchitectureConfig) -> Self {
        Self {
            architecture,
            require_explicit_return: true,
        }
    }

    pub fn with_explicit_return(mut self, required: bool) -> Self {
        self.require_explicit_return = required;
        self
    }

    pub fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    pub fn require_explicit_return(&self) -> bool {
        self.require_explicit_return
    }
}
