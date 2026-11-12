use cx_target::ArchitectureConfig;

#[derive(Debug, Clone, Copy)]
pub struct HMIRSettings {
    architecture: ArchitectureConfig,
    require_explicit_return: bool,
    assertions: bool,
    record_types: bool,
}

impl HMIRSettings {
    pub fn new(architecture: ArchitectureConfig) -> Self {
        Self {
            architecture,
            require_explicit_return: true,
            assertions: true,
            record_types: false,
        }
    }

    pub fn with_type_recording(mut self, enabled: bool) -> Self {
        self.record_types = enabled;
        self
    }

    pub fn record_types(&self) -> bool {
        self.record_types
    }

    pub fn with_explicit_return(mut self, required: bool) -> Self {
        self.require_explicit_return = required;
        self
    }

    pub fn with_assertions(mut self, enabled: bool) -> Self {
        self.assertions = enabled;
        self
    }

    pub fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    pub fn require_explicit_return(&self) -> bool {
        self.require_explicit_return
    }

    pub fn assertions(&self) -> bool {
        self.assertions
    }
}
