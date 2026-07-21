#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArchitectureConfig {
    pointer_size: usize,
    pointer_alignment: usize,
}

impl ArchitectureConfig {
    pub const fn new(pointer_size: usize, pointer_alignment: usize) -> Self {
        assert!(matches!(pointer_size, 1 | 2 | 4 | 8 | 16));
        assert!(pointer_alignment.is_power_of_two());
        assert!(pointer_alignment <= u8::MAX as usize);

        Self {
            pointer_size,
            pointer_alignment,
        }
    }

    pub const fn pointer_size(&self) -> usize {
        self.pointer_size
    }

    pub const fn pointer_alignment(&self) -> usize {
        self.pointer_alignment
    }
}

impl Default for ArchitectureConfig {
    fn default() -> Self {
        Self::new(8, 8)
    }
}
