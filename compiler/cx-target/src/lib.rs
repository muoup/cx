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

    pub const fn native() -> ArchitectureConfig {
        Self {
            pointer_size: std::mem::size_of::<*const ()>(),
            pointer_alignment: std::mem::align_of::<*const ()>(),
        }
    }

    pub const fn pointer_size(&self) -> usize {
        self.pointer_size
    }

    pub const fn pointer_alignment(&self) -> usize {
        self.pointer_alignment
    }
}
