#[macro_export]
macro_rules! dense_id {
    ($name:ident, $display_prefix:literal) => {
        #[derive(
            Debug,
            Clone,
            Copy,
            speedy::Readable,
            speedy::Writable,
            PartialEq,
            Eq,
            PartialOrd,
            Ord,
            Hash,
        )]
        pub struct $name(pub usize);

        impl $name {
            pub const fn new(index: usize) -> Self {
                Self(index)
            }

            pub const fn index(self) -> usize {
                self.0
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}{}", $display_prefix, self.0)
            }
        }
    };
}
