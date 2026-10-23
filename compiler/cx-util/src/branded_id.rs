#[macro_export]
macro_rules! branded_id {
    ($name:ident, $brand:ty, $display_prefix:literal) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name {
            brand: $brand,
            index: u32,
        }

        impl $name {
            pub fn new(brand: $brand, index: usize) -> Self {
                let index = u32::try_from(index).unwrap_or_else(|_| {
                    panic!(
                        "{} index {} exceeds the u32 range",
                        stringify!($name),
                        index
                    )
                });
                Self { brand, index }
            }

            pub const fn brand(self) -> $brand {
                self.brand
            }

            pub const fn index(self) -> usize {
                self.index as usize
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}{}@{}", $display_prefix, self.index, self.brand)
            }
        }
    };
}
