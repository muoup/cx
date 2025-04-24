use crate::parse::pass_molded::TypeMap;
use crate::parse::value_type::ValueType;

pub(crate) fn add_internal_types(type_map: &mut TypeMap) {
    macro_rules! add {
        ($name:expr, $type_:expr) => {
            type_map.insert(
                $name.to_string(),
                $type_.clone()
            );
        };
    }

    add!("i8", ValueType::Integer { bytes: 1, signed: true });
    add!("i16", ValueType::Integer { bytes: 2, signed: true });
    add!("i32", ValueType::Integer { bytes: 4, signed: true });
    add!("i64", ValueType::Integer { bytes: 8, signed: true });

    add!("u8", ValueType::Integer { bytes: 1, signed: false });
    add!("u16", ValueType::Integer { bytes: 2, signed: false });
    add!("u32", ValueType::Integer { bytes: 4, signed: false });
    add!("u64", ValueType::Integer { bytes: 8, signed: false });

    add!("f32", ValueType::Float { bytes: 4 });
    add!("f64", ValueType::Float { bytes: 8 });

    add!("int", ValueType::Integer { bytes: 4, signed: true });
    add!("signed int", ValueType::Integer { bytes: 4, signed: true });
    add!("unsigned int", ValueType::Integer { bytes: 4, signed: false });
    add!("long", ValueType::Integer { bytes: 8, signed: true });
    add!("long int", ValueType::Integer { bytes: 8, signed: true });
    add!("unsigned long", ValueType::Integer { bytes: 8, signed: false });
    add!("long long", ValueType::Integer { bytes: 8, signed: true });
    add!("long long int", ValueType::Integer { bytes: 8, signed: true });
    add!("unsigned long long", ValueType::Integer { bytes: 8, signed: false });

    add!("char", ValueType::Integer { bytes: 1, signed: false });
    add!("unsigned char", ValueType::Integer { bytes: 1, signed: false });
    add!("signed char", ValueType::Integer { bytes: 1, signed: true });

    add!("float", ValueType::Float { bytes: 4 });
    add!("double", ValueType::Float { bytes: 8 });
}