use crate::parse::pass_ast::TypeMap;
use crate::parse::value_type::CXValType;

pub(crate) fn add_internal_types(type_map: &mut TypeMap) {
    macro_rules! add {
        ($name:expr, $type_:expr) => {
            type_map.insert(
                $name.to_string(),
                $type_.clone()
            );
        };
    }

    add!("void", CXValType::Unit);

    add!("i8", CXValType::Integer { bytes: 1, signed: true });
    add!("i16", CXValType::Integer { bytes: 2, signed: true });
    add!("i32", CXValType::Integer { bytes: 4, signed: true });
    add!("i64", CXValType::Integer { bytes: 8, signed: true });

    add!("u8", CXValType::Integer { bytes: 1, signed: false });
    add!("u16", CXValType::Integer { bytes: 2, signed: false });
    add!("u32", CXValType::Integer { bytes: 4, signed: false });
    add!("u64", CXValType::Integer { bytes: 8, signed: false });

    add!("f32", CXValType::Float { bytes: 4 });
    add!("f64", CXValType::Float { bytes: 8 });

    add!("int", CXValType::Integer { bytes: 4, signed: true });
    add!("signed int", CXValType::Integer { bytes: 4, signed: true });
    add!("unsigned int", CXValType::Integer { bytes: 4, signed: false });
    add!("long", CXValType::Integer { bytes: 8, signed: true });
    add!("long int", CXValType::Integer { bytes: 8, signed: true });
    add!("unsigned long", CXValType::Integer { bytes: 8, signed: false });
    add!("long long", CXValType::Integer { bytes: 8, signed: true });
    add!("long long int", CXValType::Integer { bytes: 8, signed: true });
    add!("unsigned long long", CXValType::Integer { bytes: 8, signed: false });

    add!("char", CXValType::Integer { bytes: 1, signed: false });
    add!("unsigned char", CXValType::Integer { bytes: 1, signed: false });
    add!("signed char", CXValType::Integer { bytes: 1, signed: true });

    add!("float", CXValType::Float { bytes: 4 });
    add!("double", CXValType::Float { bytes: 8 });
}