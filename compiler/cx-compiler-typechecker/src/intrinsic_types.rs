use cx_data_ast::parse::ast::TypeMap;
use cx_data_ast::parse::value_type::CXTypeUnion;

pub fn add_internal_types(type_map: &mut TypeMap) {
    macro_rules! add {
        ($name:expr, $type_:expr) => {
            type_map.insert(
                $name.to_string(),
                $type_.to_val_type()
            );
        };
    }

    add!("void", CXTypeUnion::Unit);

    add!("i8", CXTypeUnion::Integer { bytes: 1, signed: true });
    add!("i16", CXTypeUnion::Integer { bytes: 2, signed: true });
    add!("i32", CXTypeUnion::Integer { bytes: 4, signed: true });
    add!("i64", CXTypeUnion::Integer { bytes: 8, signed: true });

    add!("u8", CXTypeUnion::Integer { bytes: 1, signed: false });
    add!("u16", CXTypeUnion::Integer { bytes: 2, signed: false });
    add!("u32", CXTypeUnion::Integer { bytes: 4, signed: false });
    add!("u64", CXTypeUnion::Integer { bytes: 8, signed: false });

    add!("f32", CXTypeUnion::Float { bytes: 4 });
    add!("f64", CXTypeUnion::Float { bytes: 8 });

    add!("int", CXTypeUnion::Integer { bytes: 4, signed: true });
    add!("signed int", CXTypeUnion::Integer { bytes: 4, signed: true });
    add!("unsigned int", CXTypeUnion::Integer { bytes: 4, signed: false });
    add!("signed", CXTypeUnion::Integer { bytes: 4, signed: true });
    add!("unsigned", CXTypeUnion::Integer { bytes: 4, signed: false });
    add!("long", CXTypeUnion::Integer { bytes: 8, signed: true });
    add!("long int", CXTypeUnion::Integer { bytes: 8, signed: true });
    add!("unsigned long", CXTypeUnion::Integer { bytes: 8, signed: false });
    add!("long long", CXTypeUnion::Integer { bytes: 8, signed: true });
    add!("long long int", CXTypeUnion::Integer { bytes: 8, signed: true });
    add!("unsigned long long", CXTypeUnion::Integer { bytes: 8, signed: false });

    add!("char", CXTypeUnion::Integer { bytes: 1, signed: false });
    add!("unsigned char", CXTypeUnion::Integer { bytes: 1, signed: false });
    add!("signed char", CXTypeUnion::Integer { bytes: 1, signed: true });

    add!("float", CXTypeUnion::Float { bytes: 4 });
    add!("double", CXTypeUnion::Float { bytes: 8 });
}