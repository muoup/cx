use cx_mir::mir::r#type::{MIRType, MIRTypeKind};
use cx_util::CXResult;

use crate::environment::TypeEnvironment;

pub fn compatible_types(
    env: &mut TypeEnvironment,
    type1: &MIRType,
    type2: &MIRType,
) -> CXResult<bool> {
    if env.type_eq(type1, type2) {
        return Ok(true);
    }

    if type1.strong_identifier.is_some() || type2.strong_identifier.is_some() {
        return Ok(type1.strong_identifier == type2.strong_identifier);
    }

    if type1.specifiers != type2.specifiers {
        return Ok(false);
    }

    match (&type1.kind, &type2.kind) {
        (
            MIRTypeKind::PointerTo {
                inner_type: inner1, ..
            },
            MIRTypeKind::PointerTo {
                inner_type: inner2, ..
            },
        ) => {
            let inner1 = env.symbols.context.get(*inner1).cloned().unwrap();
            let inner2 = env.symbols.context.get(*inner2).cloned().unwrap();

            compatible_types(env, &inner1, &inner2)
        }

        (
            MIRTypeKind::Array {
                inner_type: inner1,
                length: len1,
            },
            MIRTypeKind::Array {
                inner_type: inner2,
                length: len2,
            },
        ) => {
            if len1 != len2 {
                return Ok(false);
            }

            let inner1 = env.symbols.context.get(*inner1).cloned().unwrap();
            let inner2 = env.symbols.context.get(*inner2).cloned().unwrap();

            compatible_types(env, &inner1, &inner2)
        }

        // TODO: Should we have standalone enumeration types instead of decaying them immediately to their underlying integral type?
        (
            MIRTypeKind::Structured {
                fields: fields1, ..
            },
            MIRTypeKind::Structured {
                fields: fields2, ..
            },
        )
        | (
            MIRTypeKind::TaggedUnion {
                variants: fields1, ..
            },
            MIRTypeKind::TaggedUnion {
                variants: fields2, ..
            },
        )
        | (
            MIRTypeKind::Union {
                variants: fields1, ..
            },
            MIRTypeKind::Union {
                variants: fields2, ..
            },
        ) => {
            if fields1.len() != fields2.len() {
                return Ok(false);
            }

            let mut fields1 = fields1.clone();
            let mut fields2 = fields2.clone();

            // Unions in C are compatible even if their fields are in different orders, as long as they have the same set of fields.
            // Structs, however, require the fields to be in the same order.
            if type1.is_union() && type2.is_union() {
                fields1.sort_by(|(name1, _), (name2, _)| name1.cmp(name2));
                fields2.sort_by(|(name1, _), (name2, _)| name1.cmp(name2));
            }

            return Ok(fields1.iter().zip(fields2.iter()).all(
                |((_, field_type1), (_, field_type2))| {
                    let field_type1 = env.symbols.context.get(*field_type1).cloned().unwrap();
                    let field_type2 = env.symbols.context.get(*field_type2).cloned().unwrap();

                    compatible_types(env, &field_type1, &field_type2).unwrap_or(false)
                },
            ));
        }

        _ => todo!(),
    }
}
