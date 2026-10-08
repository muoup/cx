use cx_tokens::token::{IntegerBase, IntegerLiteral, OperatorType};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct IntegerValue {
    pub(super) value: u64,
    pub(super) signed: bool,
}

impl IntegerValue {
    pub(super) fn literal(literal: IntegerLiteral) -> Result<Self, &'static str> {
        let signed = !literal.suffix.unsigned
            && (literal.base == IntegerBase::Decimal || literal.magnitude <= i64::MAX as u64);
        if signed && literal.magnitude > i64::MAX as u64 {
            return Err("integer literal is out of range for intmax_t");
        }
        Ok(Self {
            value: literal.magnitude,
            signed,
        })
    }

    pub(super) fn boolean(value: bool) -> Self {
        Self {
            value: u64::from(value),
            signed: true,
        }
    }

    pub(super) fn unary(
        self,
        operator: OperatorType,
        evaluate: bool,
    ) -> Result<Self, &'static str> {
        let signed = operator == OperatorType::Exclamation || self.signed;
        if !evaluate {
            return Ok(Self { value: 0, signed });
        }
        let value = match operator {
            OperatorType::Exclamation => u64::from(self.value == 0),
            OperatorType::Plus => self.value,
            OperatorType::Minus if self.signed => (self.value as i64)
                .checked_neg()
                .ok_or("signed integer overflow")?
                as u64,
            OperatorType::Minus => self.value.wrapping_neg(),
            OperatorType::Tilda => !self.value,
            _ => return Err("invalid unary operator"),
        };
        Ok(Self { value, signed })
    }

    pub(super) fn binary(
        self,
        operator: OperatorType,
        rhs: Self,
        evaluate: bool,
    ) -> Result<Self, &'static str> {
        use OperatorType::*;

        let arithmetic_signed = self.signed && rhs.signed;
        let signed = match operator {
            DoubleBar | DoubleAmpersand | Equal | NotEqual | Less | LessEqual | Greater
            | GreaterEqual => true,
            LShift | RShift => self.signed,
            _ => arithmetic_signed,
        };
        if !evaluate {
            return Ok(Self { value: 0, signed });
        }

        let lhs_signed = self.value as i64;
        let rhs_signed = rhs.value as i64;
        let value = match operator {
            DoubleBar => u64::from(self.value != 0 || rhs.value != 0),
            DoubleAmpersand => u64::from(self.value != 0 && rhs.value != 0),
            Equal => u64::from(self.value == rhs.value),
            NotEqual => u64::from(self.value != rhs.value),
            Less | LessEqual | Greater | GreaterEqual => {
                let ordering = if arithmetic_signed {
                    lhs_signed.cmp(&rhs_signed)
                } else {
                    self.value.cmp(&rhs.value)
                };
                u64::from(match operator {
                    Less => ordering.is_lt(),
                    LessEqual => ordering.is_le(),
                    Greater => ordering.is_gt(),
                    GreaterEqual => ordering.is_ge(),
                    _ => unreachable!(),
                })
            }
            Bar => self.value | rhs.value,
            Ampersand => self.value & rhs.value,
            Caret => self.value ^ rhs.value,
            LShift | RShift => {
                if (rhs.signed && rhs_signed < 0) || rhs.value >= 64 {
                    return Err("shift count must be between 0 and 63");
                }
                let shift = rhs.value as u32;
                match operator {
                    RShift if self.signed => (lhs_signed >> shift) as u64,
                    RShift => self.value >> shift,
                    LShift if self.signed => {
                        if lhs_signed < 0 {
                            return Err("left shift of a negative signed integer");
                        }
                        i64::try_from((lhs_signed as i128) << shift)
                            .map_err(|_| "signed integer overflow")? as u64
                    }
                    LShift => self.value << shift,
                    _ => unreachable!(),
                }
            }
            Plus | Minus | Asterisk | Slash | Percent => {
                if matches!(operator, Slash | Percent) && rhs.value == 0 {
                    return Err(if operator == Slash {
                        "division by zero"
                    } else {
                        "remainder by zero"
                    });
                }
                if arithmetic_signed {
                    match operator {
                        Plus => lhs_signed.checked_add(rhs_signed),
                        Minus => lhs_signed.checked_sub(rhs_signed),
                        Asterisk => lhs_signed.checked_mul(rhs_signed),
                        Slash => lhs_signed.checked_div(rhs_signed),
                        Percent => lhs_signed.checked_rem(rhs_signed),
                        _ => unreachable!(),
                    }
                    .ok_or("signed integer overflow")? as u64
                } else {
                    match operator {
                        Plus => self.value.wrapping_add(rhs.value),
                        Minus => self.value.wrapping_sub(rhs.value),
                        Asterisk => self.value.wrapping_mul(rhs.value),
                        Slash => self.value / rhs.value,
                        Percent => self.value % rhs.value,
                        _ => unreachable!(),
                    }
                }
            }
            _ => return Err("invalid binary operator"),
        };
        Ok(Self { value, signed })
    }
}
