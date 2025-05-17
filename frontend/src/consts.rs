use crate::ast::Ident;
use bigdecimal::ToPrimitive;
use bigdecimal::BigDecimal;
use bigdecimal::FromPrimitive;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Write;
use std::i32;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct IntConstant(i128);

impl fmt::Display for IntConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl IntConstant {
    pub fn is_zero(&self) -> bool {
        self.0 == 0
    }
    
    pub fn zero() -> Self {
        Self(0)
    }

    pub fn parse_str(s: &str) -> Option<Self> {
        let s = s.trim();
        let first_char = s.chars().next()?;

        match first_char {
            '#' => {
                let val: u8 = s[1..].parse().ok()?;
                Some(IntConstant::from(val))
            }

            // hex literals always produce unsigned values
            '$' => {
                let val = u64::from_str_radix(&s[1..], 16).ok()?;
                Some(IntConstant::from(val))
            }

            // negative numbers produce int32, or uint32 if they're too large
            '-' => {
                let val: i64 = s.parse().ok()?;
                Some(IntConstant::from(val))
            }

            // positive numbers produce int32, or i64/u64 if they're too large
            _ => {
                let val: u64 = s.parse().ok()?;
                Some(IntConstant::from(val))
            }
        }
    }

    pub fn as_i128(&self) -> i128 {
        self.0
    }

    pub fn as_i8(&self) -> Option<i8> {
        self.0.to_i8()
    }

    pub fn as_u8(&self) -> Option<u8> {
        self.0.to_u8()
    }

    pub fn as_i16(&self) -> Option<i16> {
        self.0.to_i16()
    }

    pub fn as_u16(&self) -> Option<u16> {
        self.0.to_u16()
    }

    pub fn as_i32(&self) -> Option<i32> {
        self.0.to_i32()
    }

    pub fn as_u32(&self) -> Option<u32> {
       self.0.to_u32()
    }

    pub fn as_i64(&self) -> Option<i64> {
        self.0.to_i64()
    }

    pub fn as_u64(&self) -> Option<u64> {
        self.0.to_u64()
    }

    pub fn as_f32(&self) -> Option<f32> {
        self.0.to_f32()
    }

    pub fn as_f64(&self) -> Option<f64> {
        self.0.to_f64()
    }

    pub fn as_usize(&self) -> Option<usize> {
        self.0.to_usize()
    }

    pub fn as_isize(&self) -> Option<isize> {
        self.0.to_isize()
    }
}

impl Ord for IntConstant {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_i128().cmp(&other.as_i128())
    }
}

impl PartialOrd for IntConstant {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl From<i8> for IntConstant {
    fn from(val: i8) -> Self {
        IntConstant(val as i128)
    }
}

impl From<u8> for IntConstant {
    fn from(val: u8) -> Self {
        IntConstant(val as i128)
    }
}

impl From<i16> for IntConstant {
    fn from(val: i16) -> Self {
        IntConstant(val as i128)
    }
}

impl From<u16> for IntConstant {
    fn from(val: u16) -> Self {
        IntConstant(val as i128)
    }
}

impl From<i32> for IntConstant {
    fn from(val: i32) -> Self {
        IntConstant(val as i128)
    }
}

impl From<u32> for IntConstant {
    fn from(val: u32) -> Self {
        IntConstant(val as i128)
    }
}

impl From<i64> for IntConstant {
    fn from(val: i64) -> Self {
        IntConstant(val as i128)
    }
}

impl From<u64> for IntConstant {
    fn from(val: u64) -> Self {
        IntConstant(val as i128)
    }
}

impl From<usize> for IntConstant {
    fn from(val: usize) -> Self {
        IntConstant(val as i128)
    }
}

impl From<isize> for IntConstant {
    fn from(val: isize) -> Self {
        IntConstant(val as i128)
    }
}

impl From<i128> for IntConstant {
    fn from(val: i128) -> Self {
        IntConstant(val)
    }
}

impl Add for IntConstant {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self::from(self.as_i128().wrapping_add(rhs.as_i128()))
    }
}

impl Sub for IntConstant {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Self::from(self.as_i128().wrapping_sub(rhs.as_i128()))
    }
}

impl Mul for IntConstant {
    type Output = IntConstant;

    fn mul(self, rhs: IntConstant) -> IntConstant {
        Self::from(self.as_i128() * rhs.as_i128())
    }
}

impl Div for IntConstant {
    type Output = IntConstant;

    fn div(self, rhs: IntConstant) -> IntConstant {
        Self::from(self.as_i128() / rhs.as_i128())
    }
}

// TODO needs NaN/Inf variants
#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct RealConstant(pub BigDecimal);

impl RealConstant {
    pub fn parse_str(s: &str) -> Option<Self> {
        let val: f64 = s.parse().ok()?;
        Some(RealConstant::from(val))
    }

    pub fn as_f32(&self) -> Option<f32> {
        self.0.to_f32()
    }

    pub fn as_f64(&self) -> Option<f64> {
        self.0.to_f64()
    }
    
    pub fn round(&self) -> Self {
        RealConstant(self.0.round(0))
    }
}

impl From<f64> for RealConstant {
    fn from(val: f64) -> Self {
        RealConstant(BigDecimal::from_f64(val).expect("NaN/infinite constant values not supported yet"))
    }
}

impl Sub for RealConstant {
    type Output = RealConstant;

    fn sub(self, rhs: RealConstant) -> RealConstant {
        RealConstant(self.0 - rhs.0)
    }
}

impl Add for RealConstant {
    type Output = RealConstant;

    fn add(self, rhs: RealConstant) -> RealConstant {
        RealConstant(self.0 + rhs.0)
    }
}

impl Mul for RealConstant {
    type Output = RealConstant;

    fn mul(self, rhs: RealConstant) -> RealConstant {
        RealConstant(self.0 * rhs.0)
    }
}

impl Div for RealConstant {
    type Output = RealConstant;

    fn div(self, rhs: RealConstant) -> RealConstant {
        RealConstant(self.0 / rhs.0)
    }
}

impl fmt::Display for RealConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RealConstant(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumConstant {
    pub ordinal: u64,
    pub enumeration: Ident,
}

impl EnumConstant {
    pub fn new(ordinal: u64, enumeration: impl Into<Ident>) -> Self {
        EnumConstant {
            ordinal,
            enumeration: enumeration.into(),
        }
    }
}

impl fmt::Display for EnumConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.enumeration, self.ordinal)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SetConstant {
    pub included_values: Vec<Ident>,
    pub set: Ident,
}

impl SetConstant {
    pub fn new(
        included_values: impl IntoIterator<Item = impl Into<Ident>>,
        set: impl Into<Ident>,
    ) -> Self {
        let vals = included_values.into_iter().map(Into::into);

        let mut unique_vals = Vec::new();
        for val in vals {
            if !unique_vals.contains(&val) {
                unique_vals.push(val);
            }
        }

        SetConstant {
            included_values: unique_vals,
            set: set.into(),
        }
    }
}

impl fmt::Display for SetConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('[')?;

        let mut first = true;
        for val in &self.included_values {
            if !first {
                f.write_str(", ")?;
                first = false;
            }
            write!(f, "{}", val)?;
        }

        f.write_char(']')
    }
}
