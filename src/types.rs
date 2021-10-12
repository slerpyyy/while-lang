use std::{
    convert::{TryFrom, TryInto},
    fmt,
};

use num_bigint::BigUint;
use num_traits::Zero;

use crate::{pair, pair_inv, Inst, Prog};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum IndexV2 {
    Int(BigUint),
    Name(String),
}

impl IndexV2 {
    #[must_use]
    pub fn is_zero(&self) -> bool {
        matches!(self, &IndexV2::Int(ref num) if num.is_zero())
    }
}

impl fmt::Display for IndexV2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IndexV2::Int(num) => write!(f, "x{}", num),
            IndexV2::Name(name) => write!(f, "{}", name),
        }
    }
}

impl TryFrom<IndexV2> for BigUint {
    type Error = ();

    fn try_from(value: IndexV2) -> Result<Self, Self::Error> {
        match value {
            IndexV2::Int(num) => Ok(num),
            IndexV2::Name(_) => Err(()),
        }
    }
}

impl From<BigUint> for IndexV2 {
    fn from(value: BigUint) -> Self {
        Self::Int(value)
    }
}

impl From<u8> for IndexV2 {
    fn from(value: u8) -> Self {
        Self::Int(value.into())
    }
}

impl From<String> for IndexV2 {
    fn from(value: String) -> Self {
        Self::Name(value)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ValueV2 {
    Int(BigUint),
    Tuple(Box<Self>, Box<Self>),
    Prog(Prog),
}

impl ValueV2 {
    #[must_use]
    pub fn is_zero(&self) -> bool {
        match self {
            ValueV2::Int(num) => num.is_zero(),
            ValueV2::Tuple(left, right) => left.is_zero() && right.is_zero(),
            ValueV2::Prog(prog) => {
                matches!(prog.inst.as_slice(), &[Inst::Add { .. }])
                    && BigUint::try_from(prog.clone()) == Ok(0_u8.into())
            }
        }
    }
}

impl std::ops::Add for ValueV2 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.is_zero() {
            return rhs;
        }

        if rhs.is_zero() {
            return self;
        }

        let left: BigUint = self.try_into().unwrap();
        let right: BigUint = rhs.try_into().unwrap();
        Self::Int(left + right)
    }
}

impl std::ops::Sub for ValueV2 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        if rhs.is_zero() {
            return self;
        }

        let left: BigUint = self.try_into().unwrap();
        let right: BigUint = rhs.try_into().unwrap();

        if left <= right {
            return 0_u8.into();
        }

        Self::Int(left - right)
    }
}

impl fmt::Display for ValueV2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueV2::Int(num) => write!(f, "{}", num),
            ValueV2::Tuple(left, right) => write!(f, "({}, {})", left, right),
            ValueV2::Prog(_) => write!(f, "<fn def>"),
        }
    }
}

impl Default for ValueV2 {
    fn default() -> Self {
        0_u8.into()
    }
}

impl TryFrom<ValueV2> for BigUint {
    type Error = ();

    fn try_from(value: ValueV2) -> Result<Self, Self::Error> {
        match value {
            ValueV2::Int(num) => Ok(num),
            ValueV2::Tuple(left, right) => {
                let left = Self::try_from(*left)?;
                let right = Self::try_from(*right)?;
                Ok(pair(left, right))
            }
            ValueV2::Prog(prog) => Self::try_from(prog),
        }
    }
}

impl TryFrom<ValueV2> for Prog {
    type Error = ();

    fn try_from(value: ValueV2) -> Result<Self, Self::Error> {
        match value {
            ValueV2::Int(num) => num.try_into(),
            v @ ValueV2::Tuple(_, _) => {
                let num: BigUint = v.try_into()?;
                num.try_into()
            }
            ValueV2::Prog(prog) => Ok(prog),
        }
    }
}

impl TryFrom<ValueV2> for (ValueV2, ValueV2) {
    type Error = ();

    fn try_from(value: ValueV2) -> Result<Self, Self::Error> {
        match value {
            ValueV2::Int(num) => {
                let (left, right) = pair_inv(num);
                Ok((ValueV2::Int(left), ValueV2::Int(right)))
            }
            ValueV2::Tuple(left, right) => Ok((*left, *right)),
            ValueV2::Prog(prog) => {
                let num: BigUint = prog.try_into()?;
                let (left, right) = pair_inv(num);
                Ok((ValueV2::Int(left), ValueV2::Int(right)))
            }
        }
    }
}

impl From<BigUint> for ValueV2 {
    fn from(value: BigUint) -> Self {
        Self::Int(value)
    }
}

impl From<u8> for ValueV2 {
    fn from(value: u8) -> Self {
        Self::Int(value.into())
    }
}
