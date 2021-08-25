use std::{convert::{TryFrom, TryInto}, fmt};

use num_bigint::BigUint;
use num_traits::Zero;

use crate::{Prog, pair};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum IndexV2 {
    Int(BigUint),
    Name(String),
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
        IndexV2::Int(value)
    }
}

impl From<u8> for IndexV2 {
    fn from(value: u8) -> Self {
        IndexV2::Int(value.into())
    }
}

impl From<String> for IndexV2 {
    fn from(value: String) -> Self {
        IndexV2::Name(value)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ValueV2 {
    Int(BigUint),
    Tuple(Box<ValueV2>, Box<ValueV2>),
    Prog(Prog),
}

impl ValueV2 {
    pub fn is_zero(&self) -> bool {
        match self {
            ValueV2::Int(num) => num.is_zero(),
            ValueV2::Tuple(left, right) => left.is_zero() && right.is_zero(),
            ValueV2::Prog(_) => todo!(),
        }
    }
}

impl fmt::Display for ValueV2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let num = BigUint::try_from(self.clone()).map_err(|_| std::fmt::Error)?;
        write!(f, "{}", num)
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
            },
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
            },
            ValueV2::Prog(prog) => Ok(prog),
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
