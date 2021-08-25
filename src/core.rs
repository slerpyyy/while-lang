use std::{convert::{TryFrom, TryInto}, unreachable};
use std::{fmt, collections::HashMap};
use num_bigint::BigUint;
use num_integer::Roots;
use num_traits::{NumOps, Zero};

pub type Value = BigUint;

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
pub enum Inst {
    Add {
        target: IndexV2,
        left: IndexV2,
        right: IndexV2,
    },
    Sub {
        target: IndexV2,
        left: IndexV2,
        right: IndexV2,
    },
    Set {
        target: IndexV2,
        value: Value,
    },
    Call {
        target: IndexV2,
        function: IndexV2,
        input: IndexV2,
    },
    Block {
        inner: Vec<Inst>,
    },
    While {
        cond: IndexV2,
        inner: Vec<Inst>,
    },
    For {
        num: IndexV2,
        inner: Vec<Inst>,
    },
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Prog {
    pub inst: Vec<Inst>,
}

impl Prog {
    #[must_use]
    pub fn highest_index(&self) -> BigUint {
        fn check(value: &IndexV2, curr: &mut BigUint) {
            if let IndexV2::Int(num) = value {
                if num > curr {
                    *curr = num.clone();
                }
            }
        }

        fn recurse(inst_vec: &[Inst], highest: &mut BigUint) {
            for inst in inst_vec {
                match inst {
                    Inst::Set { target, .. } => {
                        check(target, highest);
                    }
                    Inst::Add {target, left, right} |
                    Inst::Sub {target, left, right}  => {
                        check(target, highest);
                        check(left, highest);
                        check(right, highest);
                    }
                    Inst::Block { inner } => {
                        recurse(inner, highest);
                    }
                    Inst::While {cond, inner} => {
                        check(cond, highest);
                        recurse(inner, highest);
                    }
                    Inst::For {num, inner} => {
                        check(num, highest);
                        recurse(inner, highest);
                    }
                    Inst::Call {target, function, input} => {
                        check(target, highest);
                        check(function, highest);
                        check(input, highest);
                    }
                }
            }
        }

        let mut highest = BigUint::zero();
        recurse(&self.inst, &mut highest);
        highest
    }

    #[must_use]
    pub fn reindex_vars(mut self) -> Self {
        fn reindex_single(
            var: &mut IndexV2,
            map: &mut HashMap::<IndexV2, IndexV2>,
            next_available: &mut BigUint
        ) {
            *var = map.get(var).cloned().unwrap_or_else(|| {
                let new_var = IndexV2::Int(next_available.clone());
                map.insert(var.clone(), new_var.clone());
                *next_available += 1_u8;
                new_var
            });
        }

        fn recurse(
            inst_vec: &mut Vec<Inst>,
            map: &mut HashMap::<IndexV2, IndexV2>,
            next_available: &mut BigUint
        ) {
            for inst in inst_vec {
                match inst {
                    Inst::Set { target, .. } => {
                        reindex_single(target, map, next_available);
                    }
                    Inst::Add { target, left, right } |
                    Inst::Sub { target, left, right } => {
                        reindex_single(target, map, next_available);
                        reindex_single(left, map, next_available);
                        reindex_single(right, map, next_available);
                    }
                    Inst::Block { inner } => {
                        recurse(inner, map, next_available);
                    }
                    Inst::While { cond, inner } => {
                        reindex_single(cond, map, next_available);
                        recurse(inner, map, next_available);
                    }
                    Inst::For { num, inner } => {
                        reindex_single(num, map, next_available);
                        recurse(inner, map, next_available);
                    }
                    Inst::Call { target, function, input } => {
                        reindex_single(target, map, next_available);
                        reindex_single(function, map, next_available);
                        reindex_single(input, map, next_available);
                    },
                }
            }
        }

        let mut next_available = BigUint::zero();
        let mut map = HashMap::new();
        map.insert(IndexV2::Int(BigUint::zero()), IndexV2::Int(BigUint::zero()));
        recurse(&mut self.inst, &mut map, &mut next_available);
        self
    }

    #[must_use]
    pub fn inline_blocks(mut self) -> Self {
        fn recurse(inst_vec: Vec<Inst>) -> Vec<Inst> {
            inst_vec.into_iter().flat_map(|inst| match inst {
                Inst::Block { inner } => recurse(inner),
                Inst::While { cond, inner } => {
                    vec![Inst::While {cond, inner: recurse(inner)}]
                }
                Inst::For { num, inner } => {
                    vec![Inst::For {num, inner: recurse(inner)}]
                }
                s => vec![s],
            }).collect()
        }

        self.inst = recurse(self.inst);
        self
    }

    #[must_use]
    pub fn translate_while(mut self) -> Self {
        fn recurse(inst_vec: &mut Vec<Inst>, reserved: &BigUint, next_available: &mut BigUint) {
            for inst in inst_vec {
                match inst {
                    Inst::For {num, inner} => {
                        recurse(inner, reserved, next_available);

                        inner.extend(vec![
                            Inst::Set {target: IndexV2::Int(reserved.clone()), value: 1_u8.into()},
                            Inst::Sub {target: IndexV2::Int(next_available.clone()), left: IndexV2::Int(next_available.clone()), right: IndexV2::Int(reserved.clone())},
                        ]);

                        *inst = Inst::Block {inner: vec![
                            Inst::Set {target: IndexV2::Int(reserved.clone()), value: 0_u8.into()},
                            Inst::Add {target: IndexV2::Int(next_available.clone()), left: num.clone(), right: IndexV2::Int(reserved.clone())},
                            Inst::While {cond: IndexV2::Int(next_available.clone()), inner: inner.clone()}
                        ]};

                        *next_available += 1_u8;
                    }
                    Inst::Block {inner} |
                    Inst::While {inner, ..} => {
                        recurse(inner, reserved, next_available)
                    }
                    _ => (),
                }
            }
        }

        let reserved = self.highest_index() + 1_u8;
        let mut next_available = reserved.clone() + 1_u8;
        recurse(&mut self.inst, &reserved, &mut next_available);
        self
    }

    #[must_use]
    pub fn translate_for(self) -> Self {
        unimplemented!()
    }
}

impl fmt::Display for Prog {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[inline]
        fn fmt_indent(f: &mut fmt::Formatter<'_>, indent: u32) -> fmt::Result {
            for _ in 0..indent { write!(f, "  ")? }
            Ok(())
        }

        #[inline]
        fn fmt_recurse(f: &mut fmt::Formatter<'_>, inst_vec: &[Inst], indent: u32) -> fmt::Result {
            for inst in inst_vec {
                match inst {
                    Inst::Add { target, left, right } => {
                        fmt_indent(f, indent)?;
                        writeln!(f, "x{} := x{} + x{};", target, left, right)?;
                    }
                    Inst::Sub { target, left, right } => {
                        fmt_indent(f, indent)?;
                        writeln!(f, "x{} := x{} - x{};", target, left, right)?;
                    }
                    Inst::Set { target, value } => {
                        fmt_indent(f, indent)?;
                        writeln!(f, "x{} := {};", target, value)?;
                    }
                    Inst::Block { inner } => {
                        fmt_indent(f, indent)?;
                        writeln!(f, "[")?;
                        fmt_recurse(f, inner, indent + 1)?;
                        fmt_indent(f, indent)?;
                        writeln!(f, "]")?;
                    }
                    Inst::While { cond, inner } => {
                        fmt_indent(f, indent)?;
                        writeln!(f, "while x{} /= 0 do", cond)?;
                        fmt_recurse(f, inner, indent + 1)?;
                        fmt_indent(f, indent)?;
                        writeln!(f, "od")?;
                    }
                    Inst::For { num, inner } => {
                        fmt_indent(f, indent)?;
                        writeln!(f, "for x{} do", num)?;
                        fmt_recurse(f, inner, indent + 1)?;
                        fmt_indent(f, indent)?;
                        writeln!(f, "od")?;
                    }
                    Inst::Call { target, function, input } => {
                        fmt_indent(f, indent)?;
                        writeln!(f, "x{} := {}({});", target, function, input)?;
                    },
                }
            }
            Ok(())
        }

        fmt_recurse(f, &self.inst, 0)
    }
}

#[inline]
#[must_use]
pub fn pair<T>(x: T, y: T) -> T
where T: Clone + From<u8> + NumOps {
    let sum: T = x + y.clone();
    let tri: T = sum.clone() * (sum + T::from(1)) / T::from(2);
    tri + y
}

#[inline]
#[must_use]
pub fn pair_inv<T>(z: T) -> (T, T)
where T: Clone + From<u8> + NumOps + Roots {
    let z_off: T = T::from(8) * z.clone() + T::one();
    let sum: T = (Roots::sqrt(&z_off) - T::one()) / T::from(2);
    let tri: T = sum.clone() * (sum.clone() + T::one()) / T::from(2);
    let y: T = z - tri;
    let x: T = sum - y.clone();
    (x, y)
}

impl TryFrom<BigUint> for Prog {
    type Error = ();

    fn try_from(num: BigUint) -> Result<Self, Self::Error> {
        fn recurse(num: BigUint) -> Result<Inst, ()> {
            let kind: u8 = (num.clone() % 5_u8).try_into().map_err(|_| ())?;
            let num: BigUint = num / 5_u8;
            let inst: Inst = match kind {
                0..=2 => {
                    let (i, c) = pair_inv(num);
                    if kind == 2 {
                        Inst::Set { target: i.into(), value: c }
                    } else {
                        let (j, k) = pair_inv(c);
                        if kind == 0 {
                            Inst::Add { target: i.into(), left: j.into(), right: k.into() }
                        } else {
                            Inst::Sub { target: i.into(), left: j.into(), right: k.into() }
                        }
                    }
                },
                3 => {
                    let (i, p) = pair_inv(num);
                    Inst::While { cond: i.into(), inner: vec![recurse(p)?] }
                },
                4 => {
                    let (p1, p2) = pair_inv(num);
                    Inst::Block { inner: vec![recurse(p1)?, recurse(p2)? ] }
                },
                _ => unreachable!(),
            };

            Ok(inst)
        }

        Ok(Self {inst: vec![recurse(num)?]})
    }
}

impl TryFrom<Prog> for BigUint {
    type Error = ();

    fn try_from(prog: Prog) -> Result<Self, Self::Error> {
        fn recurse(inst_vec: &[Inst]) -> Result<BigUint, ()> {
            let (head, tail) = inst_vec.split_first().ok_or(())?;
            let head = match head {
                Inst::Add { target, left, right } => {
                    let num: BigUint = pair(left.clone().try_into()?, right.clone().try_into()?);
                    let num: BigUint = pair(target.clone().try_into()?, num);
                    BigUint::from(5_u8) * num
                }
                Inst::Sub { target, left, right } => {
                    let num: BigUint = pair(left.clone().try_into()?, right.clone().try_into()?);
                    let num: BigUint = pair(target.clone().try_into()?, num);
                    BigUint::from(5_u8) * num + BigUint::from(1_u8)
                }
                Inst::Set { target, value } => {
                    let num: BigUint = pair(target.clone().try_into()?, value.clone());
                    BigUint::from(5_u8) * num + BigUint::from(2_u8)
                }
                Inst::While { cond, inner } => {
                    let num: BigUint = pair(cond.clone().try_into()?, recurse(inner)?);
                    BigUint::from(5_u8) * num + BigUint::from(3_u8)
                }
                Inst::Block { inner } => {
                    recurse(inner)?
                }
                _ => return Err(()),
            };

            Ok(match tail {
                &[] => head,
                tail => BigUint::from(5_u8) * pair(head, recurse(tail)?) + BigUint::from(4_u8)
            })
        }

        recurse(&prog.inst)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn pair_id_small() {
        for n in 0..1000 {
            let (x, y) = pair_inv(n);
            let z = pair(x, y);

            assert_eq!(n, z, "in={}, x={}, y={}, out={}", n, x, y, z)
        }
    }

    #[test]
    fn pair_id_big() {
        let mut n = BigUint::from(std::u32::MAX);
        for _ in 0..1000 {
            let (x, y) = pair_inv(n.clone());
            let z = pair(x.clone(), y.clone());

            assert_eq!(&n, &z, "in={}, x={}, y={}, out={}", &n, &x, &y, &z);
            n = n + BigUint::from(1_u8);
        }
    }

    #[test]
    fn to_string_simple() {
        let prog = Prog { inst: vec![
            Inst::Set { target: 0.into(), value: 8_u8.into() },
            Inst::Block { inner: vec![
                Inst::Set { target: 1.into(), value: 16_u8.into() },
                Inst::Set { target: 2.into(), value: 32_u8.into() },
            ] },
            Inst::While { cond: 2.into(), inner: vec![
                Inst::Add { target: 3.into(), left: 1.into(), right: 3.into() },
                Inst::Sub { target: 2.into(), left: 2.into(), right: 0.into() },
                Inst::For { num: 0.into(), inner: vec![
                    Inst::Set { target: 3.into(), value: 1_u8.into() }
                ] },
            ] },
        ] };

        let actual = prog.to_string();
        let expected = "\
x0 := 8;
[
  x1 := 16;
  x2 := 32;
]
while x2 /= 0 do
  x3 := x1 + x3;
  x2 := x2 - x0;
  for x0 do
    x3 := 1;
  od
od
";

        assert_eq!(expected, actual);
    }

    #[test]
    fn highest_index_reindex_simple() {
        let prog = Prog { inst: vec![
            Inst::Set { target: 8_u8.into(), value: 8_u8.into() },
            Inst::Block { inner: vec![
                Inst::Set { target: 5_u8.into(), value: 16_u8.into() },
                Inst::Set { target: 2_u8.into(), value: 32_u8.into() },
            ] },
            Inst::While { cond: 2_u8.into(), inner: vec![
                Inst::Add { target: 5_u8.into(), left: 5_u8.into(), right: 8_u8.into() },
                Inst::Sub { target: 2_u8.into(), left: 2_u8.into(), right: 0_u8.into() },
                Inst::For { num: 0_u8.into(), inner: vec![
                    Inst::Set { target: 7_u8.into(), value: 1_u8.into() }
                ] },
            ] },
        ] };

        assert_eq!(prog.highest_index(), 8_u8.into());

        let prog = prog.reindex_vars();
        assert_eq!(prog.highest_index(), 4_u8.into());
    }

    #[test]
    fn translate_while_simple() {
        let prog = Prog { inst: vec![
            Inst::For { num: 2_u8.into(), inner: vec![
                Inst::Add { target: 5_u8.into(), left: 5_u8.into(), right: 8_u8.into() },
                Inst::For { num: 0_u8.into(), inner: vec![
                    Inst::Set { target: 7_u8.into(), value: 1_u8.into() }
                ] },
            ] },
        ] };
        println!("{}", prog);

        let prog = prog.translate_while();
        println!("{}", prog);
    }

    #[test]
    fn from_big_int_simple() {
        let num: BigUint = 13499359_u64.into();
        let prog: Prog = num.try_into().unwrap();

        let expected = "\
[
  while x22 /= 0 do
    [
      x0 := x0 + x0;
      x0 := x0 + x0;
    ]
  od
  x10 := x1 + x1;
]
";

        assert_eq!(prog.to_string(), expected);
    }

    #[test]
    fn from_big_int_huge() {
        let num = BigUint::parse_bytes(b"\
6918533271165862244800453842255214768368549267336031513389449453398\
0092400299925515080206483860328907198635248416013381235319140639272\
6897754342224678926067874361569070021552220521388010615008170881095\
6243268016923493158803722982452545915649955777420013923183320489359\
8918915193172285526551142584434955736640059829711203281957250872373\
1400892288209387853483996163716217775929", 10).unwrap();
        let prog: Prog = num.try_into().unwrap();

        let result = prog.inline_blocks().to_string();
        let expected = "\
x2 := 8;
x3 := 16;
x4 := 32;
while x4 /= 0 do
  x10 := x3 + x10;
  x4 := x4 - x2;
  while x2 /= 0 do
    x10 := 1;
  od
od
";

        assert_eq!(result, expected);
    }

    #[test]
    fn from_into_consistency() {
        let expected = BigUint::parse_bytes(
            b"6353471964769560710236650382363524229281885364863",
            10,
        ).unwrap();

        let num = &expected;
        let prog = Prog::try_from(num.clone()).unwrap();
        let num = BigUint::try_from(prog).unwrap();
        let prog = Prog::try_from(num.clone()).unwrap();
        let num = BigUint::try_from(prog).unwrap();
        let prog = Prog::try_from(num.clone()).unwrap();
        let num = BigUint::try_from(prog).unwrap();

        assert_eq!(expected, num);
    }
}
