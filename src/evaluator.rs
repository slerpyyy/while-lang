use std::{borrow::Cow, collections::HashMap, convert::{TryFrom, TryInto}};

use num_bigint::BigUint;
use num_traits::Zero;

use crate::*;

#[derive(Debug)]
pub struct Frame<'prog> {
    pub work: Vec<Cow<'prog, Inst>>,
    pub state: HashMap<IndexV2, ValueV2>,
    pub target: Option<IndexV2>,
}

pub struct Evaluator<'prog> {
    pub stack: Vec<Frame<'prog>>,
    pub base: Frame<'prog>,
    pub position: usize,
}

impl<'prog> Evaluator<'prog> {
    #[must_use]
    pub fn new(prog: &'prog Prog) -> Self {
        let work = prog.inst.iter().rev().map(Cow::Borrowed).collect();
        let base = Frame { work, state: HashMap::new(), target: None };
        Self { stack: Vec::new(), base, position: 0 }
    }

    pub fn step(&mut self) -> bool {
        let inst = loop {
            if let Some(inst) = self.base.work.pop() {
                if let Inst::CodePoint(k) = inst.as_ref() {
                    self.position = *k;
                    continue;
                }

                break inst
            } else {
                return if let Some(target) = self.base.target.take() {
                    let x0 = IndexV2::Int(0_u8.into());
                    let value = self.base.state.remove(&x0).unwrap_or_default();

                    self.base = self.stack.pop().unwrap();
                    self.base.state.insert(target, value);
                    true
                } else {
                    false
                }
            }
        };

        match inst.as_ref() {
            Inst::Add { target, left, right } => {
                let left: BigUint = self.base.state.get(&left).cloned().unwrap_or_default().try_into().unwrap();
                let right: BigUint = self.base.state.get(&right).cloned().unwrap_or_default().try_into().unwrap();
                self.base.state.insert(target.clone(), ValueV2::Int(left + right));
            }
            Inst::Sub { target, left, right } => {
                let left: BigUint = self.base.state.get(&left).cloned().unwrap_or_default().try_into().unwrap();
                let right: BigUint = self.base.state.get(&right).cloned().unwrap_or_default().try_into().unwrap();
                let value = if right < left { left - right } else { 0_u8.into() };
                self.base.state.insert(target.clone(), ValueV2::Int(value));
            }
            Inst::Set { target, value } => {
                self.base.state.insert(target.clone(), value.clone());
            }
            Inst::Copy { target, source } => {
                let value = self.base.state.get(&source).cloned().unwrap_or_default();
                self.base.state.insert(target.clone(), value);
            }
            Inst::Merge { target, left, right } => {
                let tuple = ValueV2::Tuple(
                    Box::new(self.base.state.get(&left).cloned().unwrap_or_default()),
                    Box::new(self.base.state.get(&right).cloned().unwrap_or_default()),
                );
                self.base.state.insert(target.clone(), tuple);
            }
            Inst::Split { left, right, source } => {
                let value = self.base.state.get(&source).cloned().unwrap_or_default();
                let (left_value, right_value) = value.try_into().unwrap();
                self.base.state.insert(left.clone(), left_value);
                self.base.state.insert(right.clone(), right_value);
            }
            Inst::Block { inner } => {
                self.base.work.extend(inner.clone().into_iter().rev().map(Cow::Owned));
            }
            Inst::While { ref cond, ref inner } => {
                if self.base.state.get(cond).filter(|n| !n.is_zero()).is_some() {
                    self.base.work.push(inst.to_owned());
                    self.base.work.extend(inner.clone().into_iter().rev().map(Cow::Owned));
                }
            }
            Inst::For { num, inner } => {
                if let Some(num) = self.base.state.get(&num).cloned() {
                    let mut num = BigUint::try_from(num).unwrap();
                    while !num.is_zero() {
                        self.base.work.extend(inner.clone().into_iter().rev().map(Cow::Owned));
                        num -= 1_u8;
                    }
                }
            }
            Inst::Call { target, function, input } => {
                let function = self.base.state.get(&function).cloned().unwrap_or_default();
                let input = self.base.state.get(&input).cloned().unwrap_or_default();

                let sub_routine: Prog = function.try_into().unwrap();
                let work = sub_routine.inst.into_iter().rev().map(Cow::Owned).collect();

                let mut state = self.base.state.clone();
                let x0 = IndexV2::Int(0_u8.into());
                state.insert(x0, input);

                let frame = Frame {
                    state, work, target: Some(target.clone())
                };

                self.stack.push(frame);
                std::mem::swap(&mut self.base, self.stack.last_mut().unwrap());
            },
            Inst::CodePoint(_) => unreachable!(),
        }

        true
    }

    pub fn run(&mut self) {
        while self.step() {}
    }

    pub fn debug_print(&self, code: &str, context: usize) {
        let curr_ln = code[..self.position].chars().filter(|&ch| ch == '\n').count();
        let first_ln = curr_ln.saturating_sub(context / 2);

        println!("\nCode Point:");
        for (k , line) in code.lines().enumerate().skip(first_ln).take(context) {
            let sep = match k == curr_ln {
                true => '>',
                false => '|',
            };
            println!("{:8} {} {}", k, sep, line);
        }

        println!("\nState:\n{{");
        for (key, value) in &self.base.state {
            println!("    {} => {}", key, value);
        }
        println!("}}\n");
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple_for_program() {
        let prog = Prog { inst: vec![
            Inst::Set { target: 1_u8.into(), value: 2_u8.into() },
            Inst::Set { target: 2_u8.into(), value: 3_u8.into() },
            Inst::For { num: 2_u8.into(), inner: vec![
                Inst::Add { target: 0_u8.into(), left: 0_u8.into(), right: 2_u8.into() },
                Inst::Sub { target: 1_u8.into(), left: 0_u8.into(), right: 1_u8.into() },
            ] },
        ] };

        let mut eval = Evaluator::new(&prog);
        eval.run();

        assert_eq!(eval.base.state[&0_u8.into()], 9_u8.into());
        assert_eq!(eval.base.state[&1_u8.into()], 4_u8.into());
        assert_eq!(eval.base.state[&2_u8.into()], 3_u8.into());
    }

    #[test]
    fn simple_while_program() {
        let prog = Prog { inst: vec![
            Inst::Set { target: 1_u8.into(), value: 2_u8.into() },
            Inst::Set { target: 2_u8.into(), value: 3_u8.into() },
            Inst::For { num: 2_u8.into(), inner: vec![
                Inst::Add { target: 0_u8.into(), left: 0_u8.into(), right: 2_u8.into() },
                Inst::Sub { target: 1_u8.into(), left: 0_u8.into(), right: 1_u8.into() },
            ] },
        ] };

        let prog = prog.for_to_while();
        let mut eval = Evaluator::new(&prog);
        eval.run();

        assert_eq!(eval.base.state[&0_u8.into()], 9_u8.into());
        assert_eq!(eval.base.state[&1_u8.into()], 4_u8.into());
        assert_eq!(eval.base.state[&2_u8.into()], 3_u8.into());
    }
}
