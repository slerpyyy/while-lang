use std::{collections::HashMap, convert::{TryFrom, TryInto}};

use num_bigint::BigUint;
use num_traits::Zero;

use crate::*;

#[derive(Debug)]
struct Frame {
    pub work: Vec<Inst>,
    pub state: HashMap<IndexV2, ValueV2>,
    pub target: Option<IndexV2>,
}

pub struct Evaluator {
    stack: Vec<Frame>,
    pub position: usize,
}

impl Evaluator {
    #[must_use]
    pub fn new(prog: Prog) -> Self {
        let mut work = prog.inst;
        work.reverse();

        let base = Frame { work, state: HashMap::new(), target: None };
        Self { stack: vec![base], position: 0 }
    }

    pub fn step(&mut self) -> bool {
        let frame = self.stack.last_mut().unwrap();

        let inst = loop {
            match frame.work.pop() {
                Some(Inst::CodePoint(k)) => self.position = k,
                Some(inst) => break inst,

                None => return if let Some(target) = frame.target.take() {
                    let x0 = IndexV2::Int(0_u8.into());
                    let value = frame.state.remove(&x0).unwrap_or_default();

                    let _ = self.stack.pop();

                    let top = self.stack.last_mut().unwrap();
                    top.state.insert(target, value);
                    true
                } else {
                    false
                },
            }
        };

        match inst {
            Inst::Add { target, left, right } => {
                let left: BigUint = frame.state.get(&left).cloned().unwrap_or_default().try_into().unwrap();
                let right: BigUint = frame.state.get(&right).cloned().unwrap_or_default().try_into().unwrap();
                frame.state.insert(target, ValueV2::Int(left + right));
            }
            Inst::Sub { target, left, right } => {
                let left: BigUint = frame.state.get(&left).cloned().unwrap_or_default().try_into().unwrap();
                let right: BigUint = frame.state.get(&right).cloned().unwrap_or_default().try_into().unwrap();
                let value = if right < left { left - right } else { 0_u8.into() };
                frame.state.insert(target, ValueV2::Int(value));
            }
            Inst::Set { target, value } => {
                frame.state.insert(target, value);
            }
            Inst::Copy { target, source } => {
                let value = frame.state.get(&source).cloned().unwrap_or_default();
                frame.state.insert(target, value);
            }
            Inst::Merge { target, left, right } => {
                let tuple = ValueV2::Tuple(
                    Box::new(frame.state.get(&left).cloned().unwrap_or_default()),
                    Box::new(frame.state.get(&right).cloned().unwrap_or_default()),
                );
                frame.state.insert(target, tuple);
            }
            Inst::Split { left, right, source } => {
                let value = frame.state.get(&source).cloned().unwrap_or_default();
                let (left_value, right_value) = value.try_into().unwrap();
                frame.state.insert(left, left_value);
                frame.state.insert(right, right_value);
            }
            Inst::Block { inner } => {
                frame.work.extend(inner.into_iter().rev());
            }
            Inst::While { ref cond, ref inner } => {
                if frame.state.get(cond).filter(|n| !n.is_zero()).is_some() {
                    frame.work.push(inst.clone());
                    frame.work.extend(inner.clone().into_iter().rev());
                }
            }
            Inst::For { num, inner } => {
                if let Some(num) = frame.state.get(&num).cloned() {
                    let mut num = BigUint::try_from(num).unwrap();
                    while !num.is_zero() {
                        frame.work.extend(inner.clone().into_iter().rev());
                        num -= 1_u8;
                    }
                }
            }
            Inst::Call { target, function, input } => {
                let function = frame.state.get(&function).cloned().unwrap_or_default();
                let input = frame.state.get(&input).cloned().unwrap_or_default();

                let sub_routine: Prog = function.try_into().unwrap();
                let mut work = sub_routine.inst;
                work.reverse();

                let mut state = frame.state.clone();
                let x0 = IndexV2::Int(0_u8.into());
                state.insert(x0, input);

                let frame = Frame {
                    state, work, target: Some(target)
                };

                self.stack.push(frame);
            },
            Inst::CodePoint(_) => unreachable!(),
        }

        true
    }

    pub fn state(&self) -> &HashMap<IndexV2, ValueV2> {
        &self.stack.last().unwrap().state
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
        for (key, value) in self.state() {
            println!("    {} => {}", key, value);
        }
        println!("}}\n");

        println!("{}", &self.stack.len());
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

        let mut eval = Evaluator::new(prog);
        eval.run();

        assert_eq!(eval.state()[&0_u8.into()], 9_u8.into());
        assert_eq!(eval.state()[&1_u8.into()], 4_u8.into());
        assert_eq!(eval.state()[&2_u8.into()], 3_u8.into());
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
        let mut eval = Evaluator::new(prog);
        eval.run();

        assert_eq!(eval.state()[&0_u8.into()], 9_u8.into());
        assert_eq!(eval.state()[&1_u8.into()], 4_u8.into());
        assert_eq!(eval.state()[&2_u8.into()], 3_u8.into());
    }
}
