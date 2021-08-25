use std::{collections::HashMap, convert::TryInto};

use crate::*;
use num_traits::Zero;

pub struct Evaluator {
    pub work: Vec<Inst>,
    pub state: HashMap<IndexV2, Value>,
}

impl Evaluator {
    #[must_use]
    pub fn new(prog: Prog) -> Self {
        let mut work = prog.inst;
        work.reverse();
        Self { state: HashMap::new(), work }
    }

    pub fn step(&mut self) -> bool {
        let inst = match self.work.pop() {
            Some(inst) => inst,
            None => return false,
        };

        match inst {
            Inst::Add { target, left, right } => {
                let left = self.state.get(&left).cloned().unwrap_or_default();
                let right = self.state.get(&right).cloned().unwrap_or_default();
                self.state.insert(target, left + right);
            }
            Inst::Sub { target, left, right } => {
                let left = self.state.get(&left).cloned().unwrap_or_default();
                let right = self.state.get(&right).cloned().unwrap_or_default();
                self.state.insert(target, left - right);
            }
            Inst::Set { target, value } => {
                self.state.insert(target, value);
            }
            Inst::Block { inner } => {
                self.work.extend(inner.into_iter().rev());
            }
            Inst::While { ref cond, ref inner } => {
                if self.state.get(cond).filter(|n| !n.is_zero()).is_some() {
                    self.work.push(inst.clone());
                    self.work.extend(inner.clone().into_iter().rev());
                }
            }
            Inst::For { num, inner } => {
                if let Some(mut num) = self.state.get(&num).cloned() {
                    while !num.is_zero() {
                        self.work.extend(inner.clone().into_iter().rev());
                        num -= 1_u8;
                    }
                }
            }
            Inst::Call { target, function, input } => {
                let function = self.state.get(&function).cloned().unwrap_or_default();
                let input = self.state.get(&input).cloned().unwrap_or_default();

                let sub_routine: Prog = function.try_into().unwrap();
                let mut eval = Evaluator::new(sub_routine);
                let x0 = IndexV2::Int(0_u8.into());
                eval.state.insert(x0.clone(), input);

                eval.run();
                let output = eval.state.remove(&x0).unwrap();
                self.state.insert(target, output);
            },
        }

        true
    }

    pub fn run(&mut self) {
        while self.step() {}
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

        assert_eq!(eval.state[&0_u8.into()], 9_u8.into());
        assert_eq!(eval.state[&1_u8.into()], 4_u8.into());
        assert_eq!(eval.state[&2_u8.into()], 3_u8.into());
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

        let prog = prog.translate_while();
        let mut eval = Evaluator::new(prog);
        eval.run();

        assert_eq!(eval.state[&0_u8.into()], 9_u8.into());
        assert_eq!(eval.state[&1_u8.into()], 4_u8.into());
        assert_eq!(eval.state[&2_u8.into()], 3_u8.into());
    }
}
