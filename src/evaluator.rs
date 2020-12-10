use crate::*;
use num_bigint::BigUint;

pub struct Evaluator {
    pub work: Vec<Inst>,
    pub state: Vec<BigUint>,
}

impl Evaluator {
    #[must_use]
    pub fn new(prog: Prog) -> Self {
        let num_states = prog.highest_index() + 1;
        let state = vec![BigUint::from(0_u8); num_states];
        let mut work = prog.inst;
        work.reverse();
        Self { state, work }
    }

    pub fn step(&mut self) -> bool {
        let inst = match self.work.pop() {
            Some(inst) => inst,
            None => return false,
        };

        match inst.clone() {
            Inst::Add { target, left, right } => {
                let left = self.state[left].clone();
                let right = self.state[right].clone();
                self.state[target] = left + right;
            }
            Inst::Sub { target, left, right } => {
                let left = self.state[left].clone();
                let right = self.state[right].clone();
                self.state[target] = left - right;
            }
            Inst::Set { target, value } => {
                self.state[target] = value.clone();
            }
            Inst::Block { inner } => {
                self.work.extend(inner.into_iter().rev());
            }
            Inst::While { cond, inner } => {
                if self.state[cond] != 0_u8.into() {
                    self.work.extend(inner.into_iter().rev());
                    self.work.push(inst);
                }
            }
            Inst::For { num, inner } => {
                let mut iter = self.state[num].clone();
                while iter > 0_u8.into() {
                    self.work.extend(inner.clone().into_iter().rev());
                    iter = iter - BigUint::from(1_u8);
                }
            }
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
            Inst::Set { target: 1, value: 2_u8.into() },
            Inst::Set { target: 2, value: 3_u8.into() },
            Inst::For { num: 2, inner: vec![
                Inst::Add { target: 0, left: 0, right: 2 },
                Inst::Sub { target: 1, left: 0, right: 1 },
            ] },
        ] };

        let mut eval = Evaluator::new(prog);
        eval.run();

        assert_eq!(eval.state[0], 9_u8.into());
        assert_eq!(eval.state[1], 4_u8.into());
        assert_eq!(eval.state[2], 3_u8.into());
    }
}
