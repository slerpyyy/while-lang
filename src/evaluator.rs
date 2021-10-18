use std::{
    collections::HashMap,
    convert::{TryFrom, TryInto},
};

use num_bigint::BigUint;
use num_traits::Zero;

use crate::*;

#[derive(Debug, Clone)]
enum Repeat<T> {
    Once(T),
    Many(T, BigUint),
}

#[derive(Debug)]
pub struct Frame {
    work: Vec<Repeat<Inst>>,
    pub state: HashMap<IndexV2, ValueV2>,
    target: Option<IndexV2>,
}

pub struct Evaluator {
    pub stack: Vec<Frame>,
    pub base: Frame,
    pub position: usize,
}

impl Evaluator {
    #[must_use]
    pub fn new(prog: Prog) -> Self {
        let work = prog.inst.into_iter().map(Repeat::Once).rev().collect();

        let base = Frame {
            work,
            state: HashMap::new(),
            target: None,
        };

        Self {
            stack: Vec::new(),
            base,
            position: 0,
        }
    }

    pub fn step(&mut self) -> bool {
        let mut rep_inst = loop {
            if let Some(rep_inst) = self.base.work.pop() {
                if let Repeat::Many(_, n) = &rep_inst {
                    if n.is_zero() {
                        continue;
                    }
                }

                break rep_inst;
            } else {
                return if let Some(target) = self.base.target.take() {
                    let x0 = IndexV2::Int(0_u8.into());
                    let value = self.base.state.remove(&x0).unwrap_or_default();

                    self.base = self.stack.pop().unwrap();
                    self.base.state.insert(target, value);
                    true
                } else {
                    false
                };
            }
        };

        let inst = match rep_inst {
            Repeat::Once(x) => x,
            Repeat::Many(ref x, ref mut count) => {
                let inst = x.clone();
                *count -= 1_u8;
                self.base.work.push(rep_inst);
                inst
            }
        };

        self.position = inst.span().start;

        match inst {
            Inst::Add {
                target,
                left,
                right,
                ..
            } => {
                let left = self.base.state.get(&left).cloned().unwrap_or_default();
                let right = self.base.state.get(&right).cloned().unwrap_or_default();
                self.base.state.insert(target.clone(), left + right);
            }
            Inst::Sub {
                target,
                left,
                right,
                ..
            } => {
                let left = self.base.state.get(&left).cloned().unwrap_or_default();
                let right = self.base.state.get(&right).cloned().unwrap_or_default();
                self.base.state.insert(target.clone(), left - right);
            }
            Inst::Set { target, value, .. } => {
                self.base.state.insert(target.clone(), value.clone());
            }
            Inst::Copy { target, source, .. } => {
                let value = self.base.state.get(&source).cloned().unwrap_or_default();
                self.base.state.insert(target.clone(), value);
            }
            Inst::Merge {
                target,
                left,
                right,
                ..
            } => {
                let tuple = ValueV2::Tuple(
                    Box::new(self.base.state.get(&left).cloned().unwrap_or_default()),
                    Box::new(self.base.state.get(&right).cloned().unwrap_or_default()),
                );
                self.base.state.insert(target.clone(), tuple);
            }
            Inst::Split {
                left,
                right,
                source,
                ..
            } => {
                let value = self.base.state.get(&source).cloned().unwrap_or_default();
                let (left_value, right_value) = value.try_into().unwrap();
                self.base.state.insert(left.clone(), left_value);
                self.base.state.insert(right.clone(), right_value);
            }
            Inst::Block { inner, .. } => {
                self.base
                    .work
                    .extend(inner.into_iter().rev().map(Repeat::Once));
            }
            Inst::While {
                ref cond,
                ref inner,
                ..
            } => {
                if self.base.state.get(cond).filter(|n| !n.is_zero()).is_some() {
                    self.base.work.push(Repeat::Once(inst.clone()));
                    self.base
                        .work
                        .extend(inner.iter().rev().cloned().map(Repeat::Once));
                }
            }
            Inst::For { num, inner, span } => {
                if let Some(num) = self.base.state.get(&num).cloned() {
                    let mut num = BigUint::try_from(num).unwrap();

                    if num < BigUint::from(4_u8) {
                        while !num.is_zero() {
                            self.base
                                .work
                                .extend(inner.iter().rev().cloned().map(Repeat::Once));
                            num -= 1_u8;
                        }
                    } else {
                        self.base
                            .work
                            .push(Repeat::Many(Inst::Block { inner, span }, num));
                    }
                }
            }
            Inst::Call {
                target,
                function,
                input,
                ..
            } => {
                let function = self.base.state.get(&function).cloned().unwrap_or_default();
                let input = self.base.state.get(&input).cloned().unwrap_or_default();

                let sub_routine: Prog = function.try_into().unwrap();
                let work = sub_routine
                    .inst
                    .into_iter()
                    .rev()
                    .map(Repeat::Once)
                    .collect();

                let mut state = self.base.state.clone();
                let x0 = IndexV2::Int(0_u8.into());
                state.insert(x0, input);

                let frame = Frame {
                    state,
                    work,
                    target: Some(target.clone()),
                };

                self.stack.push(frame);
                std::mem::swap(&mut self.base, self.stack.last_mut().unwrap());
            }
        }

        true
    }

    pub fn run(&mut self) {
        while self.step() {}
    }

    pub fn debug_print(&self, code: &str, context: usize) {
        let curr_ln = code[..self.position]
            .chars()
            .filter(|&ch| ch == '\n')
            .count();
        let first_ln = curr_ln.saturating_sub(context / 2);

        println!("\nCode Point:");
        for (k, line) in code.lines().enumerate().skip(first_ln).take(context) {
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
        let prog = Prog {
            inst: vec![
                Inst::Set {
                    target: 1_u8.into(),
                    value: 2_u8.into(),
                    span: 0..0,
                },
                Inst::Set {
                    target: 2_u8.into(),
                    value: 3_u8.into(),
                    span: 0..0,
                },
                Inst::For {
                    num: 2_u8.into(),
                    inner: vec![
                        Inst::Add {
                            target: 0_u8.into(),
                            left: 0_u8.into(),
                            right: 2_u8.into(),
                            span: 0..0,
                        },
                        Inst::Sub {
                            target: 1_u8.into(),
                            left: 0_u8.into(),
                            right: 1_u8.into(),
                            span: 0..0,
                        },
                    ],
                    span: 0..0,
                },
            ],
        };

        let mut eval = Evaluator::new(prog);
        eval.run();

        assert_eq!(eval.base.state[&0_u8.into()], 9_u8.into());
        assert_eq!(eval.base.state[&1_u8.into()], 4_u8.into());
        assert_eq!(eval.base.state[&2_u8.into()], 3_u8.into());
    }

    #[test]
    fn simple_while_program() {
        let prog = Prog {
            inst: vec![
                Inst::Set {
                    target: 1_u8.into(),
                    value: 2_u8.into(),
                    span: 0..0,
                },
                Inst::Set {
                    target: 2_u8.into(),
                    value: 3_u8.into(),
                    span: 0..0,
                },
                Inst::For {
                    num: 2_u8.into(),
                    inner: vec![
                        Inst::Add {
                            target: 0_u8.into(),
                            left: 0_u8.into(),
                            right: 2_u8.into(),
                            span: 0..0,
                        },
                        Inst::Sub {
                            target: 1_u8.into(),
                            left: 0_u8.into(),
                            right: 1_u8.into(),
                            span: 0..0,
                        },
                    ],
                    span: 0..0,
                },
            ],
        };

        let prog = prog.for_to_while();
        let mut eval = Evaluator::new(prog);
        eval.run();

        assert_eq!(eval.base.state[&0_u8.into()], 9_u8.into());
        assert_eq!(eval.base.state[&1_u8.into()], 4_u8.into());
        assert_eq!(eval.base.state[&2_u8.into()], 3_u8.into());
    }
}
