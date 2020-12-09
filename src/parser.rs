use crate::*;
use std::iter::Peekable;

fn parse_recursive(mut tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Vec<Inst> {
    let mut out = Vec::new();

    while let Some(token) = tokens.peek() {
        match token.kind {
            TokenKind::Var(target) => {
                tokens.next();

                if tokens.next().map(|t| t.kind) != Some(TokenKind::Eq) {
                    panic!()
                }

                let left = tokens.next().unwrap();
                if let TokenKind::Const(value) = left.kind {
                    out.push(Inst::Set { target, value });
                    continue;
                }

                let left = left.var().unwrap();
                let op = tokens.next().unwrap();
                let right = tokens.next().and_then(|t| t.var()).unwrap();

                match op.kind {
                    TokenKind::Plus => {
                        out.push(Inst::Add {
                            target,
                            left,
                            right,
                        });
                    }
                    TokenKind::Minus => {
                        out.push(Inst::Sub {
                            target,
                            left,
                            right,
                        });
                    }
                    _ => unreachable!(),
                }
            }

            TokenKind::Semicolon => {
                tokens.next();
            }

            TokenKind::LeftBracket => {
                tokens.next();

                let inner = parse_recursive(&mut tokens);
                if tokens.next().map(|t| t.kind) != Some(TokenKind::RightBracket) {
                    panic!()
                }

                let block = Inst::Block { inner };
                out.push(block);
            }

            TokenKind::While => {
                tokens.next();

                let cond = tokens.next().and_then(|t| t.var()).unwrap();

                if tokens.next().map(|t| t.kind) != Some(TokenKind::Neq)
                    || tokens.next().map(|t| t.kind) != Some(TokenKind::Const(0_u8.into()))
                    || tokens.next().map(|t| t.kind) != Some(TokenKind::Do)
                {
                    panic!()
                }

                let inner = parse_recursive(&mut tokens);

                if tokens.next().map(|t| t.kind) != Some(TokenKind::Od) {
                    panic!()
                }

                let block = Inst::While { cond, inner };
                out.push(block);
            }

            TokenKind::For => {
                tokens.next();

                let num = tokens.next().and_then(|t| t.var()).unwrap();

                if tokens.next().map(|t| t.kind) != Some(TokenKind::Do) {
                    panic!()
                }

                let inner = parse_recursive(&mut tokens);

                if tokens.next().map(|t| t.kind) != Some(TokenKind::Od) {
                    panic!()
                }

                let block = Inst::For { num, inner };
                out.push(block);
            }

            TokenKind::RightBracket | TokenKind::Od => break,

            _ => unreachable!(),
        }
    }

    out
}

pub fn parse(tokens: impl IntoIterator<Item = Token>) -> Prog {
    let mut iter = tokens.into_iter().peekable();
    let inst = parse_recursive(&mut iter);
    if iter.peek().is_some() {
        panic!()
    }

    Prog { inst }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn assign_simple() {
        let tokens = vec![
            Token::new(TokenKind::Var(2)),
            Token::new(TokenKind::Eq),
            Token::new(TokenKind::Const(5_u8.into())),
        ];

        let result = parse(tokens).inst;
        let expected = vec![Inst::Set {
            target: 2,
            value: 5_u8.into(),
        }];

        assert_eq!(result, expected);
    }

    #[test]
    fn assign_three_times() {
        let tokens = vec![
            Token::new(TokenKind::Var(0)),
            Token::new(TokenKind::Eq),
            Token::new(TokenKind::Const(8_u8.into())),
            Token::new(TokenKind::Semicolon),
            Token::new(TokenKind::LeftBracket),
            Token::new(TokenKind::Var(1)),
            Token::new(TokenKind::Eq),
            Token::new(TokenKind::Const(16_u8.into())),
            Token::new(TokenKind::Semicolon),
            Token::new(TokenKind::Var(2)),
            Token::new(TokenKind::Eq),
            Token::new(TokenKind::Const(32_u8.into())),
            Token::new(TokenKind::Semicolon),
            Token::new(TokenKind::RightBracket),
        ];

        let result = parse(tokens).inst;
        let expected = vec![
            Inst::Set {
                target: 0,
                value: 8_u8.into(),
            },
            Inst::Block {
                inner: vec![
                    Inst::Set {
                        target: 1,
                        value: 16_u8.into(),
                    },
                    Inst::Set {
                        target: 2,
                        value: 32_u8.into(),
                    },
                ],
            },
        ];

        assert_eq!(result, expected);
    }

    #[test]
    fn while_simple() {
        let tokens = vec![
            Token::new(TokenKind::While),
            Token::new(TokenKind::Var(2)),
            Token::new(TokenKind::Neq),
            Token::new(TokenKind::Const(0_u8.into())),
            Token::new(TokenKind::Do),
            Token::new(TokenKind::Var(2)),
            Token::new(TokenKind::Eq),
            Token::new(TokenKind::Const(0_u8.into())),
            Token::new(TokenKind::Od),
        ];

        let result = parse(tokens).inst;
        let expected = vec![Inst::While {
            cond: 2,
            inner: vec![Inst::Set {
                target: 2,
                value: 0_u8.into(),
            }],
        }];

        assert_eq!(result, expected);
    }

    #[test]
    fn for_simple() {
        let tokens = vec![
            Token::new(TokenKind::For),
            Token::new(TokenKind::Var(0)),
            Token::new(TokenKind::Do),
            Token::new(TokenKind::Var(2)),
            Token::new(TokenKind::Eq),
            Token::new(TokenKind::Var(2)),
            Token::new(TokenKind::Plus),
            Token::new(TokenKind::Var(1)),
            Token::new(TokenKind::Od),
        ];

        let result = parse(tokens).inst;
        let expected = vec![Inst::For {
            num: 0,
            inner: vec![Inst::Add {
                target: 2,
                left: 2,
                right: 1,
            }],
        }];

        assert_eq!(result, expected);
    }
}
