use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::*;

use std::{convert::TryInto, iter::Peekable, ops::Range};

fn parse_recursive(mut tokens: &mut Peekable<impl Iterator<Item = Token>>, file_id: usize) -> Result<Vec<Inst>, Diagnostic<usize>> {
    let mut out = Vec::new();

    fn diag_unwarp<T>(option: Option<T>, file_id: usize, span: Range<usize>) -> Result<T, Diagnostic<usize>> {
        match option {
            Some(value) => Ok(value),
            None => Err(
                Diagnostic::error()
                    .with_message("Incomplete statement")
                    .with_labels(vec![Label::primary(file_id, span)])
            ),
        }
    }

    while let Some(token) = tokens.peek() {
        match token.kind.clone() {
            TokenKind::Var(target) => {
                let var = tokens.next().unwrap();

                let eq_token = match diag_unwarp(tokens.next(), file_id, var.span.clone())? {
                    token if token.kind != TokenKind::Eq => {
                        return Err(
                            Diagnostic::error()
                                .with_message("Expected operator `:=`")
                                .with_labels(vec![
                                    Label::primary(file_id, token.span).with_message("invalid token here"),
                                    Label::secondary(file_id, var.span).with_message("statement starts here"),
                                ])
                        );
                    },
                    t => t,
                };

                let (left, left_span) = match diag_unwarp(tokens.next(), file_id, var.span.start..eq_token.span.end)? {
                    Token { kind: TokenKind::Const(value), .. } => {
                        out.push(Inst::Set { target, value });
                        continue;
                    },
                    Token { kind: TokenKind::Var(var), span } => (var, span),
                    t => {
                        return Err(
                            Diagnostic::error()
                                .with_message("Operator `:=` must be followed by an expression")
                                .with_labels(vec![
                                    Label::primary(file_id, t.span).with_message("invalid token here"),
                                    Label::secondary(file_id, eq_token.span).with_message("assignment operator here"),
                                ])
                        );
                    }
                };

                let open = match diag_unwarp(tokens.next(), file_id, var.span.start..left_span.end)? {
                    op if matches!(op.kind, TokenKind::Plus | TokenKind::Minus) => {
                        let right = match diag_unwarp(tokens.next(), file_id, var.span.start..op.span.end)? {
                            Token { kind: TokenKind::Var(var), .. } => var,
                            t => {
                                return Err(
                                    Diagnostic::error()
                                        .with_message("Numeric operator must be followed by a variable")
                                        .with_labels(vec![
                                            Label::primary(file_id, t.span).with_message("invalid token here"),
                                            Label::secondary(file_id, eq_token.span).with_message("assignment operator here"),
                                            Label::secondary(file_id, op.span).with_message("numeric operator here"),
                                        ])
                                );
                            }
                        };

                        match op.kind {
                            TokenKind::Plus => out.push(Inst::Add { target, left, right }),
                            TokenKind::Minus => out.push(Inst::Sub { target, left, right }),
                            _ => unreachable!(),
                        }

                        continue;
                    },
                    t if matches!(t.kind, TokenKind::LeftParen) => t,
                    t => {
                        return Err(
                            Diagnostic::error()
                                .with_message("Expected either numeric operator or parentheses")
                                .with_labels(vec![
                                    Label::primary(file_id, t.span).with_message("invalid token here"),
                                    Label::secondary(file_id, eq_token.span).with_message("assignment operator here"),
                                ])
                        );
                    }
                };

                let input = match diag_unwarp(tokens.next(), file_id, var.span.start..open.span.end)? {
                    Token { kind: TokenKind::Var(var), .. } => var,
                    t => {
                        return Err(
                            Diagnostic::error()
                                .with_message("A function must be called with a variable as input")
                                .with_labels(vec![
                                    Label::primary(file_id, t.span).with_message("invalid token here"),
                                    Label::secondary(file_id, eq_token.span).with_message("assignment operator here"),
                                    Label::secondary(file_id, left_span).with_message("function name here"),
                                ])
                        );
                    }
                };

                if tokens.next().map(|t| t.kind) != Some(TokenKind::RightParen) {
                    return Err(
                        Diagnostic::error()
                            .with_message("Left parentheses without a friend")
                            .with_labels(vec![Label::primary(file_id, open.span)])
                    );
                }

                out.push(Inst::Call { target, function: left, input  })
            }

            TokenKind::Semicolon => {
                tokens.next();
            }

            TokenKind::LeftBracket => {
                let open = tokens.next().unwrap();
                let inner = parse_recursive(&mut tokens, file_id)?;

                if tokens.next().map(|t| t.kind) != Some(TokenKind::RightBracket) {
                    return Err(
                        Diagnostic::error()
                            .with_message("Left bracket without a friend")
                            .with_labels(vec![
                                Label::primary(file_id, open.span).with_message("code block starts here"),
                                //Label::primary(file_id, last.span.clone()).with_message("...and continues until here"),
                            ])
                    );
                }

                let block = Inst::Block { inner };
                out.push(block);
            }

            TokenKind::While => {
                let while_span = tokens.next().unwrap().span;

                let (cond, cond_span) = match diag_unwarp(tokens.next(), file_id, while_span.clone())? {
                    Token { kind: TokenKind::Var(var), span } => (var, span),
                    t => {
                        return Err(
                            Diagnostic::error()
                                .with_message("Expected variable after `while` keyword")
                                .with_labels(vec![
                                    Label::primary(file_id, t.span).with_message("invalid token here"),
                                    Label::secondary(file_id, while_span.clone()).with_message("while keyword here"),
                                ])
                        );
                    }
                };

                let neq = diag_unwarp(tokens.next(), file_id, while_span.start..cond_span.end)?;
                let zero = diag_unwarp(tokens.next(), file_id, while_span.start..neq.span.end)?;
                let open = diag_unwarp(tokens.next(), file_id, while_span.start..zero.span.end)?;

                if neq.kind != TokenKind::Neq || zero.kind != TokenKind::Const(0_u8.into()) || open.kind != TokenKind::Do {
                    return Err(
                        Diagnostic::error()
                            .with_message("Head of `while` loop must match `while ?? /= 0 do`")
                            .with_labels(vec![Label::primary(file_id, neq.span.start..open.span.end)])
                    );
                }

                let inner = parse_recursive(&mut tokens, file_id)?;

                if tokens.next().map(|t| t.kind) != Some(TokenKind::Od) {
                    return Err(
                        Diagnostic::error()
                            .with_message("Body of `while` loop is never closed")
                            .with_labels(vec![
                                Label::primary(file_id, open.span).with_message("loop body starts here"),
                            ])
                    );
                }

                let block = Inst::While { cond, inner };
                out.push(block);
            }

            TokenKind::For => {
                tokens.next();

                let num = tokens.next().and_then(|t| t.var().cloned()).unwrap();

                if tokens.next().map(|t| t.kind) != Some(TokenKind::Do) {
                    panic!()
                }

                let inner = parse_recursive(&mut tokens, file_id)?;

                if tokens.next().map(|t| t.kind) != Some(TokenKind::Od) {
                    panic!()
                }

                let block = Inst::For { num, inner };
                out.push(block);
            }

            TokenKind::Fn => {
                let fn_span = tokens.next().unwrap().span;

                let (target, target_span) = match diag_unwarp(tokens.next(), file_id, fn_span.clone())? {
                    Token { kind: TokenKind::Var(var), span } => (var, span),
                    t => {
                        return Err(
                            Diagnostic::error()
                                .with_message("Function name must be a valid variable")
                                .with_labels(vec![
                                    Label::primary(file_id, t.span).with_message("invalid token here"),
                                    Label::secondary(file_id, fn_span).with_message("function definition starts here"),
                                ])
                        );
                    }
                };

                if diag_unwarp(tokens.next(), file_id, fn_span.start..target_span.end)?.kind != TokenKind::Do {
                    panic!()
                }

                let inst = parse_recursive(&mut tokens, file_id)?;

                if diag_unwarp(tokens.next(), file_id, fn_span.start..target_span.end)?.kind != TokenKind::Od {
                    panic!()
                }

                let sub = Prog { inst };
                let value = sub.try_into().unwrap();
                out.push(Inst::Set { target, value })
            }

            TokenKind::RightBracket | TokenKind::Od => break,

            _ => {
                let token_span = tokens.next().unwrap().span;
                return Err(
                    Diagnostic::error()
                        .with_message("Invalid beginning of statement")
                        .with_labels(vec![Label::primary(file_id, token_span)])
                );
            },
        }
    }

    Ok(out)
}

pub fn parse(tokens: impl IntoIterator<Item = Token>, file_id: usize) -> Result<Prog, Diagnostic<usize>> {
    let mut iter = tokens.into_iter().peekable();
    let inst = parse_recursive(&mut iter, file_id)?;
    debug_assert!(iter.next().is_none());

    Ok(Prog { inst })
}

pub fn compile(code: &str, file_id: usize) -> Result<Prog, Diagnostic<usize>> {
    let tokens = lex(code, file_id)?;
    let prog = parse(tokens, file_id)?;
    Ok(prog)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn assign_simple() {
        let tokens = vec![
            Token::no_span(TokenKind::Var(2_u8.into())),
            Token::no_span(TokenKind::Eq),
            Token::no_span(TokenKind::Const(5_u8.into())),
        ];

        let result = parse(tokens, 0).unwrap().inst;
        let expected = vec![Inst::Set {
            target: 2_u8.into(),
            value: 5_u8.into(),
        }];

        assert_eq!(result, expected);
    }

    #[test]
    fn call_simple() {
        let tokens = vec![
            Token::no_span(TokenKind::Var(2_u8.into())),
            Token::no_span(TokenKind::Eq),
            Token::no_span(TokenKind::Var(3_u8.into())),
            Token::no_span(TokenKind::LeftParen),
            Token::no_span(TokenKind::Var(4_u8.into())),
            Token::no_span(TokenKind::RightParen),
        ];

        let result = parse(tokens, 0).unwrap().inst;
        let expected = vec![Inst::Call {
            target: 2_u8.into(),
            function: 3_u8.into(),
            input: 4_u8.into(),
        }];

        assert_eq!(result, expected);
    }

    #[test]
    fn assign_three_times() {
        let tokens = vec![
            Token::no_span(TokenKind::Var(0_u8.into())),
            Token::no_span(TokenKind::Eq),
            Token::no_span(TokenKind::Const(8_u8.into())),
            Token::no_span(TokenKind::Semicolon),
            Token::no_span(TokenKind::LeftBracket),
            Token::no_span(TokenKind::Var(1_u8.into())),
            Token::no_span(TokenKind::Eq),
            Token::no_span(TokenKind::Const(16_u8.into())),
            Token::no_span(TokenKind::Semicolon),
            Token::no_span(TokenKind::Var(2_u8.into())),
            Token::no_span(TokenKind::Eq),
            Token::no_span(TokenKind::Const(32_u8.into())),
            Token::no_span(TokenKind::Semicolon),
            Token::no_span(TokenKind::RightBracket),
        ];

        let result = parse(tokens, 0).unwrap().inst;
        let expected = vec![
            Inst::Set {
                target: 0_u8.into(),
                value: 8_u8.into(),
            },
            Inst::Block {
                inner: vec![
                    Inst::Set {
                        target: 1_u8.into(),
                        value: 16_u8.into(),
                    },
                    Inst::Set {
                        target: 2_u8.into(),
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
            Token::no_span(TokenKind::While),
            Token::no_span(TokenKind::Var(2_u8.into())),
            Token::no_span(TokenKind::Neq),
            Token::no_span(TokenKind::Const(0_u8.into())),
            Token::no_span(TokenKind::Do),
            Token::no_span(TokenKind::Var(2_u8.into())),
            Token::no_span(TokenKind::Eq),
            Token::no_span(TokenKind::Const(0_u8.into())),
            Token::no_span(TokenKind::Od),
        ];

        let result = parse(tokens, 0).unwrap().inst;
        let expected = vec![Inst::While {
            cond: 2_u8.into(),
            inner: vec![Inst::Set {
                target: 2_u8.into(),
                value: 0_u8.into(),
            }],
        }];

        assert_eq!(result, expected);
    }

    #[test]
    fn for_simple() {
        let tokens = vec![
            Token::no_span(TokenKind::For),
            Token::no_span(TokenKind::Var(0_u8.into())),
            Token::no_span(TokenKind::Do),
            Token::no_span(TokenKind::Var(2_u8.into())),
            Token::no_span(TokenKind::Eq),
            Token::no_span(TokenKind::Var(2_u8.into())),
            Token::no_span(TokenKind::Plus),
            Token::no_span(TokenKind::Var(1_u8.into())),
            Token::no_span(TokenKind::Od),
        ];

        let result = parse(tokens, 0).unwrap().inst;
        let expected = vec![Inst::For {
            num: 0_u8.into(),
            inner: vec![Inst::Add {
                target: 2_u8.into(),
                left: 2_u8.into(),
                right: 1_u8.into(),
            }],
        }];

        assert_eq!(result, expected);
    }
}
