use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::*;

use std::{iter::Peekable, ops::Range};

fn parse_recursive(mut tokens: &mut Peekable<impl Iterator<Item = Token>>, file_id: usize) -> Result<Vec<Inst>, Diagnostic<usize>> {
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

    let parse_tuple = |tokens: &mut Peekable<_>, initial: Token| -> Result<_, Diagnostic<usize>> {
        let open_span = initial.span;

        let (left, left_span) = match diag_unwarp(tokens.next(), file_id, open_span.clone())? {
            Token { kind: TokenKind::Var(var), span } => (var, span),
            t => {
                return Err(
                    Diagnostic::error()
                        .with_message("Expected variable in tuple expression")
                        .with_labels(vec![
                            Label::primary(file_id, t.span).with_message("invalid token here"),
                        ])
                );
            }
        };

        let _ = match diag_unwarp(tokens.next(), file_id, open_span.start..left_span.end)? {
            Token{ kind: TokenKind::Comma, span } => span,
            t => return Err(
                Diagnostic::error()
                    .with_message("Expected comma between tuple elements")
                    .with_labels(vec![
                        Label::primary(file_id, t.span).with_message("invalid token here"),
                    ])
            ),
        };

        let (right, _) = match diag_unwarp(tokens.next(), file_id, open_span.clone())? {
            Token { kind: TokenKind::Var(var), span } => (var, span),
            t => {
                return Err(
                    Diagnostic::error()
                        .with_message("Expected variable in tuple expression")
                        .with_labels(vec![
                            Label::primary(file_id, t.span).with_message("invalid token here"),
                        ])
                );
            }
        };

        let close_span = match diag_unwarp(tokens.next(), file_id, open_span.start..left_span.end)? {
            Token{ kind: TokenKind::RightParen, span } => span,
            t => return Err(
                Diagnostic::error()
                    .with_message("Expected closing tuple delimiter")
                    .with_labels(vec![
                        Label::primary(file_id, t.span).with_message("invalid token here"),
                        Label::secondary(file_id, open_span).with_message("tuple starts here"),
                    ])
            ),
        };

        Ok((left, right, open_span.start..close_span.end))
    };

    let mut out = Vec::new();
    while let Some(token) = tokens.peek() {
        #[cfg(not(test))]
        out.push(Inst::CodePoint(token.span.start));

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
                    t @ Token { kind: TokenKind::LeftParen, .. } => {
                        let (left, right, _) = parse_tuple(&mut tokens, t)?;
                        out.push(Inst::Merge { target, left, right });
                        continue;
                    }
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

                let open = match tokens.peek() {
                    Some(&Token { kind: TokenKind::Plus | TokenKind::Minus, .. }) => {
                        let op = tokens.next().unwrap();

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
                    Some(&Token { kind: TokenKind::LeftParen, .. }) => tokens.next().unwrap(),
                    _ => {
                        out.push(Inst::Copy { target, source: left });
                        continue;
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

                out.push(Inst::Call { target, function: left, input  });
            }

            TokenKind::LeftParen => {
                let left_paren = tokens.next().unwrap();
                let (left, right, tuple_span) = parse_tuple(&mut tokens, left_paren)?;

                let eq_span = match diag_unwarp(tokens.next(), file_id, tuple_span.clone())? {
                    Token{ kind: TokenKind::Eq, span } => span,
                    t => return Err(
                        Diagnostic::error()
                            .with_message("Expected operator `:=` after tuple expression")
                            .with_labels(vec![
                                Label::primary(file_id, t.span).with_message("invalid token here"),
                                Label::secondary(file_id, tuple_span).with_message("tuple here"),
                            ])
                    ),
                };

                let (source, _) = match diag_unwarp(tokens.next(), file_id, tuple_span.start..eq_span.end)? {
                    Token { kind: TokenKind::Var(var), span } => (var, span),
                    t => {
                        return Err(
                            Diagnostic::error()
                                .with_message("Expected variable after assignment operator")
                                .with_labels(vec![
                                    Label::primary(file_id, t.span).with_message("invalid token here"),
                                    Label::secondary(file_id, eq_span).with_message("assignment operator here"),
                                ])
                        );
                    }
                };

                out.push(Inst::Split { left, right, source });
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
                let for_span = tokens.next().unwrap().span;

                let (num, num_span) = match diag_unwarp(tokens.next(), file_id, for_span.clone())? {
                    Token { kind: TokenKind::Var(var), span } => (var, span),
                    t => {
                        return Err(
                            Diagnostic::error()
                                .with_message("Expected variable after `for` keyword")
                                .with_labels(vec![
                                    Label::primary(file_id, t.span).with_message("invalid token here"),
                                    Label::secondary(file_id, for_span.clone()).with_message("for keyword here"),
                                ])
                        );
                    }
                };

                let do_token = diag_unwarp(tokens.next(), file_id, for_span.start..num_span.end)?;
                if do_token.kind != TokenKind::Do {
                    return Err(
                        Diagnostic::error()
                            .with_message("Expected start of code block")
                            .with_labels(vec![
                                Label::primary(file_id, do_token.span).with_message("invalid token here"),
                                Label::secondary(file_id, for_span).with_message("for loop starts here"),
                            ])
                    );
                }

                let inner = parse_recursive(&mut tokens, file_id)?;

                let od_token = diag_unwarp(tokens.next(), file_id, for_span.start..do_token.span.end)?;
                if od_token.kind != TokenKind::Od {
                    return Err(
                        Diagnostic::error()
                            .with_message("Wrong or missing code block delimiter")
                            .with_labels(vec![
                                Label::primary(file_id, od_token.span).with_message("invalid token here"),
                                Label::secondary(file_id, do_token.span).with_message("code block starts here"),
                            ])
                    );
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

                let do_token = diag_unwarp(tokens.next(), file_id, fn_span.start..target_span.end)?;
                if do_token.kind != TokenKind::Do {
                    return Err(
                        Diagnostic::error()
                            .with_message("Function name must be followed by a code block")
                            .with_labels(vec![
                                Label::primary(file_id, do_token.span).with_message("invalid token here"),
                                Label::secondary(file_id, fn_span).with_message("function definition starts here"),
                            ])
                    );
                }

                let inst = parse_recursive(&mut tokens, file_id)?;

                let od_token = diag_unwarp(tokens.next(), file_id, fn_span.start..target_span.end)?;
                if od_token.kind != TokenKind::Od {
                    return Err(
                        Diagnostic::error()
                            .with_message("Wrong or missing code block delimiter")
                            .with_labels(vec![
                                Label::primary(file_id, od_token.span).with_message("invalid token here"),
                                Label::secondary(file_id, do_token.span).with_message("code block starts here"),
                            ])
                    );
                }

                let sub = Prog { inst };
                let value = ValueV2::Prog(sub);
                out.push(Inst::Set { target, value });
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

    #[test]
    fn merge_and_split() {
        let tokens = vec![
            Token::no_span(TokenKind::Var(2_u8.into())),
            Token::no_span(TokenKind::Eq),
            Token::no_span(TokenKind::LeftParen),
            Token::no_span(TokenKind::Var(0_u8.into())),
            Token::no_span(TokenKind::Comma),
            Token::no_span(TokenKind::Var(1_u8.into())),
            Token::no_span(TokenKind::RightParen),
            Token::no_span(TokenKind::Semicolon),

            Token::no_span(TokenKind::LeftParen),
            Token::no_span(TokenKind::Var(0_u8.into())),
            Token::no_span(TokenKind::Comma),
            Token::no_span(TokenKind::Var(1_u8.into())),
            Token::no_span(TokenKind::RightParen),
            Token::no_span(TokenKind::Eq),
            Token::no_span(TokenKind::Var(2_u8.into())),
            Token::no_span(TokenKind::Semicolon),
        ];

        let result = parse(tokens, 0).unwrap().inst;
        let expected = vec![
            Inst::Merge { target: 2_u8.into(), left: 0_u8.into(), right: 1_u8.into() },
            Inst::Split { left: 0_u8.into(), right: 1_u8.into(), source: 2_u8.into() },
        ];

        assert_eq!(result, expected);
    }
}
