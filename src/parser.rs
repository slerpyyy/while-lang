use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::*;

use std::{iter::Peekable, ops::Range};

fn diag_unwarp<T>(
    option: Option<T>,
    file_id: usize,
    span: Range<usize>,
) -> Result<T, Diagnostic<usize>> {
    match option {
        Some(value) => Ok(value),
        None => Err(Diagnostic::error()
            .with_message("Incomplete statement")
            .with_labels(vec![Label::primary(file_id, span)])),
    }
}

fn parse_tuple(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    file_id: usize,
    initial: Token,
) -> Result<(IndexV2, IndexV2, Range<usize>), Diagnostic<usize>> {
    let open_span = initial.span;

    let (left, left_span) = match diag_unwarp(tokens.next(), file_id, open_span.clone())? {
        Token {
            kind: TokenKind::Var(var),
            span,
            ..
        } => (var, span),
        t => {
            return Err(Diagnostic::error()
                .with_message("Expected variable in tuple expression")
                .with_labels(vec![
                    Label::primary(file_id, t.span).with_message("invalid token here")
                ]));
        }
    };

    let _ = match diag_unwarp(tokens.next(), file_id, open_span.start..left_span.end)? {
        Token {
            kind: TokenKind::Comma,
            span,
            ..
        } => span,
        t => {
            return Err(Diagnostic::error()
                .with_message("Expected comma between tuple elements")
                .with_labels(vec![
                    Label::primary(file_id, t.span).with_message("invalid token here")
                ]))
        }
    };

    let (right, _) = match diag_unwarp(tokens.next(), file_id, open_span.clone())? {
        Token {
            kind: TokenKind::Var(var),
            span,
            ..
        } => (var, span),
        t => {
            return Err(Diagnostic::error()
                .with_message("Expected variable in tuple expression")
                .with_labels(vec![
                    Label::primary(file_id, t.span).with_message("invalid token here")
                ]));
        }
    };

    let close_span = match diag_unwarp(tokens.next(), file_id, open_span.start..left_span.end)? {
        Token {
            kind: TokenKind::RightParen,
            span,
            ..
        } => span,
        t => {
            return Err(Diagnostic::error()
                .with_message("Expected closing tuple delimiter")
                .with_labels(vec![
                    Label::primary(file_id, t.span).with_message("invalid token here"),
                    Label::secondary(file_id, open_span).with_message("tuple starts here"),
                ]))
        }
    };

    Ok((left, right, open_span.start..close_span.end))
}

fn parse_stuff(
    mut tokens: &mut Peekable<impl Iterator<Item = Token>>,
    target: IndexV2,
    //file_id: usize,
) -> Result<Inst, Diagnostic<usize>> {
    let var = tokens.next().unwrap();

    let eq_token = match diag_unwarp(tokens.next(), var.file_id, var.span.clone())? {
        token if token.kind != TokenKind::Eq => {
            return Err(Diagnostic::error()
                .with_message("Expected operator `:=`")
                .with_labels(vec![
                    Label::primary(token.file_id, token.span).with_message("invalid token here"),
                    Label::secondary(var.file_id, var.span).with_message("statement starts here"),
                ]));
        }
        t => t,
    };

    let (left, left_span) = match diag_unwarp(
        tokens.next(),
        var.file_id,
        var.span.start..eq_token.span.end,
    )? {
        Token {
            kind: TokenKind::Const(value),
            span,
            ..
        } => {
            return Ok(Inst::Set {
                target,
                value,
                span: var.span.start..span.end,
            });
        }
        t @ Token {
            kind: TokenKind::LeftParen,
            ..
        } => {
            let (left, right, tuple_span) = parse_tuple(&mut tokens, t.file_id, t)?;
            return Ok(Inst::Merge {
                target,
                left,
                right,
                span: var.span.start..tuple_span.end,
            });
        }
        Token {
            kind: TokenKind::Var(var),
            span,
            ..
        } => (var, span),
        t => {
            return Err(Diagnostic::error()
                .with_message("Operator `:=` must be followed by an expression")
                .with_labels(vec![
                    Label::primary(t.file_id, t.span).with_message("invalid token here"),
                    Label::secondary(eq_token.file_id, eq_token.span)
                        .with_message("assignment operator here"),
                ]));
        }
    };

    let open = match tokens.peek() {
        Some(&Token {
            kind: TokenKind::Plus | TokenKind::Minus,
            ..
        }) => {
            let op = tokens.next().unwrap();

            let (right, right_span) =
                match diag_unwarp(tokens.next(), var.file_id, var.span.start..op.span.end)? {
                    Token {
                        kind: TokenKind::Var(var),
                        span,
                        ..
                    } => (var, span),
                    t => {
                        return Err(Diagnostic::error()
                            .with_message("Numeric operator must be followed by a variable")
                            .with_labels(vec![
                                Label::primary(t.file_id, t.span)
                                    .with_message("invalid token here"),
                                Label::secondary(eq_token.file_id, eq_token.span)
                                    .with_message("assignment operator here"),
                                Label::secondary(op.file_id, op.span)
                                    .with_message("numeric operator here"),
                            ]));
                    }
                };

            let span = var.span.start..right_span.end;
            let inst = match op.kind {
                TokenKind::Plus => Inst::Add {
                    target,
                    left,
                    right,
                    span,
                },
                TokenKind::Minus => Inst::Sub {
                    target,
                    left,
                    right,
                    span,
                },
                _ => unreachable!(),
            };

            return Ok(inst);
        }
        Some(&Token {
            kind: TokenKind::LeftParen,
            ..
        }) => tokens.next().unwrap(),
        _ => {
            return Ok(Inst::Copy {
                target,
                source: left,
                span: var.span.start..left_span.end,
            });
        }
    };

    let (input, input_span) =
        match diag_unwarp(tokens.next(), var.file_id, var.span.start..open.span.end)? {
            Token {
                kind: TokenKind::Var(var),
                span,
                ..
            } => (var, span),
            t => {
                return Err(Diagnostic::error()
                    .with_message("A function must be called with a variable as input")
                    .with_labels(vec![
                        Label::primary(t.file_id, t.span).with_message("invalid token here"),
                        Label::secondary(eq_token.file_id, eq_token.span)
                            .with_message("assignment operator here"),
                        Label::secondary(var.file_id, left_span).with_message("function name here"),
                    ]));
            }
        };

    let closing = diag_unwarp(tokens.next(), var.file_id, var.span.start..input_span.end)?;
    if closing.kind != TokenKind::RightParen {
        return Err(Diagnostic::error()
            .with_message("Left parentheses without a friend")
            .with_labels(vec![Label::primary(open.file_id, open.span)]));
    }

    Ok(Inst::Call {
        target,
        function: left,
        input,
        span: var.span.start..closing.span.end,
    })
}

fn parse_split(
    mut tokens: &mut Peekable<impl Iterator<Item = Token>>,
    file_id: usize,
) -> Result<Inst, Diagnostic<usize>> {
    let left_paren = tokens.next().unwrap();
    let (left, right, tuple_span) = parse_tuple(&mut tokens, file_id, left_paren)?;

    let eq_span = match diag_unwarp(tokens.next(), file_id, tuple_span.clone())? {
        Token {
            kind: TokenKind::Eq,
            span,
            ..
        } => span,
        t => {
            return Err(Diagnostic::error()
                .with_message("Expected operator `:=` after tuple expression")
                .with_labels(vec![
                    Label::primary(file_id, t.span).with_message("invalid token here"),
                    Label::secondary(file_id, tuple_span).with_message("tuple here"),
                ]))
        }
    };

    let (source, source_span) =
        match diag_unwarp(tokens.next(), file_id, tuple_span.start..eq_span.end)? {
            Token {
                kind: TokenKind::Var(var),
                span,
                ..
            } => (var, span),
            t => {
                return Err(Diagnostic::error()
                    .with_message("Expected variable after assignment operator")
                    .with_labels(vec![
                        Label::primary(file_id, t.span).with_message("invalid token here"),
                        Label::secondary(file_id, eq_span).with_message("assignment operator here"),
                    ]));
            }
        };

    Ok(Inst::Split {
        left,
        right,
        source,
        span: tuple_span.start..source_span.end,
    })
}

fn parse_block<I: Iterator<Item = Token>>(
    mut tokens: &mut Peekable<I>,
    file_id: usize,
    mut parse_inner: impl FnMut(&mut Peekable<I>) -> Vec<Inst>,
) -> Result<Inst, Diagnostic<usize>> {
    let open = tokens.next().unwrap();
    let inner = parse_inner(&mut tokens);

    let close = tokens.next();
    if close.as_ref().map(|t| &t.kind) != Some(&TokenKind::RightBracket) {
        return Err(Diagnostic::error()
            .with_message("Left bracket without a friend")
            .with_labels(vec![
                Label::primary(file_id, open.span).with_message("code block starts here"),
                //Label::primary(file_id, last.span.clone()).with_message("...and continues until here"),
            ]));
    }

    let close = close.unwrap();
    let block = Inst::Block {
        inner,
        span: open.span.start..close.span.end,
    };
    Ok(block)
}

fn parse_while<I: Iterator<Item = Token>>(
    mut tokens: &mut Peekable<I>,
    file_id: usize,
    mut parse_inner: impl FnMut(&mut Peekable<I>) -> Vec<Inst>,
) -> Result<Inst, Diagnostic<usize>> {
    let while_span = tokens.next().unwrap().span;

    let (cond, cond_span) = match diag_unwarp(tokens.next(), file_id, while_span.clone())? {
        Token {
            kind: TokenKind::Var(var),
            span,
            ..
        } => (var, span),
        t => {
            return Err(Diagnostic::error()
                .with_message("Expected variable after `while` keyword")
                .with_labels(vec![
                    Label::primary(file_id, t.span).with_message("invalid token here"),
                    Label::secondary(file_id, while_span.clone())
                        .with_message("while keyword here"),
                ]));
        }
    };

    let neq = diag_unwarp(tokens.next(), file_id, while_span.start..cond_span.end)?;
    let zero = diag_unwarp(tokens.next(), file_id, while_span.start..neq.span.end)?;
    let do_token = diag_unwarp(tokens.next(), file_id, while_span.start..zero.span.end)?;

    if neq.kind != TokenKind::Neq
        || zero.kind != TokenKind::Const(0_u8.into())
        || do_token.kind != TokenKind::Do
    {
        return Err(Diagnostic::error()
            .with_message("Head of `while` loop must match `while ?? /= 0 do`")
            .with_labels(vec![Label::primary(
                file_id,
                neq.span.start..do_token.span.end,
            )]));
    }

    let inner = parse_inner(&mut tokens);

    let od_token = diag_unwarp(tokens.next(), file_id, while_span.start..do_token.span.end)?;
    if od_token.kind != TokenKind::Od {
        return Err(Diagnostic::error()
            .with_message("Body of `while` loop is never closed")
            .with_labels(vec![
                Label::primary(file_id, do_token.span).with_message("loop body starts here")
            ]));
    }

    let block = Inst::While {
        cond,
        inner,
        span: while_span.start..od_token.span.end,
    };
    Ok(block)
}

fn parse_for<I: Iterator<Item = Token>>(
    mut tokens: &mut Peekable<I>,
    file_id: usize,
    mut parse_inner: impl FnMut(&mut Peekable<I>) -> Vec<Inst>,
) -> Result<Inst, Diagnostic<usize>> {
    let for_span = tokens.next().unwrap().span;

    let (num, num_span) = match diag_unwarp(tokens.next(), file_id, for_span.clone())? {
        Token {
            kind: TokenKind::Var(var),
            span,
            ..
        } => (var, span),
        t => {
            return Err(Diagnostic::error()
                .with_message("Expected variable after `for` keyword")
                .with_labels(vec![
                    Label::primary(file_id, t.span).with_message("invalid token here"),
                    Label::secondary(file_id, for_span.clone()).with_message("for keyword here"),
                ]));
        }
    };

    let do_token = diag_unwarp(tokens.next(), file_id, for_span.start..num_span.end)?;
    if do_token.kind != TokenKind::Do {
        return Err(Diagnostic::error()
            .with_message("Expected start of code block")
            .with_labels(vec![
                Label::primary(file_id, do_token.span).with_message("invalid token here"),
                Label::secondary(file_id, for_span).with_message("for loop starts here"),
            ]));
    }

    let inner = parse_inner(&mut tokens);

    let od_token = diag_unwarp(tokens.next(), file_id, for_span.start..do_token.span.end)?;
    if od_token.kind != TokenKind::Od {
        return Err(Diagnostic::error()
            .with_message("Wrong or missing code block delimiter")
            .with_labels(vec![
                Label::primary(file_id, od_token.span).with_message("invalid token here"),
                Label::secondary(file_id, do_token.span).with_message("code block starts here"),
            ]));
    }

    let block = Inst::For {
        num,
        inner,
        span: for_span.start..od_token.span.end,
    };
    Ok(block)
}

fn parse_function_definition<I: Iterator<Item = Token>>(
    mut tokens: &mut Peekable<I>,
    file_id: usize,
    mut parse_inner: impl FnMut(&mut Peekable<I>) -> Vec<Inst>,
) -> Result<Inst, Diagnostic<usize>> {
    let fn_span = tokens.next().unwrap().span;

    let (target, target_span) = match diag_unwarp(tokens.next(), file_id, fn_span.clone())? {
        Token {
            kind: TokenKind::Var(var),
            span,
            ..
        } => (var, span),
        t => {
            return Err(Diagnostic::error()
                .with_message("Function name must be a valid variable")
                .with_labels(vec![
                    Label::primary(file_id, t.span).with_message("invalid token here"),
                    Label::secondary(file_id, fn_span)
                        .with_message("function definition starts here"),
                ]));
        }
    };

    let do_token = diag_unwarp(tokens.next(), file_id, fn_span.start..target_span.end)?;
    if do_token.kind != TokenKind::Do {
        return Err(Diagnostic::error()
            .with_message("Function name must be followed by a code block")
            .with_labels(vec![
                Label::primary(file_id, do_token.span).with_message("invalid token here"),
                Label::secondary(file_id, fn_span).with_message("function definition starts here"),
            ]));
    }

    let inst = parse_inner(&mut tokens);

    let od_token = diag_unwarp(tokens.next(), file_id, fn_span.start..target_span.end)?;
    if od_token.kind != TokenKind::Od {
        return Err(Diagnostic::error()
            .with_message("Wrong or missing code block delimiter")
            .with_labels(vec![
                Label::primary(file_id, od_token.span).with_message("invalid token here"),
                Label::secondary(file_id, do_token.span).with_message("code block starts here"),
            ]));
    }

    let sub = Prog { inst };
    let value = ValueV2::Prog(sub);
    Ok(Inst::Set {
        target,
        value,
        span: fn_span.start..od_token.span.end,
    })
}

fn parse_recursive(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    file_id: usize,
) -> Result<Vec<Inst>, Vec<Diagnostic<usize>>> {
    let mut out = Vec::new();
    let mut errors = Vec::new();

    while let Some(token) = tokens.peek() {
        let result = match token.kind.clone() {
            TokenKind::Var(target) => Some(parse_stuff(tokens, target)),

            TokenKind::LeftParen => Some(parse_split(tokens, file_id)),

            TokenKind::Semicolon => {
                tokens.next();
                None
            }

            TokenKind::LeftBracket => {
                Some(parse_block(
                    tokens,
                    file_id,
                    |tokens| match parse_recursive(tokens, file_id) {
                        Ok(inst) => inst,
                        Err(err) => {
                            errors.extend(err);
                            Vec::new()
                        }
                    },
                ))
            }

            TokenKind::While => Some(parse_while(
                tokens,
                file_id,
                |tokens| match parse_recursive(tokens, file_id) {
                    Ok(inst) => inst,
                    Err(err) => {
                        errors.extend(err);
                        Vec::new()
                    }
                },
            )),

            TokenKind::For => Some(parse_for(tokens, file_id, |tokens| {
                match parse_recursive(tokens, file_id) {
                    Ok(inst) => inst,
                    Err(err) => {
                        errors.extend(err);
                        Vec::new()
                    }
                }
            })),

            TokenKind::Fn => {
                Some(parse_function_definition(
                    tokens,
                    file_id,
                    |tokens| match parse_recursive(tokens, file_id) {
                        Ok(inst) => inst,
                        Err(err) => {
                            errors.extend(err);
                            Vec::new()
                        }
                    },
                ))
            }

            TokenKind::RightBracket | TokenKind::Od => break,

            _ => {
                let token_span = tokens.next().unwrap().span;
                Some(Err(Diagnostic::error()
                    .with_message("Invalid beginning of statement")
                    .with_labels(vec![Label::primary(file_id, token_span)])))
            }
        };

        match result {
            Some(Ok(inst)) if errors.is_empty() => out.push(inst),

            Some(Err(err)) => {
                errors.push(err);

                // Try to get the parser into a position
                // where it can continue parsing
                while let Some(kind) = tokens.peek().map(|t| &t.kind) {
                    if matches!(
                        kind,
                        TokenKind::Semicolon | TokenKind::RightBracket | TokenKind::Od
                    ) {
                        break;
                    }

                    let _ = tokens.next();
                }
            }

            _ => (),
        }
    }

    match errors.is_empty() {
        true => Ok(out),
        false => Err(errors),
    }
}

pub fn parse(
    tokens: impl IntoIterator<Item = Token>,
    file_id: usize,
) -> Result<Prog, Vec<Diagnostic<usize>>> {
    let mut iter = tokens.into_iter().peekable();
    let mut result = parse_recursive(&mut iter, file_id);

    if let Some(trailing) = iter.next() {
        let err = Diagnostic::error()
            .with_message("Trailing token detected")
            .with_notes(vec!["This might be a bug in the interpreter".into()])
            .with_labels(vec![Label::primary(file_id, trailing.span)]);

        match &mut result {
            s @ Ok(_) => *s = Err(vec![err]),
            Err(vec) => vec.push(err),
        }
    }

    result.map(|inst| Prog { inst })
}

pub fn compile(code: &str, file_id: usize) -> Result<Prog, Vec<Diagnostic<usize>>> {
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
            span: 0..0,
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
            span: 0..0,
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
                span: 0..0,
            },
            Inst::Block {
                inner: vec![
                    Inst::Set {
                        target: 1_u8.into(),
                        value: 16_u8.into(),
                        span: 0..0,
                    },
                    Inst::Set {
                        target: 2_u8.into(),
                        value: 32_u8.into(),
                        span: 0..0,
                    },
                ],
                span: 0..0,
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
                span: 0..0,
            }],
            span: 0..0,
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
                span: 0..0,
            }],
            span: 0..0,
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
            Inst::Merge {
                target: 2_u8.into(),
                left: 0_u8.into(),
                right: 1_u8.into(),
                span: 0..0,
            },
            Inst::Split {
                left: 0_u8.into(),
                right: 1_u8.into(),
                source: 2_u8.into(),
                span: 0..0,
            },
        ];

        assert_eq!(result, expected);
    }
}
