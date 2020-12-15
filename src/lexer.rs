use crate::*;
use std::{iter::Peekable, ops::{Add, Mul}, panic};
use source_span::{DEFAULT_METRICS, Position, Span};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Var(Index),
    Const(Value),
    Eq,
    Neq,
    Semicolon,
    Plus,
    Minus,
    LeftBracket,
    RightBracket,
    While,
    For,
    Do,
    Od,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    #[must_use]
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    #[must_use]
    pub fn no_span(kind: TokenKind) -> Self {
        Self { kind, span: Span::default() }
    }

    #[must_use]
    pub const fn var(&self) -> Option<Index> {
        match self.kind {
            TokenKind::Var(k) => Some(k),
            _ => None,
        }
    }
}

#[must_use]
fn parse_number<T>(iter: &mut Peekable<impl Iterator<Item = char>>, pos: &mut Position) -> T
where
    T: From<u8> + Add<Output = T> + Mul<Output = T>,
{
    let mut num = T::from(0);
    while let Some(&c @ '0'..='9') = iter.peek() {
        num = num * T::from(10) + T::from(c as u8 - b'0');
        pos.shift(c, &DEFAULT_METRICS);
        iter.next();
    }
    num
}

#[must_use]
pub fn lex(code: &str) -> Vec<Token> {
    let mut out = Vec::new();
    let mut iter = code.chars().peekable();
    let mut pos = Position::new(0, 0);

    while let Some(&c) = iter.peek() {
        let start = pos;
        let kind = match c {
            '0'..='9' => {
                let num = parse_number(&mut iter, &mut pos);
                TokenKind::Const(num)
            }
            'x' => {
                pos.shift(c, &DEFAULT_METRICS);
                iter.next();
                let num = parse_number(&mut iter, &mut pos);
                TokenKind::Var(num)
            }
            ':' => {
                pos.shift(c, &DEFAULT_METRICS);
                iter.next();
                if iter.next() == Some('=') {
                    pos.shift('=', &DEFAULT_METRICS);
                    TokenKind::Eq
                } else {
                    panic!()
                }
            }
            '/' => {
                pos.shift(c, &DEFAULT_METRICS);
                iter.next();
                if iter.next() == Some('=') {
                    pos.shift('=', &DEFAULT_METRICS);
                    TokenKind::Neq
                } else {
                    panic!()
                }
            }
            ';' => {
                pos.shift(c, &DEFAULT_METRICS);
                iter.next();
                TokenKind::Semicolon
            }
            '+' => {
                pos.shift(c, &DEFAULT_METRICS);
                iter.next();
                TokenKind::Plus
            }
            '-' => {
                pos.shift(c, &DEFAULT_METRICS);
                iter.next();
                TokenKind::Minus
            }
            '[' => {
                pos.shift(c, &DEFAULT_METRICS);
                iter.next();
                TokenKind::LeftBracket
            }
            ']' => {
                pos.shift(c, &DEFAULT_METRICS);
                iter.next();
                TokenKind::RightBracket
            }
            'w' => {
                iter.next();
                if iter.next() == Some('h')
                && iter.next() == Some('i')
                && iter.next() == Some('l')
                && iter.next() == Some('e')
                {
                    pos.shift('w', &DEFAULT_METRICS);
                    pos.shift('h', &DEFAULT_METRICS);
                    pos.shift('i', &DEFAULT_METRICS);
                    pos.shift('l', &DEFAULT_METRICS);
                    pos.shift('e', &DEFAULT_METRICS);
                    TokenKind::While
                } else {
                    panic!()
                }
            }
            'f' => {
                iter.next();
                if iter.next() == Some('o')
                && iter.next() == Some('r') {
                    pos.shift('f', &DEFAULT_METRICS);
                    pos.shift('o', &DEFAULT_METRICS);
                    pos.shift('r', &DEFAULT_METRICS);
                    TokenKind::For
                } else {
                    panic!()
                }
            }
            'd' => {
                iter.next();
                if iter.next() == Some('o') {
                    pos.shift('d', &DEFAULT_METRICS);
                    pos.shift('o', &DEFAULT_METRICS);
                    TokenKind::Do
                } else {
                    panic!()
                }
            }
            'o' => {
                iter.next();
                if iter.next() == Some('d') {
                    pos.shift('o', &DEFAULT_METRICS);
                    pos.shift('d', &DEFAULT_METRICS);
                    TokenKind::Od
                } else {
                    panic!()
                }
            }
            ' ' | '\t' | '\n' => {
                pos.shift(c, &DEFAULT_METRICS);
                iter.next();
                continue;
            }
            _ => panic!(),
        };

        let span = Span::new(start, pos, Position::end());
        out.push(Token::new(kind, span));
    }

    out
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn small_program() {
        let code = "\
x0 := 32;
x16 := 25;
";

        let tokens = lex(code);
        let expected: Vec<Token> = vec![
            Token::new(TokenKind::Var(0), Span::new(Position::new(0, 0), Position::new(0, 2), Position::end())),
            Token::new(TokenKind::Eq, Span::new(Position::new(0, 3), Position::new(0, 5), Position::end())),
            Token::new(TokenKind::Const(32_u8.into()), Span::new(Position::new(0, 6), Position::new(0, 8), Position::end())),
            Token::new(TokenKind::Semicolon, Span::new(Position::new(0, 8), Position::new(0, 9), Position::end())),
            Token::new(TokenKind::Var(16), Span::new(Position::new(1, 0), Position::new(1, 3), Position::end())),
            Token::new(TokenKind::Eq, Span::new(Position::new(1, 4), Position::new(1, 6), Position::end())),
            Token::new(TokenKind::Const(25_u8.into()), Span::new(Position::new(1, 7), Position::new(1, 9), Position::end())),
            Token::new(TokenKind::Semicolon, Span::new(Position::new(1, 9), Position::new(1, 10), Position::end())),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn simple_program_token_kind() {
        let code = "[[x0 := 32; [x1:=25;x2:=x0+x1]]; while x2 /= 0 do x2 := x2 - x1 od]";
        let tokens = lex(code).into_iter().map(|t| t.kind).collect::<Vec<_>>();
        let expected = vec![
            TokenKind::LeftBracket,
            TokenKind::LeftBracket,
            TokenKind::Var(0),
            TokenKind::Eq,
            TokenKind::Const(32_u8.into()),
            TokenKind::Semicolon,
            TokenKind::LeftBracket,
            TokenKind::Var(1),
            TokenKind::Eq,
            TokenKind::Const(25_u8.into()),
            TokenKind::Semicolon,
            TokenKind::Var(2),
            TokenKind::Eq,
            TokenKind::Var(0),
            TokenKind::Plus,
            TokenKind::Var(1),
            TokenKind::RightBracket,
            TokenKind::RightBracket,
            TokenKind::Semicolon,
            TokenKind::While,
            TokenKind::Var(2),
            TokenKind::Neq,
            TokenKind::Const(0_u8.into()),
            TokenKind::Do,
            TokenKind::Var(2),
            TokenKind::Eq,
            TokenKind::Var(2),
            TokenKind::Minus,
            TokenKind::Var(1),
            TokenKind::Od,
            TokenKind::RightBracket,
        ];

        assert_eq!(tokens, expected);
    }
}
