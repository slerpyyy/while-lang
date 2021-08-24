use crate::*;
use std::{iter::Peekable, ops::{Add, Mul, Range}, panic};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Var(Value),
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
    pub span: Range<usize>,
}

impl Token {
    #[must_use]
    pub const fn new(kind: TokenKind, span: Range<usize>) -> Self {
        Self { kind, span }
    }

    #[must_use]
    pub fn no_span(kind: TokenKind) -> Self {
        Self { kind, span: 0..0 }
    }

    #[must_use]
    pub const fn var(&self) -> Option<&Value> {
        match self.kind {
            TokenKind::Var(ref k) => Some(k),
            _ => None,
        }
    }
}

#[must_use]
fn parse_number<T>(iter: &mut Peekable<impl Iterator<Item = char>>, pos: &mut usize) -> T
where
    T: From<u8> + Add<Output = T> + Mul<Output = T>,
{
    let mut num = T::from(0);
    while let Some(&c @ '0'..='9') = iter.peek() {
        num = num * T::from(10) + T::from(c as u8 - b'0');
        *pos += c.len_utf8();
        iter.next();
    }
    num
}

#[must_use]
pub fn lex(code: &str) -> Vec<Token> {
    let mut out = Vec::new();
    let mut iter = code.chars().peekable();
    let mut pos = 0;

    while let Some(&c) = iter.peek() {
        let start = pos;
        let kind = match c {
            '0'..='9' => {
                let num = parse_number(&mut iter, &mut pos);
                TokenKind::Const(num)
            }
            'x' => {
                pos += c.len_utf8();
                iter.next();
                let num = parse_number(&mut iter, &mut pos);
                TokenKind::Var(num)
            }
            ':' => {
                pos += c.len_utf8();
                iter.next();
                if iter.next() == Some('=') {
                    pos += '='.len_utf8();
                    TokenKind::Eq
                } else {
                    panic!()
                }
            }
            '/' => {
                pos += c.len_utf8();
                iter.next();
                if iter.next() == Some('=') {
                    pos += '='.len_utf8();
                    TokenKind::Neq
                } else {
                    panic!()
                }
            }
            ';' => {
                pos += c.len_utf8();
                iter.next();
                TokenKind::Semicolon
            }
            '+' => {
                pos += c.len_utf8();
                iter.next();
                TokenKind::Plus
            }
            '-' => {
                pos += c.len_utf8();
                iter.next();
                TokenKind::Minus
            }
            '[' => {
                pos += c.len_utf8();
                iter.next();
                TokenKind::LeftBracket
            }
            ']' => {
                pos += c.len_utf8();
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
                    pos += "while".len();
                    TokenKind::While
                } else {
                    panic!()
                }
            }
            'f' => {
                iter.next();
                if iter.next() == Some('o') && iter.next() == Some('r') {
                    pos += "for".len();
                    TokenKind::For
                } else {
                    panic!()
                }
            }
            'd' => {
                iter.next();
                if iter.next() == Some('o') {
                    pos += "do".len();
                    TokenKind::Do
                } else {
                    panic!()
                }
            }
            'o' => {
                iter.next();
                if iter.next() == Some('d') {
                    pos += "od".len();
                    TokenKind::Od
                } else {
                    panic!()
                }
            }
            ' ' | '\t' | '\n' => {
                pos += c.len_utf8();
                iter.next();
                continue;
            }
            _ => panic!(),
        };

        out.push(Token::new(kind, start..pos));
    }

    out
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn small_program() {
        let code = "x0 := 32;\nx16 := 25;";
        let mut tokens = lex(code).into_iter();

        assert_eq!(tokens.next(), Some(Token::new(TokenKind::Var(0_u8.into()), 0..2)));
        assert_eq!(tokens.next(), Some(Token::new(TokenKind::Eq, 3..5)));
        assert_eq!(tokens.next(), Some(Token::new(TokenKind::Const(32_u8.into()), 6..8)));
        assert_eq!(tokens.next(), Some(Token::new(TokenKind::Semicolon, 8..9)));
        assert_eq!(tokens.next(), Some(Token::new(TokenKind::Var(16_u8.into()), 10..13)));
        assert_eq!(tokens.next(), Some(Token::new(TokenKind::Eq, 14..16)));
        assert_eq!(tokens.next(), Some(Token::new(TokenKind::Const(25_u8.into()), 17..19)));
        assert_eq!(tokens.next(), Some(Token::new(TokenKind::Semicolon, 19..20)));
    }

    #[test]
    fn simple_program_token_kind() {
        let code = "[[x0 := 32; [x1:=25;x2:=x0+x1]]; while x2 /= 0 do x2 := x2 - x1 od]";
        let tokens = lex(code).into_iter().map(|t| t.kind).collect::<Vec<_>>();
        let expected = vec![
            TokenKind::LeftBracket,
            TokenKind::LeftBracket,
            TokenKind::Var(0_u8.into()),
            TokenKind::Eq,
            TokenKind::Const(32_u8.into()),
            TokenKind::Semicolon,
            TokenKind::LeftBracket,
            TokenKind::Var(1_u8.into()),
            TokenKind::Eq,
            TokenKind::Const(25_u8.into()),
            TokenKind::Semicolon,
            TokenKind::Var(2_u8.into()),
            TokenKind::Eq,
            TokenKind::Var(0_u8.into()),
            TokenKind::Plus,
            TokenKind::Var(1_u8.into()),
            TokenKind::RightBracket,
            TokenKind::RightBracket,
            TokenKind::Semicolon,
            TokenKind::While,
            TokenKind::Var(2_u8.into()),
            TokenKind::Neq,
            TokenKind::Const(0_u8.into()),
            TokenKind::Do,
            TokenKind::Var(2_u8.into()),
            TokenKind::Eq,
            TokenKind::Var(2_u8.into()),
            TokenKind::Minus,
            TokenKind::Var(1_u8.into()),
            TokenKind::Od,
            TokenKind::RightBracket,
        ];

        assert_eq!(tokens, expected);
    }
}
