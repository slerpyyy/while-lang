use crate::*;
use std::{
    iter::Peekable,
    ops::{Add, Mul},
    panic,
};

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
}

impl Token {
    #[must_use]
    pub fn new(kind: TokenKind) -> Self {
        Self { kind }
    }

    #[must_use]
    pub fn var(&self) -> Option<Index> {
        match self.kind {
            TokenKind::Var(k) => Some(k),
            _ => None,
        }
    }
}

#[must_use]
fn parse_number<T>(iter: &mut Peekable<impl Iterator<Item = char>>) -> T
where
    T: From<u8> + Add<Output = T> + Mul<Output = T>,
{
    let mut num = T::from(0);
    while let Some(&c @ '0'..='9') = iter.peek() {
        num = num * T::from(10) + T::from(c as u8 - b'0');
        iter.next();
    }
    num
}

#[must_use]
pub fn lex(code: &str) -> Vec<Token> {
    let mut out = Vec::new();
    let mut iter = code.chars().peekable();

    while let Some(&c) = iter.peek() {
        let kind = match c {
            '0'..='9' => {
                let num = parse_number(&mut iter);
                TokenKind::Const(num)
            }
            'x' => {
                iter.next();
                let num = parse_number(&mut iter);
                TokenKind::Var(num)
            }
            ':' => {
                iter.next();
                if iter.next() == Some('=') {
                    TokenKind::Eq
                } else {
                    panic!()
                }
            }
            '/' => {
                iter.next();
                if iter.next() == Some('=') {
                    TokenKind::Neq
                } else {
                    panic!()
                }
            }
            ';' => {
                iter.next();
                TokenKind::Semicolon
            }
            '+' => {
                iter.next();
                TokenKind::Plus
            }
            '-' => {
                iter.next();
                TokenKind::Minus
            }
            '[' => {
                iter.next();
                TokenKind::LeftBracket
            }
            ']' => {
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
                    TokenKind::While
                } else {
                    panic!()
                }
            }
            'f' => {
                iter.next();
                if iter.next() == Some('o') && iter.next() == Some('r') {
                    TokenKind::For
                } else {
                    panic!()
                }
            }
            'd' => {
                iter.next();
                if iter.next() == Some('o') {
                    TokenKind::Do
                } else {
                    panic!()
                }
            }
            'o' => {
                iter.next();
                if iter.next() == Some('d') {
                    TokenKind::Od
                } else {
                    panic!()
                }
            }
            ' ' | '\t' | '\n' => {
                iter.next();
                continue;
            }
            _ => panic!(),
        };

        out.push(Token { kind });
    }

    out
}

#[cfg(test)]
mod test {
    use super::*;
    use TokenKind::*;

    #[test]
    fn simple_program() {
        let code = "[[x0 := 32; [x1:=25;x2:=x0+x1]]; while x2 /= 0 do x2 := x2 - x1 od]";
        let tokens = lex(code);
        let expected = vec![
            Token::new(LeftBracket),
            Token::new(LeftBracket),
            Token::new(Var(0)),
            Token::new(Eq),
            Token::new(Const(32_u8.into())),
            Token::new(Semicolon),
            Token::new(LeftBracket),
            Token::new(Var(1)),
            Token::new(Eq),
            Token::new(Const(25_u8.into())),
            Token::new(Semicolon),
            Token::new(Var(2)),
            Token::new(Eq),
            Token::new(Var(0)),
            Token::new(Plus),
            Token::new(Var(1)),
            Token::new(RightBracket),
            Token::new(RightBracket),
            Token::new(Semicolon),
            Token::new(While),
            Token::new(Var(2)),
            Token::new(Neq),
            Token::new(Const(0_u8.into())),
            Token::new(Do),
            Token::new(Var(2)),
            Token::new(Eq),
            Token::new(Var(2)),
            Token::new(Minus),
            Token::new(Var(1)),
            Token::new(Od),
            Token::new(RightBracket),
        ];

        assert_eq!(tokens, expected);
    }
}
