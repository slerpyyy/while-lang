use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::*;

use std::{iter::Peekable, ops::{Add, Mul, Range}};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Var(IndexV2),
    Const(Value),
    Eq,
    Neq,
    Semicolon,
    Plus,
    Minus,
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,
    While,
    For,
    Fn,
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
    pub const fn var(&self) -> Option<&IndexV2> {
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
pub fn lex(code: &str, file_id: usize) -> Result<Vec<Token>, Diagnostic<usize>> {
    let mut out = Vec::new();
    let mut iter = code.chars().peekable();
    let mut pos = 0;

    fn check_inc(actual: Option<char>, expected: char, position: &mut usize) -> bool {
        let check = actual == Some(expected);
        if check {
            *position += expected.len_utf8();
        }
        check
    }

    fn unexpected_symbol(file_id: usize, span: Range<usize>, help: Option<impl Into<String>>) -> Diagnostic<usize> {
        let base = Diagnostic::error()
            .with_message("Unexpected symbol")
            .with_labels(vec![Label::primary(file_id, span)]);

        match help {
            Some(msg) => base.with_notes(vec![msg.into()]),
            None => base,
        }
    }

    while let Some(&c) = iter.peek() {
        let start = pos;
        let kind = match c {
            '#' => {
                while let Some(ch) = iter.next() {
                    pos += ch.len_utf8();
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }
            '{' => {
                while let Some(ch) = iter.next() {
                    pos += ch.len_utf8();
                    if ch == '}' {
                        break;
                    }
                }
                continue;
            }
            '0'..='9' => {
                let num = parse_number(&mut iter, &mut pos);
                TokenKind::Const(num)
            }
            'x' => {
                pos += c.len_utf8();
                iter.next();
                let num = parse_number(&mut iter, &mut pos);
                TokenKind::Var(IndexV2::Int(num))
            }
            ':' => {
                pos += c.len_utf8();
                iter.next();
                if iter.next() == Some('=') {
                    pos += '='.len_utf8();
                    TokenKind::Eq
                } else {
                    Err(unexpected_symbol(file_id, start..pos, Some("help: Did you mean `:=`?")))?
                }
            }
            '/' => {
                pos += c.len_utf8();
                iter.next();
                if iter.next() == Some('=') {
                    pos += '='.len_utf8();
                    TokenKind::Neq
                } else {
                    Err(unexpected_symbol(file_id, start..pos, Some("help: Did you mean `/=`?")))?
                }
            }
            '=' => {
                pos += c.len_utf8();
                Err(unexpected_symbol(file_id, start..pos, Some("help: Did you mean `:=` or `/=`?")))?
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
            '(' => {
                pos += c.len_utf8();
                iter.next();
                TokenKind::LeftParen
            }
            ')' => {
                pos += c.len_utf8();
                iter.next();
                TokenKind::RightParen
            }
            'w' => {
                pos += c.len_utf8();
                iter.next();
                if check_inc(iter.next(), 'h', &mut pos)
                && check_inc(iter.next(), 'i', &mut pos)
                && check_inc(iter.next(), 'l', &mut pos)
                && check_inc(iter.next(), 'e', &mut pos)
                {
                    TokenKind::While
                } else {
                    Err(unexpected_symbol(file_id, start..pos, Some("help: Did you mean `while`?")))?
                }
            }
            'f' => {
                pos += c.len_utf8();
                iter.next();
                let second = iter.next();
                if check_inc(second, 'n', &mut pos) {
                    TokenKind::Fn
                } else if check_inc(second, 'o', &mut pos)
                && check_inc(iter.next(), 'r', &mut pos) {
                    TokenKind::For
                } else {
                    Err(unexpected_symbol(file_id, start..pos, Some("help: Did you mean `for` or `fn`?")))?
                }
            }
            'd' => {
                pos += c.len_utf8();
                iter.next();
                if check_inc(iter.next(), 'o', &mut pos) {
                    TokenKind::Do
                } else {
                    Err(unexpected_symbol(file_id, start..pos, Some("help: Did you mean `do`?")))?
                }
            }
            'o' => {
                pos += c.len_utf8();
                iter.next();
                if check_inc(iter.next(), 'd', &mut pos) {
                    TokenKind::Od
                } else {
                    Err(unexpected_symbol(file_id, start..pos, Some("help: Did you mean `od`?")))?
                }
            }
            ' ' | '\t' | '\n' | '\r' => {
                pos += c.len_utf8();
                iter.next();
                continue;
            }
            'A'..='Z' | '_' => {
                let mut ident = String::new();
                while let Some(&ch) = iter.peek() {
                    if !ch.is_ascii_alphanumeric() && ch != '_' {
                        break;
                    }

                    ident.push(ch);
                    pos += ch.len_utf8();
                    iter.next();
                }
                TokenKind::Var(IndexV2::Name(ident))
            }
            _ => {
                pos += c.len_utf8();
                let help = if c.is_ascii_alphabetic() {
                    Some("help: Identifiers must start with an uppercase character or underscore")
                } else if !c.is_ascii() {
                    Some("help: This interpreter does not support unicode")
                } else {
                    None
                };

                Err(unexpected_symbol(file_id, start..pos, help))?
            },
        };

        out.push(Token::new(kind, start..pos));
    }

    Ok(out)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn small_program() {
        let code = "x0 := 32;\nx16 := 25;";
        let mut tokens = lex(code, 0).unwrap().into_iter();

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
        let tokens = lex(code, 0).unwrap().into_iter().map(|t| t.kind).collect::<Vec<_>>();
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
