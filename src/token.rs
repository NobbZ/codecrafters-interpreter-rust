use std::fs;
use std::iter::Peekable;
use std::{char, fmt::Debug};

use anyhow::{ensure, Context, Result};

use crate::TokenizeArgs;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Token {
    LeftParen,
    RightParen,

    LeftBrace,
    RightBrace,

    Star,
    Dot,
    Comma,
    Plus,
    Minus,
    Semicolon,
    Slash,

    Eq,
    EqEq,

    Bang,
    BangEq,

    Lt,
    LtEq,
    Gt,
    GtEq,

    Eof,
}

impl Token {
    fn is_eof(&self) -> bool {
        self == &Token::Eof
    }
}

pub fn tokenize(args: TokenizeArgs) -> Result<()> {
    let file_contents = fs::read_to_string(&args.file)
        .with_context(|| format!("failed to read {:?}", &args.file))?;

    let (tokens, err_cnt) =
        tokenize_str(&file_contents).with_context(|| format!("scanning {}", file_contents))?;

    for token in tokens.iter() {
        use Token::*;

        let str = match token {
            LeftParen => "LEFT_PAREN ( null",
            RightParen => "RIGHT_PAREN ) null",

            LeftBrace => "LEFT_BRACE { null",
            RightBrace => "RIGHT_BRACE } null",

            Star => "STAR * null",
            Dot => "DOT . null",
            Comma => "COMMA , null",
            Plus => "PLUS + null",
            Minus => "MINUS - null",
            Semicolon => "SEMICOLON ; null",
            Slash => "SLASH / null",

            Eq => "EQUAL = null",
            EqEq => "EQUAL_EQUAL == null",

            Bang => "BANG ! null",
            BangEq => "BANG_EQUAL != null",

            Lt => "LESS < null",
            LtEq => "LESS_EQUAL <= null",
            Gt => "GREATER > null",
            GtEq => "GREATER_EQUAL >= null",

            Eof => "EOF  null",
        };

        println!("{}", str);
    }

    ensure!(err_cnt == 0);

    Ok(())
}

fn tokenize_str<S>(s: S) -> Result<(Vec<Token>, usize)>
where
    S: AsRef<str> + Debug,
{
    let mut result = Vec::new();
    let mut err_cnt: usize = 0;

    let mut chars = s.as_ref().chars().peekable();

    loop {
        match next_token(&mut chars) {
            Ok(token) => {
                result.push(token.clone());
                if token.is_eof() {
                    break;
                };
            }
            Err(_) => {
                err_cnt += 1;
            }
        }
    }

    Ok((result, err_cnt))
}

fn next_token<I>(chars: &mut Peekable<I>) -> Result<Token, char>
where
    I: Iterator<Item = char>,
{
    match chars.next() {
        Some(' ' | '\n' | '\t') => next_token(chars),
        Some('(') => Ok(Token::LeftParen),
        Some(')') => Ok(Token::RightParen),
        Some('{') => Ok(Token::LeftBrace),
        Some('}') => Ok(Token::RightBrace),
        Some('*') => Ok(Token::Star),
        Some('.') => Ok(Token::Dot),
        Some(',') => Ok(Token::Comma),
        Some('+') => Ok(Token::Plus),
        Some('-') => Ok(Token::Minus),
        Some(';') => Ok(Token::Semicolon),
        Some('=') => match chars.peek() {
            Some('=') => {
                chars.next();
                Ok(Token::EqEq)
            }
            _ => Ok(Token::Eq),
        },
        Some('!') => match chars.peek() {
            Some('=') => {
                chars.next();
                Ok(Token::BangEq)
            }
            _ => Ok(Token::Bang),
        },
        Some('<') => match chars.peek() {
            Some('=') => {
                chars.next();
                Ok(Token::LtEq)
            }
            _ => Ok(Token::Lt),
        },
        Some('>') => match chars.peek() {
            Some('=') => {
                chars.next();
                Ok(Token::GtEq)
            }
            _ => Ok(Token::Gt),
        },
        Some('/') => match chars.peek() {
            Some('/') => {
                skip_until(chars, '\n');
                next_token(chars)
            }
            _ => Ok(Token::Slash),
        },
        Some(c) => {
            eprintln!("[line 1] Error: Unexpected character: {}", c);
            Err(c)
        }
        None => Ok(Token::Eof),
    }
}

fn skip_until<I>(chars: &mut Peekable<I>, last: char)
where
    I: Iterator<Item = char>,
{
    for c in chars {
        if c == last {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;

    #[test]
    fn scans_example_from_exercise() -> Result<()> {
        assert_eq!(
            (vec![LeftParen, LeftParen, RightParen, Eof], 0),
            tokenize_str("(()")?
        );

        Ok(())
    }

    #[test]
    fn scans_example_from_test_1() -> Result<()> {
        assert_eq!((vec![LeftParen, Eof], 0), tokenize_str("(")?);

        Ok(())
    }

    #[test]
    fn scans_example_from_test_2() -> Result<()> {
        assert_eq!((vec![RightParen, RightParen, Eof], 0), tokenize_str("))")?);

        Ok(())
    }

    #[test]
    fn scans_example_from_test_3() -> Result<()> {
        assert_eq!(
            (
                vec![LeftParen, LeftParen, RightParen, RightParen, RightParen, Eof],
                0
            ),
            tokenize_str("(()))")?
        );

        Ok(())
    }

    #[test]
    fn scans_example_from_test_4() -> Result<()> {
        assert_eq!(
            (
                vec![
                    LeftParen, RightParen, LeftParen, LeftParen, LeftParen, RightParen, RightParen,
                    Eof
                ],
                0
            ),
            tokenize_str("()((())")?
        );

        Ok(())
    }

    #[test]
    fn scan_braces() -> Result<()> {
        assert_eq!((vec![RightBrace, LeftBrace, Eof], 0), tokenize_str("}{")?);

        Ok(())
    }

    #[test]
    fn scan_more_single_char_tokens() -> Result<()> {
        assert_eq!(
            (
                vec![
                    LeftParen, LeftBrace, Star, Dot, Comma, Plus, Star, RightBrace, RightParen, Eof
                ],
                0
            ),
            tokenize_str("({*.,+*})")?
        );

        Ok(())
    }

    #[test]
    fn scan_with_unkown_tokens() -> Result<()> {
        assert_eq!(
            (vec![Comma, Dot, LeftParen, Eof], 2),
            tokenize_str(",.$(#")?
        );

        Ok(())
    }

    #[test]
    fn scan_with_equals() -> Result<()> {
        assert_eq!(
            (vec![Eq, LeftBrace, EqEq, Eq, RightBrace, Eof], 0),
            tokenize_str("={===}")?
        );

        Ok(())
    }

    #[test]
    fn scan_with_bangs() -> Result<()> {
        assert_eq!((vec![Bang, BangEq, EqEq, Eof], 0), tokenize_str("!!===")?);

        Ok(())
    }

    #[test]
    fn scan_with_lt_gt() -> Result<()> {
        assert_eq!((vec![Lt, LtEq, Gt, GtEq, Eof], 0), tokenize_str("<<=>>=")?);

        Ok(())
    }

    #[test]
    fn scan_with_comment() -> Result<()> {
        assert_eq!(
            (vec![LeftParen, RightParen, Eof], 0),
            tokenize_str("()// Comment")?
        );

        Ok(())
    }

    #[test]
    fn scan_with_slash() -> Result<()> {
        assert_eq!(
            (vec![Slash, LeftParen, RightParen, Eof], 0),
            tokenize_str("/()")?
        );

        Ok(())
    }

    #[test]
    fn scan_with_whitespace() -> Result<()> {
        assert_eq!(
            (vec![LeftParen, RightParen, Eof], 0),
            tokenize_str("(\t\n )")?
        );

        Ok(())
    }
}
