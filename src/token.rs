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
    Skip,
    NewLine,
}

impl Token {
    fn is_eof(&self) -> bool {
        self == &Token::Eof
    }

    fn to_terminal(&self) -> String {
        let token_type = self.token_type();
        let lexeme = self.lexeme();
        let literal = self.literal();

        format!("{} {} {}", token_type, lexeme, literal)
    }

    fn token_type(&self) -> String {
        use Token::*;

        match self {
            LeftParen => "LEFT_PAREN",
            RightParen => "RIGHT_PAREN",

            LeftBrace => "LEFT_BRACE",
            RightBrace => "RIGHT_BRACE",

            Star => "STAR",
            Dot => "DOT",
            Comma => "COMMA",
            Plus => "PLUS",
            Minus => "MINUS",
            Semicolon => "SEMICOLON",
            Slash => "SLASH",

            Eq => "EQUAL",
            EqEq => "EQUAL_EQUAL",

            Bang => "BANG",
            BangEq => "BANG_EQUAL",

            Lt => "LESSER",
            LtEq => "LESSER_EQUAL",
            Gt => "GREATER",
            GtEq => "GREATER_EQUAL",

            Eof => "EOF",
            Skip | NewLine => unreachable!("This tokens should never be linified"),
        }
        .to_string()
    }

    fn lexeme(&self) -> String {
        use Token::*;

        match self {
            LeftParen => "(",
            RightParen => ")",

            LeftBrace => "{",
            RightBrace => "}",

            Star => "*",
            Dot => ".",
            Comma => ",",
            Plus => "+",
            Minus => "-",
            Semicolon => ";",
            Slash => "/",

            Eq => "=",
            EqEq => "==",

            Bang => "!",
            BangEq => "!=",

            Lt => "<",
            LtEq => "<=",
            Gt => ">",
            GtEq => ">=",

            Eof => "",
            Skip | NewLine => unreachable!("This tokens should never be linified"),
        }
        .to_string()
    }

    fn literal(&self) -> String {
        use Token::*;

        match self {
            LeftParen | RightParen | LeftBrace | RightBrace | Star | Dot | Comma | Plus | Minus
            | Semicolon | Slash | Eq | EqEq | Bang | BangEq | Lt | LtEq | Gt | GtEq | Eof => "null",
            Skip | NewLine => unreachable!("This tokens should never be linified"),
        }
        .to_string()
    }
}

pub fn tokenize(args: TokenizeArgs) -> Result<()> {
    let file_contents = fs::read_to_string(&args.file)
        .with_context(|| format!("failed to read {:?}", &args.file))?;

    let (tokens, err_cnt) =
        tokenize_str(&file_contents).with_context(|| format!("scanning {}", file_contents))?;

    for token in tokens.iter() {
        println!("{}", token.to_terminal());
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
    let mut line = 1;

    let mut chars = s.as_ref().chars().peekable();

    loop {
        match next_token(&mut chars) {
            Ok(Token::NewLine) => {
                line += 1;
            }
            Ok(Token::Skip) => (),
            Ok(token) => {
                result.push(token.clone());
                if token.is_eof() {
                    break;
                };
            }
            Err(c) => {
                eprintln!("[line {}] Error: Unexpected character: {}", line, c);
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
        Some(' ' | '\t') => Ok(Token::Skip),
        Some('\n') => Ok(Token::NewLine),
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
                Ok(Token::NewLine)
            }
            _ => Ok(Token::Slash),
        },
        Some(c) => Err(c),
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
