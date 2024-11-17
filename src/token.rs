use std::iter::Peekable;
use std::{char, fmt::Debug};
use std::{fmt, fs};

use anyhow::{ensure, Context, Result};

use crate::TokenizeArgs;

enum TokenError {
    UnexpectedCharacter(char),
    UnterminatedString,
}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedCharacter(c) => write!(f, "Unexpected character: {}", c),
            Self::UnterminatedString => write!(f, "Unterminated string."),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum NumberType {
    Integer(String),
    Float(String),
}

impl NumberType {
    fn new<S>(s: S, float: bool) -> Self
    where
        S: Into<String>,
    {
        match float {
            true => Self::Float(s.into()),
            false => Self::Integer(s.into()),
        }
    }
}

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

    String(String),
    Number(NumberType),

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

            Lt => "LESS",
            LtEq => "LESS_EQUAL",
            Gt => "GREATER",
            GtEq => "GREATER_EQUAL",

            String(_) => "STRING",
            Number(_) => "NUMBER",

            Eof => "EOF",
            Skip | NewLine => unreachable!("This tokens should never be linified"),
        }
        .to_string()
    }

    fn lexeme(&self) -> String {
        use Token::*;

        match self {
            LeftParen => "(".to_string(),
            RightParen => ")".to_string(),

            LeftBrace => "{".to_string(),
            RightBrace => "}".to_string(),

            Star => "*".to_string(),
            Dot => ".".to_string(),
            Comma => ",".to_string(),
            Plus => "+".to_string(),
            Minus => "-".to_string(),
            Semicolon => ";".to_string(),
            Slash => "/".to_string(),

            Eq => "=".to_string(),
            EqEq => "==".to_string(),

            Bang => "!".to_string(),
            BangEq => "!=".to_string(),

            Lt => "<".to_string(),
            LtEq => "<=".to_string(),
            Gt => ">".to_string(),
            GtEq => ">=".to_string(),

            String(s) => format!("\"{}\"", s),
            Number(NumberType::Float(s)) => s.clone(),
            Number(NumberType::Integer(s)) => s.clone(),

            Eof => "".to_string(),
            Skip | NewLine => unreachable!("This tokens should never be linified"),
        }
    }

    fn literal(&self) -> String {
        use Token::*;

        match self {
            LeftParen | RightParen | LeftBrace | RightBrace | Star | Dot | Comma | Plus | Minus
            | Semicolon | Slash | Eq | EqEq | Bang | BangEq | Lt | LtEq | Gt | GtEq | Eof => {
                "null".to_string()
            }
            String(s) => s.clone(),
            Number(NumberType::Float(s)) => s.parse::<f64>().unwrap_or_else(|_| unreachable!("Should always be a good float")).to_string(),
            Number(NumberType::Integer(s)) => format!("{}.0", s),
            Skip | NewLine => unreachable!("This tokens should never be linified"),
        }
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
            Err(te) => {
                eprintln!("[line {}] Error: {}", line, te);
                err_cnt += 1;
            }
        }
    }

    Ok((result, err_cnt))
}

fn next_token<I>(chars: &mut Peekable<I>) -> Result<Token, TokenError>
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
        Some('"') => scan_string(chars),
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
        Some(d) if d.is_ascii_digit() => scan_number(chars, d),
        Some(c) => Err(TokenError::UnexpectedCharacter(c)),
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

fn scan_string<I>(chars: &mut Peekable<I>) -> Result<Token, TokenError>
where
    I: Iterator<Item = char>,
{
    let mut string = String::new();

    for c in chars {
        if c == '"' {
            return Ok(Token::String(string));
        }
        string.push(c);
    }

    Err(TokenError::UnterminatedString)
}

fn scan_number<I>(chars: &mut Peekable<I>, d: char) -> Result<Token, TokenError>
where
    I: Iterator<Item = char>,
{
    let mut string = d.to_string();
    let mut float = false;

    loop {
        match &chars.peek() {
            None => {
                chars.next();
                return Ok(Token::Number(NumberType::new(string, float)));
            }
            Some(d) if d.is_ascii_digit() => {
                let Some(d) = chars.next() else {
                    unreachable!("chars.next must return a some, guaranteed by prev. peek")
                };
                string.push(d);
            }
            Some('.') if !float => {
                chars.next();
                float = true;
                string.push('.');
            }
            Some(_c) => return Ok(Token::Number(NumberType::new(string, float))),
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

    #[test]
    fn scan_string() -> Result<()> {
        assert_eq!(
            (vec![String("hello".into()), Eof], 0),
            tokenize_str("\"hello\"")?
        );

        Ok(())
    }

    #[test]
    fn scan_unclosed_string() -> Result<()> {
        assert_eq!((vec![Eof], 1), tokenize_str("\"hello")?);

        Ok(())
    }

    #[test]
    fn scan_integer() -> Result<()> {
        assert_eq!(
            (vec![Number(NumberType::Integer("42".into())), Eof], 0),
            tokenize_str("42")?
        );

        Ok(())
    }

    #[test]
    fn scan_float() -> Result<()> {
        assert_eq!(
            (vec![Number(NumberType::Float("1234.1234".into())), Eof], 0),
            tokenize_str("1234.1234")?
        );

        Ok(())
    }
}
