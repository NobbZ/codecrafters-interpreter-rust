use std::fmt::Debug;
use std::fs;

use anyhow::{bail, Context, Ok, Result};

use crate::TokenizeArgs;

#[derive(Debug, PartialEq, Eq)]
enum Token {
    LeftParen,
    RightParen,

    LeftBrace,
    RightBrace,

    Eof,
}

pub fn tokenize(args: TokenizeArgs) -> Result<()> {
    let file_contents = fs::read_to_string(&args.file)
        .with_context(|| format!("failed to read {:?}", &args.file))?;

    let tokens =
        tokenize_str(&file_contents).with_context(|| format!("scanning {}", file_contents))?;

    for token in tokens.iter() {
        let str = match token {
            Token::LeftParen => "LEFT_PAREN ( null",
            Token::RightParen => "RIGHT_PAREN ) null",

            Token::LeftBrace => "LEFT_BRACE { null",
            Token::RightBrace => "RIGHT_BRACE } null",
            Token::Eof => "EOF  null",
        };

        println!("{}", str);
    }

    Ok(())
}

fn tokenize_str<S>(s: S) -> Result<Vec<Token>>
where
    S: AsRef<str> + Debug,
{
    let mut result = Vec::new();

    for c in s.as_ref().chars() {
        match c {
            '(' => result.push(Token::LeftParen),
            ')' => result.push(Token::RightParen),
            '{' => result.push(Token::LeftBrace),
            '}' => result.push(Token::RightBrace),
            _ => bail!("unknown token: {:?}", c),
        }
    }

    result.push(Token::Eof);

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scans_example_from_exercise() -> Result<()> {
        use Token::*;

        assert_eq!(
            vec![LeftParen, LeftParen, RightParen, Eof],
            tokenize_str("(()")?
        );

        Ok(())
    }

    #[test]
    fn scans_example_from_test_1() -> Result<()> {
        use Token::*;

        assert_eq!(vec![LeftParen, Eof], tokenize_str("(")?);

        Ok(())
    }
    
    #[test]
    fn scans_example_from_test_2() -> Result<()> {
        use Token::*;

        assert_eq!(vec![RightParen, RightParen, Eof], tokenize_str("))")?);

        Ok(())
    }
    
    #[test]
    fn scans_example_from_test_3() -> Result<()> {
        use Token::*;

        assert_eq!(
            vec![LeftParen, LeftParen, RightParen, RightParen, RightParen, Eof],
            tokenize_str("(()))")?
        );

        Ok(())
    }

    #[test]
    fn scans_example_from_test_4() -> Result<()> {
        use Token::*;

        assert_eq!(
            vec![LeftParen, RightParen, LeftParen, LeftParen, LeftParen, RightParen, RightParen, Eof],
            tokenize_str("()((())")?
        );

        Ok(())
    }

    #[test]
    fn scan_braces() -> Result<()> {
        use Token::*;

        assert_eq!(
            vec![RightBrace, LeftBrace, Eof],
            tokenize_str("}{")?
        );

        Ok(())
    }
}
