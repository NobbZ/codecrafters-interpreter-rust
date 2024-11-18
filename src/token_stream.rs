use std::{iter::Peekable};

use crate::token::{NumberType, Token, TokenError};

#[derive(Debug)]
pub(crate) struct TokenStream<I: Iterator<Item = char>> {
    inner: Peekable<I>,
    errors: Vec<(usize, TokenError)>,
    finished: bool,
    pub(crate) line: usize,
}

impl<I> From<Peekable<I>> for TokenStream<I>
where
    I: Iterator<Item = char>,
{
    fn from(chars: Peekable<I>) -> Self
    where
        I: Iterator<Item = char>,
    {
        TokenStream {
            inner: chars,
            errors: vec![],
            line: 1,
            finished: false,
        }
    }
}

impl<I> From<I> for TokenStream<I>
where
    I: Iterator<Item = char>,
{
    fn from(chars: I) -> Self {
        TokenStream::from(chars.peekable())
    }
}

impl<I> Iterator for TokenStream<I>
where
    I: Iterator<Item = char>,
{
    type Item = Result<Token, (usize, TokenError)>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        loop {
            match self.next_token() {
                Ok(Token::NewLine) => {
                    self.line += 1;
                }
                Ok(Token::Skip) => (),
                Ok(token) => return Some(Ok(token)),
                Err(token_error) => {
                    dbg!(token_error);
                    self.errors.push((self.line, token_error));
                    return Some(Err((self.line, token_error)));
                }
            }
        }
    }
}

impl<I> TokenStream<I>
where
    I: Iterator<Item = char>,
{
    fn next_char(&mut self) -> Option<char> {
        self.inner.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.inner.peek()
    }

    fn next_token(&mut self) -> Result<Token, TokenError> {
        match self.next_char() {
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
            Some('"') => self.scan_string(),
            Some('=') => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Ok(Token::EqEq)
                }
                _ => Ok(Token::Eq),
            },
            Some('!') => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Ok(Token::BangEq)
                }
                _ => Ok(Token::Bang),
            },
            Some('<') => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Ok(Token::LtEq)
                }
                _ => Ok(Token::Lt),
            },
            Some('>') => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Ok(Token::GtEq)
                }
                _ => Ok(Token::Gt),
            },
            Some('/') => match self.peek_char() {
                Some('/') => {
                    self.skip_until('\n');
                    Ok(Token::NewLine)
                }
                _ => Ok(Token::Slash),
            },
            Some(d) if d.is_ascii_digit() => self.scan_number(d),
            Some(c) if c.is_ascii_alphabetic() || c == '_' => self.scan_ident(c),
            Some(c) => Err(TokenError::UnexpectedCharacter(c)),
            None => {
                self.finished = true;
                Ok(Token::Eof)
            }
        }
    }

    fn scan_string(&mut self) -> Result<Token, TokenError> {
        let mut string = String::new();

        while let Some(c) = self.next_char() {
            if c == '"' {
                return Ok(Token::String(string));
            }
            string.push(c);
        }

        Err(TokenError::UnterminatedString)
    }

    fn skip_until(&mut self, until: char)
    where
        I: Iterator<Item = char>,
    {
        while let Some(c) = self.next_char() {
            if c == until {
                break;
            }
        }
    }

    fn scan_number(&mut self, initial_digit: char) -> Result<Token, TokenError> {
        let mut string = initial_digit.to_string();
        let mut float = false;

        loop {
            match &self.peek_char() {
                None => {
                    self.next_char();
                    return Ok(Token::Number(NumberType::new(string, float)));
                }
                Some(d) if d.is_ascii_digit() => {
                    let Some(d) = self.next_char() else {
                        unreachable!("chars.next must return a some, guaranteed by prev. peek")
                    };
                    string.push(d);
                }
                Some('.') if !float => {
                    self.next_char();
                    float = true;
                    string.push('.');
                }
                Some(_c) => return Ok(Token::Number(NumberType::new(string, float))),
            }
        }
    }

    fn scan_ident(&mut self, initial_char: char) -> Result<Token, TokenError>
    where
        I: Iterator<Item = char>,
    {
        let mut string = initial_char.to_string();

        loop {
            match &self.peek_char() {
                None => {
                    self.next_char();
                    return self.decide_reserved(string);
                }
                Some(c) if c.is_ascii_alphabetic() || c.is_ascii_digit() || c == &&'_' => {
                    let Some(c) = self.next_char() else {
                        unreachable!("chars.next must return a some, guaranteed by prev. peek")
                    };
                    string.push(c);
                }
                Some(_c) => return self.decide_reserved(string),
            }
        }
    }

    fn decide_reserved<S>(&self, canditate: S) -> Result<Token, TokenError>
    where
        S: AsRef<str>,
    {
        match canditate.as_ref() {
            "and" => Ok(Token::And),
            "class" => Ok(Token::Class),
            "else" => Ok(Token::Else),
            "false" => Ok(Token::False),
            "for" => Ok(Token::For),
            "fun" => Ok(Token::Fun),
            "if" => Ok(Token::If),
            "nil" => Ok(Token::Nil),
            "or" => Ok(Token::Or),
            "print" => Ok(Token::Print),
            "return" => Ok(Token::Return),
            "super" => Ok(Token::Super),
            "this" => Ok(Token::This),
            "true" => Ok(Token::True),
            "var" => Ok(Token::Var),
            "while" => Ok(Token::While),
            ident => Ok(Token::Ident(ident.to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::NumberType;
    use crate::token::Token::*;

    use super::*;

    macro_rules! example {
        ($($name:ident : $input:tt => $expected:tt,)+) => {
            $(
                #[test]
                fn $name() -> anyhow::Result<()> {
                    let token_stream = TokenStream::from($input.chars());
                    let mut err_cnt = 0;

                    let tokens = token_stream
                        .map(|t| t.map_err(|_| err_cnt += 1))
                        .filter(|t| t.is_ok())
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap();

                    assert_eq!($expected, (tokens, err_cnt));
                    Ok(())
                }
            )+
        };
    }

    example! {
        scans_example_from_exercise: "(()" => (vec![LeftParen, LeftParen, RightParen, Eof], 0),
        scans_example_from_test_1: "(" => (vec![LeftParen, Eof], 0),
        scans_example_from_test_2: "))" => (vec![RightParen, RightParen, Eof], 0),
        scans_example_from_test_3: "(()))" => (vec![LeftParen, LeftParen, RightParen, RightParen, RightParen, Eof], 0),
        scans_example_from_test_4: "()((())" => (vec![LeftParen, RightParen, LeftParen, LeftParen, LeftParen, RightParen, RightParen, Eof], 0),
        scan_braces: "}{" => (vec![RightBrace, LeftBrace, Eof], 0),
        scan_more_single_char_tokens: "({*.,+*})" => (vec![LeftParen, LeftBrace, Star, Dot, Comma, Plus, Star, RightBrace, RightParen, Eof], 0),
        scan_with_unkown_tokens: ",.$(#" => (vec![Comma, Dot, LeftParen, Eof], 2),
        scan_with_equals: "={===}" => (vec![Eq, LeftBrace, EqEq, Eq, RightBrace, Eof], 0),
        scan_with_bangs: "!!===" => (vec![Bang, BangEq, EqEq, Eof], 0),
        scan_with_lt_gt: "<<=>>=" => (vec![Lt, LtEq, Gt, GtEq, Eof], 0),
        scan_with_comment: "()// Comment" => (vec![LeftParen, RightParen, Eof], 0),
        scan_with_slash: "/()" => (vec![Slash, LeftParen, RightParen, Eof], 0),
        scan_with_whitespace: "(\t\n )" => (vec![LeftParen, RightParen, Eof], 0),
        scan_string: "\"hello\"" => (vec![String("hello".into()), Eof], 0),
        scan_unclosed_string: "\"hello" => (vec![Eof], 1),
        scan_integer: "42" => (vec![Number(NumberType::Integer("42".into())), Eof], 0),
        scan_float: "1234.1234" => (vec![Number(NumberType::Float("1234.1234".into())), Eof], 0),
        scan_ident: "foo bar _hello" => (vec![Ident("foo".into()), Ident("bar".into()), Ident("_hello".into()), Eof], 0),
        scan_reserved: "and" => (vec![And, Eof], 0),
    }
}
