use std::path::Path;
use std::{fmt::Display, fs, iter::Peekable};

use crate::{token::Token, token_stream::TokenStream};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Op {
    Mul,
    Div,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Expr {
    Bool(bool),
    Number(String),
    String(String),
    Nil,
    Group(Box<Expr>),
    Not(Box<Expr>),
    Neg(Box<Expr>),
    Binary(Op, Box<Expr>, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Number(n) => {
                if n.contains('.') {
                    write!(f, "{}", n.trim_end_matches('0'))
                } else {
                    write!(f, "{}.0", n)
                }
            }
            Self::String(s) => write!(f, "{}", s),
            Self::Nil => write!(f, "nil"),
            Self::Group(e) => write!(f, "(group {})", e),
            Self::Not(e) => write!(f, "(! {})", e),
            Self::Neg(e) => write!(f, "(- {})", e),
            Self::Binary(op, e1, e2) => write!(f, "({} {} {})", op, e1, e2),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Delim {
    Parenthesis,
}

impl std::fmt::Display for Delim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let char = match self {
            Self::Parenthesis => ')',
        };

        write!(f, "{}", char)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ParseError {
    UnbalancedDelims(Delim),
    UnexpectedEof,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedEof => write!(f, "File ended unexpectedly"),
            Self::UnbalancedDelims(d) => write!(f, "Unbalanced delimiters, expected: '{}'", d),
        }
    }
}

impl std::error::Error for ParseError {}

pub(crate) struct Parser<'de, T>
where
    T: Iterator<Item = Token<'de>>,
{
    tokens: Peekable<T>,
}

impl<'de, T> Parser<'de, T>
where
    T: Iterator<Item = Token<'de>>,
{
    pub fn new(token_stream: T) -> Self {
        Self {
            tokens: token_stream.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut exprs = Vec::new();

        while let Some(expr) = self.parse_next() {
            exprs.push(expr?);
        }

        Ok(exprs)
    }

    fn parse_next(&mut self) -> Option<Result<Expr, ParseError>> {
        use Token::*;

        let expr = (match self.tokens.next() {
            Some(True) => Some(Ok(Expr::Bool(true))),
            Some(False) => Some(Ok(Expr::Bool(false))),
            Some(Number(f)) => Some(Ok(Expr::Number(f.parse().unwrap()))),
            Some(String(s)) => Some(Ok(Expr::String(s.to_string()))),
            Some(Nil) => Some(Ok(Expr::Nil)),
            Some(Bang) => self.parse_unary(Expr::Not),
            Some(Minus) => self.parse_unary(Expr::Neg),
            Some(LeftParen) => self.parse_group(),
            Some(Eof) => None,
            _ => unimplemented!(),
        })?;

        Some(expr.and_then(|expr| {
            match self.tokens.peek() {
                Some(Star) => self
                    .parse_binary(|e| Expr::Binary(Op::Mul, Box::new(expr.clone()), e))
                    .unwrap(),
                Some(Slash) => self
                    .parse_binary(|e| Expr::Binary(Op::Div, Box::new(expr.clone()), e))
                    .unwrap(),
                _ => Ok(expr),
            }
        }))
    }

    fn parse_group(&mut self) -> Option<Result<Expr, ParseError>> {
        match self.parse_next() {
            Some(Ok(expr)) => {
                if let Some(Token::RightParen) = self.tokens.next() {
                    Some(Ok(Expr::Group(Box::new(expr))))
                } else {
                    Some(Err(ParseError::UnbalancedDelims(Delim::Parenthesis)))
                }
            }
            err @ Some(Err(_)) => err,
            None => Some(Err(ParseError::UnbalancedDelims(Delim::Parenthesis))),
        }
    }

    fn parse_unary<C>(&mut self, constructor: C) -> Option<Result<Expr, ParseError>>
    where
        C: Fn(Box<Expr>) -> Expr,
    {
        match self.parse_next() {
            Some(Ok(expr)) => Some(Ok(constructor(Box::new(expr)))),
            err @ Some(Err(_)) => err,
            None => Some(Err(ParseError::UnexpectedEof)),
        }
    }

    fn parse_binary<C>(&mut self, constructor: C) -> Option<Result<Expr, ParseError>>
    where
        C: Fn(Box<Expr>) -> Expr,
    {
        self.tokens.next();
        Some(match self.parse_next() {
            Some(Ok(expr)) => Ok(constructor(Box::new(expr))),
            Some(err @ Err(_)) => err,
            None => Err(ParseError::UnexpectedEof),
        })
    }
}

pub fn parse_file(file: impl AsRef<Path>) -> anyhow::Result<()> {
    let content = fs::read_to_string(file)?;
    let ts = TokenStream::from(content.as_ref()).filter_map(|t| match t {
        Ok(t) => Some(t),
        Err(_) => None,
    });

    let exprs = Parser::new(ts).parse()?;

    for expr in exprs.iter() {
        println!("{}", expr);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    macro_rules! example {
        ($($name:ident : $input:tt => $expected:expr,)*) => {
            $(
                #[test]
                fn $name() -> anyhow::Result<()> {
                    let token_stream = $input.into_iter();
                    let mut parser = Parser::new(token_stream);

                    assert_eq!(Some($expected), parser.parse_next());
                    assert_eq!(None, parser.parse_next());

                    anyhow::Ok(())
                }
            )*
        };
    }

    macro_rules! full_string_example {
        ($($name:ident : $input:expr => $expected:expr,)*) => {
            $(
                #[test]
                fn $name() -> anyhow::Result<()> {
                    let token_stream = crate::token_stream::TokenStream::from($input).filter_map(|tok_res| match tok_res {
                        Ok(t) => Some(t),
                        Err(_) => None,
                    });
                    let mut parser = Parser::new(token_stream);

                    let printed = format!("{}", parser.parse_next().unwrap().unwrap());

                    assert_eq!($expected, printed);
                    assert_eq!(None, parser.parse_next());

                    Ok(())
                }
            )*
        };
    }

    example! {
        negated_number: [Token::Minus, Token::Number("1"), Token::Eof] => Ok(Expr::Neg(Box::new(Expr::Number("1".into())))),
        notted_bool: [Token::Bang, Token::False, Token::Eof] => Ok(Expr::Not(Box::new(Expr::Bool(false)))),
        bool: [Token::True, Token::Eof] => Ok(Expr::Bool(true)),
        int: [Token::Number("12"), Token::Eof] => Ok(Expr::Number("12".into())),
        float: [Token::Number("12.13"), Token::Eof] => Ok(Expr::Number("12.13".into())),
        nil: [Token::Nil, Token::Eof] => Ok(Expr::Nil),
        string: [Token::String("foo"), Token::Eof] => Ok(Expr::String("foo".into())),
        group: [Token::LeftParen, Token::Nil, Token::RightParen, Token::Eof] => Ok(Expr::Group(Box::new(Expr::Nil))),
        multiply: [Token::Number("1"), Token::Star, Token::Number("1"), Token::Eof] => Ok(Expr::Binary(Op::Mul, Box::new(Expr::Number("1".into())), Box::new(Expr::Number("1".into())))),
        divide: [Token::Number("1"), Token::Slash, Token::Number("1"), Token::Eof] => Ok(Expr::Binary(Op::Div, Box::new(Expr::Number("1".into())), Box::new(Expr::Number("1".into())))),

        mixed: [
            Token::Number("23"),
            Token::Star,
            Token::Number("65"),
            Token::Slash,
            Token::Number("68"),
        ] => Ok(Expr::Binary(Op::Div, Box::new(Expr::Binary(Op::Mul, Box::new(Expr::Number("23".into())), Box::new(Expr::Number("65".into())))), Box::new(Expr::Number("68".into())))),

        unbalanced: [Token::LeftParen, Token::False, Token::False, Token::Eof] => Err(ParseError::UnbalancedDelims(Delim::Parenthesis)),
    }

    full_string_example! {
        str_negated_number: "-1" => "(- 1.0)",
        str_notted_bool: "!false" => "(! false)",
        str_bool: "true" => "true",
        str_int: "12" => "12.0",
        str_float: "12.13" => "12.13",
        str_nil: "nil" => "nil",
        str_string: "\"foo\"" => "foo",
        str_group: "(nil)" => "(group nil)",
        str_multiply: "1 * 1" => "(* 1.0 1.0)",
        str_divide: "1 / 1" => "(/ 1.0 1.0)",
        str_mixed: "23 * 65 / 68" => "(/ (* 23.0 65.0) 68.0)",
    }
}
