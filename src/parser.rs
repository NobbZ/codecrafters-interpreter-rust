use std::path::Path;
use std::{fmt::Display, fs, iter::Peekable};

use crate::{token::Token, token_stream::TokenStream};

type ParseResult<'de> = Result<Expr<'de>, ParseError>;

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
pub(crate) enum Expr<'de> {
    Bool(bool),
    Number(&'de str),
    String(&'de str),
    Nil,
    Group(Box<Expr<'de>>),
    Not(Box<Expr<'de>>),
    Neg(Box<Expr<'de>>),
    Binary(Op, Box<Expr<'de>>, Box<Expr<'de>>),
}

impl<'de> Display for Expr<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Number(n) => {
                let n: f64 = n.parse().unwrap();
                let s = n.to_string();
                if s.contains('.') {
                    write!(f, "{}", s)
                } else {
                    write!(f, "{}.0", s)
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
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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
    // TODO: get rid of this…
    toplevel: bool,
}

impl<'de, T> Parser<'de, T>
where
    T: Iterator<Item = Token<'de>>,
{
    pub fn new(token_stream: T) -> Self {
        Self {
            tokens: token_stream.peekable(),
            toplevel: true,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Expr<'de>>, ParseError> {
        let mut exprs = Vec::new();

        while let Some(expr) = self.expr() {
            exprs.push(expr?);
        }

        Ok(exprs)
    }

    fn expr(&mut self) -> Option<ParseResult<'de>> {
        let mut expr = self
            .literal()
            .or_else(|| self.unary())
            .or_else(|| self.group())?;
        let Ok(ref mut lhs) = expr else {
            return Some(expr);
        };

        if self.toplevel {
            self.toplevel = false;

            while let Some(Token::Star | Token::Slash) = self.tokens.peek() {
                let op = self.tokens.next()?;
                let expr = self.expr()?;
                let Ok(rhs) = expr else { return Some(expr) };

                let actual_op = match op {
                    Token::Star => Op::Mul,
                    Token::Slash => Op::Div,
                    _ => unreachable!(),
                };

                *lhs = Expr::Binary(actual_op, Box::new(lhs.clone()), Box::new(rhs));
            }

            self.toplevel = true;

            Some(Ok(lhs.clone()))
        } else {
            Some(expr)
        }
    }

    fn literal(&mut self) -> Option<ParseResult<'de>> {
        let lit = match self.tokens.peek() {
            Some(Token::True) => Some(Ok(Expr::Bool(true))),
            Some(Token::False) => Some(Ok(Expr::Bool(false))),
            Some(Token::Number(f)) => Some(Ok(Expr::Number(f))),
            Some(Token::String(s)) => Some(Ok(Expr::String(s))),
            Some(Token::Nil) => Some(Ok(Expr::Nil)),
            Some(_) => None,
            None => None,
        };

        if lit.is_some() {
            self.tokens.next();
        };

        lit
    }

    fn unary(&mut self) -> Option<ParseResult<'de>> {
        if ![Some(&Token::Bang), Some(&Token::Minus)].contains(&self.tokens.peek()) {
            return None;
        }

        let c = match self.tokens.next() {
            Some(Token::Minus) => Expr::Neg,
            Some(Token::Bang) => Expr::Not,
            _ => unreachable!(),
        };

        let expr = self.expr()?;

        match expr {
            Ok(e) => Some(Ok(c(Box::new(e)))),
            err @ Err(_) => Some(err),
        }
    }

    fn group(&mut self) -> Option<ParseResult<'de>> {
        if self.tokens.peek() == Some(&Token::LeftParen) {
            self.tokens.next();
            let toplevel = self.toplevel;
            self.toplevel = true;
            let inner = self.expr()?;

            let e = match inner {
                Ok(e) => e,
                err @ Err(_) => return Some(err),
            };

            self.toplevel = toplevel;

            if self.tokens.next() == Some(Token::RightParen) {
                return Some(Ok(Expr::Group(Box::new(e))));
            } else {
                return Some(Err(ParseError::UnbalancedDelims(Delim::Parenthesis)));
            }
        }

        None
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

                    assert_eq!(Some($expected), parser.expr());

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

                    let printed = format!("{}", parser.expr().unwrap().unwrap());

                    assert_eq!($expected, printed);

                    Ok(())
                }
            )*
        };
    }

    example! {
        negated_number: [Token::Minus, Token::Number("1"), Token::Eof] => Ok(Expr::Neg(Box::new(Expr::Number("1")))),
        notted_bool: [Token::Bang, Token::False, Token::Eof] => Ok(Expr::Not(Box::new(Expr::Bool(false)))),
        bool: [Token::True, Token::Eof] => Ok(Expr::Bool(true)),
        int: [Token::Number("12"), Token::Eof] => Ok(Expr::Number("12")),
        float: [Token::Number("12.13"), Token::Eof] => Ok(Expr::Number("12.13")),
        nil: [Token::Nil, Token::Eof] => Ok(Expr::Nil),
        string: [Token::String("foo"), Token::Eof] => Ok(Expr::String("foo")),
        group: [Token::LeftParen, Token::Nil, Token::RightParen, Token::Eof] => Ok(Expr::Group(Box::new(Expr::Nil))),
        multiply: [Token::Number("1"), Token::Star, Token::Number("1"), Token::Eof] => Ok(Expr::Binary(Op::Mul, Box::new(Expr::Number("1")), Box::new(Expr::Number("1")))),
        divide: [Token::Number("1"), Token::Slash, Token::Number("1"), Token::Eof] => Ok(Expr::Binary(Op::Div, Box::new(Expr::Number("1")), Box::new(Expr::Number("1")))),

        mixed: [
            Token::Number("23"),
            Token::Star,
            Token::Number("65"),
            Token::Slash,
            Token::Number("68"),
        ] => Ok(Expr::Binary(Op::Div, Box::new(Expr::Binary(Op::Mul, Box::new(Expr::Number("23")), Box::new(Expr::Number("65")))), Box::new(Expr::Number("68")))),

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
        str_complex_with_sign: "(67 * -28 / (68 * 64))" => "(group (/ (* 67.0 (- 28.0)) (group (* 68.0 64.0))))",
        str_zero_float: "0.0" => "0.0",
        str_zero_int: "0" => "0.0",
    }
}
