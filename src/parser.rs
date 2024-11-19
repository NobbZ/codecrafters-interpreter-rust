use std::path::Path;
use std::{fmt::Display, fs, iter::Peekable};

use anyhow::anyhow;

use crate::{
    token::{NumberType, Token},
    token_stream::TokenStream,
};

#[derive(Clone)]
pub(crate) enum Number {
    Int(i64),
    Float(f64),
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Number::Int(i) => i.to_string(),
            Number::Float(f) => f.to_string(),
        };

        if s.contains('.') {
            write!(f, "{}", s)
        } else {
            write!(f, "{}.0", s)
        }
    }
}

#[derive(Clone)]
pub(crate) enum Expr {
    Bool(bool),
    Number(Number),
    String(String),
    Nil,
    Group(Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Number(n) => write!(f, "{}", n),
            Self::String(s) => write!(f, "{}", s),
            Self::Nil => write!(f, "nil"),
            Self::Group(e) => write!(f, "(group {})", e),
        }
    }
}

pub(crate) struct Parser<T>
where
    T: Iterator<Item = Token>,
{
    tokens: Peekable<T>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(token_stream: T) -> Self {
        Self {
            tokens: token_stream.peekable(),
        }
    }

    pub fn parse(&mut self) -> anyhow::Result<Vec<Expr>> {
        let mut exprs = Vec::new();

        while let Some(expr) = self.parse_next() {
            exprs.push(expr?);
        }

        Ok(exprs)
    }

    fn parse_next(&mut self) -> Option<anyhow::Result<Expr>> {
        use self::Number as N;
        use NumberType as NT;
        use Token::*;

        match self.tokens.next() {
            Some(True) => Some(Ok(Expr::Bool(true))),
            Some(False) => Some(Ok(Expr::Bool(false))),
            Some(Number(NT::Integer(i))) => Some(Ok(Expr::Number(N::Int(i.parse().unwrap())))),
            Some(Number(NT::Float(f))) => Some(Ok(Expr::Number(N::Float(f.parse().unwrap())))),
            Some(String(s)) => Some(Ok(Expr::String(s.clone()))),
            Some(Nil) => Some(Ok(Expr::Nil)),
            Some(LeftParen) => match self.parse_next() {
                Some(Ok(expr)) => {
                    if let Some(RightParen) = self.tokens.next() {
                        Some(Ok(Expr::Group(Box::new(expr))))
                    } else {
                        Some(Err(anyhow!("unbalanced parenthesis")))
                    }
                }
                err @ Some(Err(_)) => err,
                None => Some(Err(anyhow!("unbalanced parenthesis"))),
            },
            Some(Eof) => None,
            _ => unimplemented!(),
        }
    }
}

pub fn parse_file(file: impl AsRef<Path>) -> anyhow::Result<()> {
    let content = fs::read_to_string(file)?;
    let ts = TokenStream::from(content.chars()).filter_map(|t| match t {
        Ok(t) => Some(t),
        Err(_) => None,
    });

    let exprs = Parser::new(ts).parse()?;

    for expr in exprs.iter() {
        println!("{}", expr);
    }

    Ok(())
}
