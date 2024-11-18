use std::path::Path;
use std::{fmt::Display, fs, iter::Peekable};

use crate::{token::Token, token_stream::TokenStream};

pub(crate) enum Expr {
    Bool(bool),
    Nil,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "nil"),
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

        for token in self.tokens.by_ref() {
            let expr = match token {
                Token::True => anyhow::Ok(Expr::Bool(true)),
                Token::False => Ok(Expr::Bool(false)),
                Token::Nil => Ok(Expr::Nil),
                Token::Eof => return Ok(exprs),
                _ => unimplemented!(),
            };

            exprs.push(expr?);
        }

        unreachable!("There should always be a EOF")
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

