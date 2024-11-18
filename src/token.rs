use std::{char, fmt::Debug};
use std::{fmt, fs};

use anyhow::{ensure, Context, Result};

use crate::token_stream::TokenStream;
use crate::TokenizeArgs;

#[derive(Debug, Clone, Copy)]
pub enum TokenError {
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
pub(crate) enum NumberType {
    Integer(String),
    Float(String),
}

impl NumberType {
    pub(crate) fn new<S>(s: S, float: bool) -> Self
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
pub(crate) enum Token {
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
    Ident(String),

    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
    Skip,
    NewLine,
}

impl Token {
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
            Ident(_) => "IDENTIFIER",

            And => "AND",
            Class => "CLASS",
            Else => "ELSE",
            False => "FALSE",
            For => "FOR",
            Fun => "FUN",
            If => "IF",
            Nil => "NIL",
            Or => "OR",
            Print => "PRINT",
            Return => "RETURN",
            Super => "SUPER",
            This => "THIS",
            True => "TRUE",
            Var => "VAR",
            While => "WHILE",

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
            Ident(s) => s.clone(),

            And => "and".to_string(),
            Class => "class".to_string(),
            Else => "else".to_string(),
            False => "false".to_string(),
            For => "for".to_string(),
            Fun => "fun".to_string(),
            If => "if".to_string(),
            Nil => "nil".to_string(),
            Or => "or".to_string(),
            Print => "print".to_string(),
            Return => "return".to_string(),
            Super => "super".to_string(),
            This => "this".to_string(),
            True => "true".to_string(),
            Var => "var".to_string(),
            While => "while".to_string(),

            Eof => "".to_string(),
            Skip | NewLine => unreachable!("This tokens should never be linified"),
        }
    }

    fn literal(&self) -> String {
        use Token::*;

        match self {
            LeftParen | RightParen | LeftBrace | RightBrace | Star | Dot | Comma | Plus | Minus
            | Semicolon | Slash | Eq | EqEq | Bang | BangEq | Lt | LtEq | Gt | GtEq | Ident(_)
            | And | Class | Else | False | For | Fun | If | Nil | Or | Print | Return | Super
            | This | True | Var | While | Eof => "null".to_string(),
            String(s) => s.clone(),
            Number(NumberType::Float(s)) => {
                let f = s
                    .parse::<f64>()
                    .unwrap_or_else(|_| unreachable!("Should always be a good float"))
                    .to_string();
                if f.contains('.') {
                    f
                } else {
                    format!("{}.0", f)
                }
            }
            Number(NumberType::Integer(s)) => format!("{}.0", s),
            Skip | NewLine => unreachable!("This tokens should never be linified"),
        }
    }
}

pub fn tokenize(args: TokenizeArgs) -> Result<()> {
    let file_contents = fs::read_to_string(&args.file)
        .with_context(|| format!("failed to read {:?}", &args.file))?;

    let mut err_cnt = 0;

    let tokens = TokenStream::from(file_contents.chars());

    for token in tokens {
        match token {
            Ok(token) => println!("{}", token.to_terminal()),
            Err((line, token_error)) => {
                eprintln!("[line {}] Error: {}", line, token_error);
                err_cnt += 1;
            }
        }
    }

    ensure!(err_cnt == 0);

    Ok(())
}
