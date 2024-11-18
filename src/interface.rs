use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

#[derive(Parser, Debug)]
pub struct InterpreterParser {
    #[command(subcommand)]
    pub command: InterpreterCommand,
}

#[derive(Subcommand, Debug)]
pub enum InterpreterCommand {
    /// Tokenizes a given file and prints out the tokens
    Tokenize(TokenizeArgs),

    /// Parses a given file and prints out its representation
    Parse(ParseArgs),
}

#[derive(Args, Debug)]
pub struct TokenizeArgs {
    /// File to tokenize
    #[arg()]
    pub file: PathBuf,
}

#[derive(Args, Debug)]
pub struct ParseArgs {
    /// File to parse
    #[arg()]
    pub file: PathBuf,
}
