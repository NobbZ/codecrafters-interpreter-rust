use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

#[derive(Parser, Debug)]
pub struct InterpreterParser {
    #[command(subcommand)]
    pub command: InterpreterCommand,
}

#[derive(Subcommand, Debug)]
pub enum InterpreterCommand {
    /// Tokenizes a given fule and prints out the tokens
   Tokenize(TokenizeArgs),
}

#[derive(Args, Debug)]
pub struct TokenizeArgs {
    /// File to tokenize
    #[arg()]
    pub file: PathBuf,
}
