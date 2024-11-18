mod interface;
mod parser;
mod token;
mod token_stream;

use std::process::ExitCode;

use interface::{InterpreterCommand, InterpreterParser, TokenizeArgs};

fn main() -> ExitCode {
    let args = <InterpreterParser as clap::Parser>::parse();

    use InterpreterCommand::*;

    let ec = match args.command {
        Tokenize(tok_args) => token::tokenize(tok_args).map_or(65, |_| 0),
        Parse(parse_args) => parser::parse_file(parse_args.file).map_or(65, |_| 0),
    };

    ExitCode::from(ec)
}
