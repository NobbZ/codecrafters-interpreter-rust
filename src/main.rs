mod interface;
mod token;

use std::process::ExitCode;

use interface::{InterpreterCommand, InterpreterParser, TokenizeArgs};

fn main() -> ExitCode {
    let args = <InterpreterParser as clap::Parser>::parse();

    let ec = match args.command {
        InterpreterCommand::Tokenize(tok_args) => token::tokenize(tok_args).map_or(65, |_| 0),
    };

    ExitCode::from(ec)
}
