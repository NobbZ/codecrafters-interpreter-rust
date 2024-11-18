mod interface;
mod token;
mod token_stream;

use std::process::ExitCode;

use interface::{InterpreterCommand, InterpreterParser, TokenizeArgs};

fn main() -> ExitCode {
    let args = <InterpreterParser as clap::Parser>::parse();

    use InterpreterCommand::*;

    let ec = match args.command {
        Tokenize(tok_args) => token::tokenize(tok_args).map_or(65, |_| 0),
        Parse(parse_args) => unimplemented!("parsing is not yet implemented, but got called with {:?}", parse_args),
    };

    ExitCode::from(ec)
}
