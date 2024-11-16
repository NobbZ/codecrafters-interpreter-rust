mod interface;
mod token;

use anyhow::Result;

use interface::{InterpreterCommand, InterpreterParser, TokenizeArgs};

fn main() -> Result<(), usize> {
    let args = <InterpreterParser as clap::Parser>::parse();

    let ec = match args.command {
        InterpreterCommand::Tokenize(tok_args) => token::tokenize(tok_args).map_or(65, |_| 0),
    };

    if ec == 0 {
        Ok(())
    } else {
        Err(ec)
    }
}
