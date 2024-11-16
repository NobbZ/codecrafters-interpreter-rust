mod interface;
mod token;

use anyhow::Result;

use interface::{InterpreterCommand, InterpreterParser, TokenizeArgs};

fn main() -> Result<()> {
    let args = <InterpreterParser as clap::Parser>::parse();

    match args.command {
        InterpreterCommand::Tokenize(tok_args) => token::tokenize(tok_args)?,
    };

    Ok(())
}
