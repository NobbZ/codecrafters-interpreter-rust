mod interface;

use std::fs;

use anyhow::{bail, Context, Result};

use interface::{InterpreterCommand, InterpreterParser};

fn main() -> Result<()> {
    let args = <InterpreterParser as clap::Parser>::parse();

    match args.command {
        InterpreterCommand::Tokenize(tok_args) => {
            let file_contents = fs::read_to_string(&tok_args.file)
                .with_context(|| format!("failed to read {:?}", &tok_args.file))?;

            if !file_contents.is_empty() {
                bail!("Scanner not implemented");
            } else {
                println!("EOF  null");
            }
        }
    }

    Ok(())
}
