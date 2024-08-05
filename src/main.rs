use clap::Parser;
use monkey_interpreter::{evaluator::Evaluatable, object::environment::Environment, parser, repl};
use std::{
    fs,
    io::{self, prelude::*},
};

#[derive(Parser)]
#[command(version, about, long_about=None)]
struct Cli {
    path: Option<String>,
}

fn main() -> Result<(), io::Error> {
    let cli = Cli::parse();
    if let Some(path) = cli.path {
        let mut file = fs::File::open(path.as_str())?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        let mut env = Environment::new();
        let statements = parser::Parser::new(contents.as_str())
            .filter_map(|s| match s {
                Ok(s) => Some(s),
                _ => None,
            })
            .collect::<Vec<_>>();
        let res = statements.evaluate(&mut env);
        println!("{res}");
    } else {
        println!("Hello! This is the monkey programming language!\n");
        repl::start(&mut io::stdin().lock(), &mut io::stdout());
    }
    Ok(())
}
