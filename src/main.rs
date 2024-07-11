use monkey_interpreter::repl::start;
use std::io;

fn main() {
    println!("Hello! This is the monkey programming language!\n");
    start(&mut io::stdin().lock(), &mut io::stdout());
}
