use crate::{
    evaluator::Evaluatable,
    object::{environment::Environment, object::NULL},
    parser::Parser,
};
use std::io;

const PROMPT: &[u8] = b">>";

pub fn start<R, W>(reader: &mut R, writer: &mut W)
where
    R: io::BufRead,
    W: io::Write,
{
    let mut env = Environment::new();
    loop {
        let mut buffer = String::new();
        writer.write(PROMPT).unwrap();
        writer.flush().unwrap();
        reader.read_line(&mut buffer).unwrap();
        // TODO: handle errors
        let statements: Vec<_> = Parser::new(buffer.as_str())
            .filter_map(|s| match s {
                Ok(s) => Some(s),
                _ => None,
            })
            .collect();
        let res = statements.evaluate(&mut env);
        if res.as_ref().kind() != NULL.kind() {
            write!(writer, "{res}\n",).unwrap();
        }
    }
}
