use crate::parser::Parser;
use std::io;

const PROMPT: &[u8] = b">>";

pub fn start<R, W>(reader: &mut R, writer: &mut W)
where
    R: io::BufRead,
    W: io::Write,
{
    loop {
        let mut buffer = String::new();
        writer.write(PROMPT).unwrap();
        writer.flush().unwrap();
        reader.read_line(&mut buffer).unwrap();
        let statements: Vec<_> = Parser::new(buffer.as_str()).collect();
        write!(writer, "{:?}\n", statements).unwrap();
    }
}
