use crate::lexer::Lexer;
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
        let tokens: Vec<_> = Lexer::new(buffer.as_bytes()).collect();
        write!(writer, "{:?}\n", tokens).unwrap();
    }
}
