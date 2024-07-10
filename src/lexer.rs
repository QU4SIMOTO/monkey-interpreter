#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Assign,
    Asterisk,
    Slash,
    Exclam,
    Plus,
    Minus,
    LT,
    GT,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Comma,
    Semicolon,
    EOF,
    Let,
    Ident(&'a [u8]),
    Int(u64),
    Function,
    Eq,
    Neq,
    True,
    False,
    If,
    Else,
    Return,
    Illegal,
}

impl<'a> Token<'a> {
    fn len(&self) -> usize {
        match self {
            Token::Let => 3,
            Token::Illegal => 1,
            Token::Ident(ident) => ident.len(),
            Token::Int(n) => (n.checked_ilog10().unwrap_or(0) + 1) as usize,
            Token::Function => 2,
            Token::EOF => 1,
            Token::Eq => 2,
            Token::Neq => 2,
            Token::True => 4,
            Token::False => 5,
            Token::If => 2,
            Token::Else => 4,
            Token::Return => 6,
            _ => 1,
        }
    }

    fn lookup_ident(ident: &'a [u8]) -> Self {
        match ident {
            b"let" => Token::Let,
            b"fn" => Token::Function,
            b"true" => Token::True,
            b"false" => Token::False,
            b"if" => Token::If,
            b"else" => Token::Else,
            b"return" => Token::Return,
            _ => Token::Ident(ident),
        }
    }

    fn lookup_int(int: &'a [u8]) -> Self {
        let int = std::str::from_utf8(int).expect("not UTF-8");
        Token::Int(int.parse().expect("not a number"))
    }
}

pub struct Lexer<'a> {
    input: &'a [u8],
    position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        Lexer { input, position: 0 }
    }

    fn skip_whitespace(&mut self) {
        if self.position >= self.input.len() || !self.input[self.position].is_ascii_whitespace() {
            return;
        }
        for ch in self.input[self.position..].iter() {
            if !ch.is_ascii_whitespace() {
                return;
            }
            self.position += 1;
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position > self.input.len() {
            return None;
        }
        self.skip_whitespace();
        let token = if self.position >= self.input.len() {
            Token::EOF
        } else {
            match self.input[self.position] {
                b'a'..=b'z' | b'A'..=b'Z' => {
                    let end = self.input[self.position..]
                        .iter()
                        .position(|&ch| !ch.is_ascii_lowercase() && !ch.is_ascii_uppercase())
                        .unwrap_or(self.input.len() - 1);
                    Token::lookup_ident(&self.input[self.position..self.position + end])
                }
                b'0'..=b'9' => {
                    let end = self.input[self.position..]
                        .iter()
                        .position(|&ch| !ch.is_ascii_digit())
                        .unwrap_or(self.input.len() - 1);
                    Token::lookup_int(&self.input[self.position..self.position + end])
                }
                b'=' => {
                    if self.input.get(self.position + 1) == Some(&b'=') {
                        Token::Eq
                    } else {
                        Token::Assign
                    }
                }
                b'+' => Token::Plus,
                b'-' => Token::Minus,
                b'!' => {
                    if self.input.get(self.position + 1) == Some(&b'=') {
                        Token::Neq
                    } else {
                        Token::Exclam
                    }
                }
                b'*' => Token::Asterisk,
                b'/' => Token::Slash,
                b'<' => Token::LT,
                b'>' => Token::GT,
                b'(' => Token::Lparen,
                b')' => Token::Rparen,
                b'{' => Token::Lbrace,
                b'}' => Token::Rbrace,
                b',' => Token::Comma,
                b';' => Token::Semicolon,
                _ => Token::Illegal,
            }
        };
        self.position += token.len();
        Some(token)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn single_char_symbols() {
        assert_eq!(
            Lexer::new(b"=+(){},!-/*<>;").collect::<Vec<_>>(),
            vec![
                Token::Assign,
                Token::Plus,
                Token::Lparen,
                Token::Rparen,
                Token::Lbrace,
                Token::Rbrace,
                Token::Comma,
                Token::Exclam,
                Token::Minus,
                Token::Slash,
                Token::Asterisk,
                Token::LT,
                Token::GT,
                Token::Semicolon,
                Token::EOF,
            ]
        )
    }

    #[test]
    fn simple_prog() {
        assert_eq!(
            Lexer::new(
                b"let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            }
            let result = add(five, ten);
            if (5 < 10) {
                return true;
            } else {
                return false;
            }
            10 == 10;
            10 != 9;"
            )
            .collect::<Vec<_>>(),
            vec![
                Token::Let,
                Token::Ident(b"five"),
                Token::Assign,
                Token::Int(5),
                Token::Semicolon,
                Token::Let,
                Token::Ident(b"ten"),
                Token::Assign,
                Token::Int(10),
                Token::Semicolon,
                Token::Let,
                Token::Ident(b"add"),
                Token::Assign,
                Token::Function,
                Token::Lparen,
                Token::Ident(b"x"),
                Token::Comma,
                Token::Ident(b"y"),
                Token::Rparen,
                Token::Lbrace,
                Token::Ident(b"x"),
                Token::Plus,
                Token::Ident(b"y"),
                Token::Semicolon,
                Token::Rbrace,
                Token::Let,
                Token::Ident(b"result"),
                Token::Assign,
                Token::Ident(b"add"),
                Token::Lparen,
                Token::Ident(b"five"),
                Token::Comma,
                Token::Ident(b"ten"),
                Token::Rparen,
                Token::Semicolon,
                Token::If,
                Token::Lparen,
                Token::Int(5),
                Token::LT,
                Token::Int(10),
                Token::Rparen,
                Token::Lbrace,
                Token::Return,
                Token::True,
                Token::Semicolon,
                Token::Rbrace,
                Token::Else,
                Token::Lbrace,
                Token::Return,
                Token::False,
                Token::Semicolon,
                Token::Rbrace,
                Token::Int(10),
                Token::Eq,
                Token::Int(10),
                Token::Semicolon,
                Token::Int(10),
                Token::Neq,
                Token::Int(9),
                Token::Semicolon,
                Token::EOF,
            ]
        )
    }

    #[test]
    fn trailing_space() {
        assert_eq!(
            Lexer::new(
                b"
                     let x = 7;

                     "
            )
            .collect::<Vec<_>>(),
            vec![
                Token::Let,
                Token::Ident(b"x"),
                Token::Assign,
                Token::Int(7),
                Token::Semicolon,
                Token::EOF
            ]
        );
    }
}
