use crate::token::Token;
use core::iter;

pub struct Lexer<'a> {
    input: iter::Peekable<std::str::Chars<'a>>,
    have_produced_eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
            have_produced_eof: false,
        }
    }

    fn skip_whitespace(&mut self) -> Option<char> {
        self.input
            .by_ref()
            .skip_while(|c| c.is_ascii_whitespace())
            .next()
    }

    pub fn lex(input: &'a str) -> Vec<Token> {
        Self::new(input).collect()
    }

    fn get_ident(&mut self, first_c: char) -> Token {
        let ident: String = iter::once(first_c)
            .chain(iter::from_fn(|| {
                self.input.by_ref().next_if(|c| c.is_alphanumeric())
            }))
            .collect();
        Token::lookup_ident(ident)
    }

    fn get_int(&mut self, first_c: char) -> Token {
        let n: u64 = iter::once(first_c)
            .chain(iter::from_fn(|| {
                self.input.by_ref().next_if(|c| c.is_ascii_digit())
            }))
            .collect::<String>()
            .parse()
            .expect("Failed to parse int");
        Token::Int(n)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(c) = self.skip_whitespace() else {
            return if self.have_produced_eof {
                None
            } else {
                self.have_produced_eof = true;
                Some(Token::EOF)
            };
        };
        let token = match c {
            'a'..='z' | 'A'..='Z' => self.get_ident(c),
            '0'..='9' => self.get_int(c),
            '=' if self.input.next_if(|&c| c == '=').is_some() => Token::Eq,
            '=' => Token::Assign,
            '!' if self.input.next_if(|&c| c == '=').is_some() => Token::Neq,
            '!' => Token::Exclam,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::LT,
            '>' => Token::GT,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            _ => Token::Illegal,
        };
        Some(token)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn single_char_symbols() {
        assert_eq!(
            Lexer::lex("=+(){},!-/*<>;"),
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
    fn simple_program() {
        assert_eq!(
            Lexer::lex(
                "let five = 5;
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
            ),
            vec![
                Token::Let,
                Token::new_ident("five"),
                Token::Assign,
                Token::Int(5),
                Token::Semicolon,
                Token::Let,
                Token::new_ident("ten"),
                Token::Assign,
                Token::Int(10),
                Token::Semicolon,
                Token::Let,
                Token::new_ident("add"),
                Token::Assign,
                Token::Function,
                Token::Lparen,
                Token::new_ident("x"),
                Token::Comma,
                Token::new_ident("y"),
                Token::Rparen,
                Token::Lbrace,
                Token::new_ident("x"),
                Token::Plus,
                Token::new_ident("y"),
                Token::Semicolon,
                Token::Rbrace,
                Token::Let,
                Token::new_ident("result"),
                Token::Assign,
                Token::new_ident("add"),
                Token::Lparen,
                Token::new_ident("five"),
                Token::Comma,
                Token::new_ident("ten"),
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
    fn trailing_whitespace() {
        assert_eq!(
            Lexer::lex(
                " let x1 = 7;
                    let x2 =8;

                     "
            ),
            vec![
                Token::Let,
                Token::new_ident("x1"),
                Token::Assign,
                Token::Int(7),
                Token::Semicolon,
                Token::Let,
                Token::new_ident("x2"),
                Token::Assign,
                Token::Int(8),
                Token::Semicolon,
                Token::EOF
            ]
        );
    }
}
