#[derive(Debug, PartialEq, Clone)]
pub enum Token {
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
    Ident(String),
    Int(i64),
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

impl AsRef<str> for Token {
    fn as_ref(&self) -> &str {
        match self {
            Token::Eq => "==",
            Token::Neq => "!=",
            Token::Assign => "=",
            Token::Asterisk => "*",
            Token::Slash => "/",
            Token::Exclam => "!",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::LT => "<",
            Token::GT => ">",
            Token::Lparen => "(",
            Token::Rparen => ")",
            Token::Lbrace => "{",
            Token::Rbrace => "}",
            Token::Comma => ",",
            Token::Semicolon => ";",
            Token::Let => "let",
            Token::Function => "fn",
            Token::True => "true",
            Token::False => "false",
            Token::If => "if",
            Token::Else => "else",
            Token::Return => "return",
            Token::Ident(_) => "ident",
            Token::Int(_) => "int",
            Token::EOF => "EOF",
            Token::Illegal => "0",
        }
    }
}

impl Token {
    pub(crate) fn lookup_ident(ident: String) -> Self {
        match ident.as_str() {
            "let" => Token::Let,
            "fn" => Token::Function,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Ident(ident),
        }
    }

    pub(crate) fn is_same_variant(&self, other: impl AsRef<str>) -> bool {
        self.as_ref() == other.as_ref()
    }
}
