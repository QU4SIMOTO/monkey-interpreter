use std::fmt;
use std::rc::Rc;

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
    Lbracket,
    Rbracket,
    Comma,
    Semicolon,
    EOF,
    Let,
    Ident(Rc<String>),
    Int(i64),
    Function,
    Eq,
    Neq,
    True,
    False,
    If,
    Else,
    Return,
    String(Rc<String>),
    Illegal,
}

impl AsRef<str> for Token {
    fn as_ref(&self) -> &str {
        match self {
            Self::Eq => "==",
            Self::Neq => "!=",
            Self::Assign => "=",
            Self::Asterisk => "*",
            Self::Slash => "/",
            Self::Exclam => "!",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::LT => "<",
            Self::GT => ">",
            Self::Lparen => "(",
            Self::Rparen => ")",
            Self::Lbrace => "{",
            Self::Rbrace => "}",
            Self::Lbracket => "[",
            Self::Rbracket => "]",
            Self::Comma => ",",
            Self::Semicolon => ";",
            Self::Let => "let",
            Self::Function => "fn",
            Self::True => "true",
            Self::False => "false",
            Self::If => "if",
            Self::Else => "else",
            Self::Return => "return",
            Self::Ident(_) => "ident",
            Self::Int(_) => "int",
            Self::EOF => "EOF",
            Self::String(_) => "string",
            Self::Illegal => "0",
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
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
            _ => Token::Ident(ident.into()),
        }
    }

    pub(crate) fn is_same_variant(&self, other: impl AsRef<str>) -> bool {
        if let Token::Ident(_) = self {
            other.as_ref() == "ident"
        } else {
            self.as_ref() == other.as_ref()
        }
    }

    pub(crate) fn new_ident(ident: impl Into<String>) -> Self {
        Self::Ident(Rc::new(ident.into()))
    }

    pub fn new_string(ident: impl Into<String>) -> Self {
        Self::String(Rc::new(ident.into()))
    }
}
