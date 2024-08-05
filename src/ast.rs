use crate::token::Token;
use std::{fmt, rc::Rc};

pub type Program = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        ident: Rc<String>,
        expression: Expression,
    },
    Return(Expression),
    Expression(Expression),
    Block(Block),
}

impl Statement {
    pub fn new_block(statements: Vec<Statement>) -> Self {
        Statement::Block(Block::new(statements))
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

impl Block {
    pub fn new(statements: impl Into<Vec<Statement>>) -> Self {
        Block {
            statements: statements.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Infix(InfixExpression),
    Prefix(PrefixExpression),
    IntegerLiteral(i64),
    Ident(Rc<String>),
    Boolean(bool),
    If(Box<IfExpression>),
    FunctionLiteral(Vec<Rc<String>>, Block),
    Call(Box<Expression>, Vec<Expression>),
    StringLiteral(Rc<String>),
    ArrayLiteral(Vec<Expression>),
    Index(Box<IndexExpression>),
    HashLiteral(Vec<(Rc<Expression>, Rc<Expression>)>),
}

impl Expression {
    pub fn new_prefix(operator: PrefixOperator, operand: impl Into<Self>) -> Self {
        Self::Prefix(PrefixExpression {
            operator,
            operand: Box::from(operand.into()),
        })
    }

    pub fn new_infix(operator: InfixOperator, lhs: impl Into<Self>, rhs: impl Into<Self>) -> Self {
        Self::Infix(InfixExpression {
            operator,
            operands: Box::from((lhs.into(), rhs.into())),
        })
    }
    pub fn new_if_else(condition: impl Into<Self>, consequence: Block, alternative: Block) -> Self {
        Self::If(Box::from(IfExpression {
            condition: condition.into(),
            consequence,
            alternative: Some(alternative),
        }))
    }

    pub fn new_if(condition: impl Into<Self>, consequence: Block) -> Self {
        Self::If(Box::from(IfExpression {
            condition: condition.into(),
            consequence,
            alternative: None,
        }))
    }

    pub fn new_index(lhs: impl Into<Self>, index: impl Into<Self>) -> Self {
        Self::Index(Box::from(IndexExpression {
            lhs: lhs.into(),
            index: index.into(),
        }))
    }
}

impl From<i64> for Expression {
    fn from(value: i64) -> Self {
        Expression::IntegerLiteral(value)
    }
}

impl From<&str> for Expression {
    fn from(value: &str) -> Self {
        Expression::Ident(Rc::new(String::from(value)))
    }
}

impl From<bool> for Expression {
    fn from(value: bool) -> Self {
        Expression::Boolean(value)
    }
}

impl From<Rc<String>> for Expression {
    fn from(value: Rc<String>) -> Self {
        Expression::Ident(value.clone())
    }
}

impl From<String> for Expression {
    fn from(value: String) -> Self {
        Expression::Ident(Rc::new(value))
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub(crate) operator: PrefixOperator,
    pub(crate) operand: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub(crate) operator: InfixOperator,
    pub(crate) operands: Box<(Expression, Expression)>,
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub(crate) condition: Expression,
    pub(crate) consequence: Block,
    pub(crate) alternative: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct IndexExpression {
    pub(crate) lhs: Expression,
    pub(crate) index: Expression,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PrefixOperator {
    Not,
    Minus,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum InfixOperator {
    Plus,
    Minus,
    Mul,
    Divide,
    Eq,
    Neq,
    LT,
    GT,
    Call,
    Index,
}

impl TryFrom<&Token> for PrefixOperator {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Exclam => Ok(PrefixOperator::Not),
            Token::Minus => Ok(PrefixOperator::Minus),
            _ => Err(()),
        }
    }
}

impl TryFrom<&Token> for InfixOperator {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Plus => Ok(InfixOperator::Plus),
            Token::Minus => Ok(InfixOperator::Minus),
            Token::Asterisk => Ok(InfixOperator::Mul),
            Token::Slash => Ok(InfixOperator::Divide),
            Token::Eq => Ok(InfixOperator::Eq),
            Token::Neq => Ok(InfixOperator::Neq),
            Token::LT => Ok(InfixOperator::LT),
            Token::GT => Ok(InfixOperator::GT),
            Token::Lparen => Ok(InfixOperator::Call),
            Token::Lbracket => Ok(InfixOperator::Index),
            _ => Err(()),
        }
    }
}

pub(crate) trait Precendence {
    fn precedence(&self) -> u8;
}

impl Precendence for PrefixOperator {
    fn precedence(&self) -> u8 {
        5
    }
}

pub const LOWEST_PRECEDENCE: u8 = 0;

impl Precendence for InfixOperator {
    fn precedence(&self) -> u8 {
        match self {
            InfixOperator::Eq | InfixOperator::Neq => 1,
            InfixOperator::LT | InfixOperator::GT => 2,
            InfixOperator::Plus | InfixOperator::Minus => 3,
            InfixOperator::Mul | InfixOperator::Divide => 4,
            InfixOperator::Call => 5,
            InfixOperator::Index => 6,
        }
    }
}

impl fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrefixOperator::Not => write!(f, "!"),
            PrefixOperator::Minus => write!(f, "-"),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let { ident, expression } => write!(f, "let {ident} = {expression};"),
            Statement::Return(expression) => write!(f, "return {expression};"),
            Statement::Expression(expression) => write!(f, "{expression}"),
            Statement::Block(block) => {
                write!(f, "{block}")
            }
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("{\n")?;
        for statement in self.statements.iter() {
            write!(f, "{statement}\n")?;
        }
        f.write_str("}")
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::IntegerLiteral(n) => write!(f, "{n}"),
            Expression::Ident(ident) => write!(f, "{ident}"),
            Expression::Infix(e) => write!(f, "{e}"),
            Expression::Prefix(e) => write!(f, "{e}"),
            Expression::Boolean(b) => write!(f, "{b}"),
            Expression::If(if_expresion) => {
                write!(f, "if ({}) ", if_expresion.as_ref().condition)?;
                write!(f, "{}", if_expresion.as_ref().consequence)?;
                if let Some(ref expression) = if_expresion.as_ref().alternative {
                    write!(f, " else {expression}")?;
                }
                Ok(())
            }
            Expression::FunctionLiteral(parameters, block) => {
                let parameters = parameters
                    .iter()
                    .map(|parameter| parameter.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "fn({parameters}){block}")
            }
            Expression::Call(function, arguments) => {
                let arguments = arguments
                    .iter()
                    .map(|argument| argument.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{function}({arguments})")
            }
            Expression::StringLiteral(s) => write!(f, "\"{s}\""),
            Expression::ArrayLiteral(items) => {
                let items = items
                    .iter()
                    .map(|item| item.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{items}]")
            }
            Expression::Index(e) => {
                let IndexExpression { lhs, index } = e.as_ref();
                write!(f, "({lhs}[{index}])")
            }
            Expression::HashLiteral(map) => {
                let pairs = map
                    .iter()
                    .map(|(key, value)| format!("{key}: {value}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{{pairs}}}")
            }
        }
    }
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.operand)
    }
}
impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (lhs, rhs) = self.operands.as_ref();
        let operator = self.operator;
        write!(f, "({lhs} {operator} {rhs})")
    }
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InfixOperator::Plus => write!(f, "+"),
            InfixOperator::Minus => write!(f, "-"),
            InfixOperator::Mul => write!(f, "*"),
            InfixOperator::Divide => write!(f, "/"),
            InfixOperator::Eq => write!(f, "=="),
            InfixOperator::Neq => write!(f, "!="),
            InfixOperator::LT => write!(f, "<"),
            InfixOperator::GT => write!(f, ">"),
            InfixOperator::Call => write!(f, "call"),
            InfixOperator::Index => write!(f, "index"),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn display_let_statement() {
        assert_eq!(
            Statement::Let {
                ident: String::from("x").into(),
                expression: Expression::from(10),
            }
            .to_string(),
            "let x = 10;"
        );
        assert_eq!(
            Statement::Let {
                ident: String::from("x").into(),
                expression: Expression::from("y")
            }
            .to_string(),
            "let x = y;"
        );
        assert_eq!(
            Statement::Let {
                ident: String::from("foo").into(),
                expression: Expression::new_prefix(PrefixOperator::Minus, Expression::from(2))
            }
            .to_string(),
            "let foo = (-2);"
        );
        assert_eq!(
            Statement::Let {
                ident: String::from("fooBAR").into(),
                expression: Expression::new_infix(
                    InfixOperator::Mul,
                    Expression::new_infix(
                        InfixOperator::Mul,
                        Expression::new_prefix(PrefixOperator::Minus, Expression::from(4)),
                        Expression::new_prefix(PrefixOperator::Minus, Expression::from(6))
                    ),
                    Expression::new_infix(
                        InfixOperator::Plus,
                        Expression::new_prefix(PrefixOperator::Minus, Expression::from(7)),
                        Expression::from("y")
                    )
                )
            }
            .to_string(),
            "let fooBAR = (((-4) * (-6)) * ((-7) + y));"
        );
    }

    #[test]
    fn display_return_statement() {
        assert_eq!(
            Statement::Return(Expression::from(10)).to_string(),
            "return 10;"
        );
        assert_eq!(
            Statement::Return(Expression::from("x")).to_string(),
            "return x;"
        );
        assert_eq!(
            Statement::Return(Expression::new_prefix(
                PrefixOperator::Minus,
                Expression::from(5)
            ))
            .to_string(),
            "return (-5);"
        );
        assert_eq!(
            Statement::Return(Expression::new_infix(
                InfixOperator::Divide,
                Expression::new_infix(
                    InfixOperator::Plus,
                    Expression::from(5),
                    Expression::from(10)
                ),
                Expression::new_infix(
                    InfixOperator::Minus,
                    Expression::from("x"),
                    Expression::from("y")
                )
            ))
            .to_string(),
            "return ((5 + 10) / (x - y));"
        );
    }

    #[test]
    fn display_expression_statement() {
        assert_eq!(
            Statement::Expression(Expression::from(10)).to_string(),
            "10"
        );
        assert_eq!(
            Statement::Expression(Expression::new_prefix(
                PrefixOperator::Not,
                Expression::from("x")
            ))
            .to_string(),
            "(!x)"
        );
        assert_eq!(
            Statement::Expression(Expression::new_prefix(
                PrefixOperator::Not,
                Expression::new_infix(
                    InfixOperator::GT,
                    Expression::new_infix(
                        InfixOperator::Plus,
                        Expression::from(2),
                        Expression::from(3)
                    ),
                    Expression::new_infix(
                        InfixOperator::Mul,
                        Expression::from(2),
                        Expression::from(3)
                    ),
                )
            ))
            .to_string(),
            "(!((2 + 3) > (2 * 3)))"
        );
    }

    #[test]
    fn display_block_statement() {
        assert_eq!(
            Statement::Block(Block {
                statements: vec![
                    Statement::Let {
                        ident: String::from("x").into(),
                        expression: Expression::new_prefix(
                            PrefixOperator::Minus,
                            Expression::from(2)
                        )
                    },
                    Statement::Let {
                        ident: String::from("y").into(),
                        expression: Expression::from(5)
                    },
                    Statement::Expression(Expression::new_infix(
                        InfixOperator::Plus,
                        Expression::from("x"),
                        Expression::from("y")
                    ))
                ]
            })
            .to_string(),
            "{\nlet x = (-2);\nlet y = 5;\n(x + y)\n}"
        )
    }

    #[test]
    fn display_if_expression() {
        assert_eq!(
            Expression::If(Box::from(IfExpression {
                condition: Expression::from(2),
                consequence: Block::new(vec![Statement::Return(Expression::from(2))]),
                alternative: None
            }))
            .to_string(),
            "if (2) {\nreturn 2;\n}"
        );
        assert_eq!(
            Expression::If(Box::from(IfExpression {
                condition: Expression::from(2),
                consequence: Block::new(vec![Statement::Return(Expression::from(2))]),
                alternative: Some(Block::new(vec![
                    Statement::Expression(Expression::from(3),)
                ])),
            }))
            .to_string(),
            "if (2) {\nreturn 2;\n} else {\n3\n}"
        );
        assert_eq!(
            Expression::If(Box::from(IfExpression {
                condition: Expression::new_infix(InfixOperator::LT, "x", "y",),
                consequence: Block::new(vec![Statement::Expression(Expression::from("x"))]),
                alternative: None,
            }))
            .to_string(),
            "if ((x < y)) {\nx\n}"
        );
        assert_eq!(
            Expression::If(Box::from(IfExpression {
                condition: Expression::new_infix(
                    InfixOperator::LT,
                    Expression::from("x"),
                    Expression::from("y")
                ),
                consequence: Block::new(vec![Statement::Expression(Expression::from("x"))]),
                alternative: Some(Block::new(vec![Statement::Expression(Expression::from(
                    "y"
                ))]))
            }))
            .to_string(),
            "if ((x < y)) {\nx\n} else {\ny\n}"
        );
    }

    #[test]
    fn display_function_literal() {
        assert_eq!(
            Expression::FunctionLiteral(vec![], Block::new(vec![]),).to_string(),
            "fn(){\n}"
        );
        assert_eq!(
            Expression::FunctionLiteral(
                vec![Rc::new(String::from("x")), Rc::new(String::from("y"))],
                Block::new(vec![Statement::Expression(Expression::new_infix(
                    InfixOperator::Plus,
                    Expression::from("x"),
                    Expression::from("y")
                ))]),
            )
            .to_string(),
            "fn(x, y){\n(x + y)\n}"
        );
    }

    #[test]
    fn display_call_expression() {
        assert_eq!(
            Expression::Call(
                Box::from(Expression::from("add")),
                vec![
                    Expression::from(1),
                    Expression::new_infix(InfixOperator::Mul, 2, 3),
                    Expression::new_infix(InfixOperator::Plus, 4, 5,),
                ]
            )
            .to_string(),
            "add(1, (2 * 3), (4 + 5))"
        );
    }

    #[test]
    fn display_string_literal() {
        assert_eq!(
            Expression::StringLiteral(Rc::new("foo".into())).to_string(),
            "\"foo\""
        )
    }

    #[test]
    fn display_array_literal() {
        assert_eq!(Expression::ArrayLiteral(vec![]).to_string(), "[]");
        assert_eq!(
            Expression::ArrayLiteral(vec![
                Expression::IntegerLiteral(1),
                Expression::IntegerLiteral(2),
                Expression::IntegerLiteral(3),
            ])
            .to_string(),
            "[1, 2, 3]"
        );
        assert_eq!(
            Expression::ArrayLiteral(vec![
                Expression::new_infix(InfixOperator::Plus, 2, 3),
                Expression::new_prefix(PrefixOperator::Not, Expression::Boolean(true)),
                Expression::ArrayLiteral(vec![])
            ])
            .to_string(),
            "[(2 + 3), (!true), []]"
        );
    }

    #[test]
    fn display_index_expression() {
        assert_eq!(
            Expression::new_index(
                Expression::Ident(Rc::new("foo".into())),
                Expression::new_infix(InfixOperator::Plus, 2, 3),
            )
            .to_string(),
            "(foo[(2 + 3)])"
        );
    }

    #[test]
    fn display_hash_literal() {
        assert_eq!(Expression::HashLiteral(Vec::new()).to_string(), "{}");
        assert_eq!(
            Expression::HashLiteral(vec!(
                (
                    Rc::new(Expression::from("foo")),
                    Rc::new(Expression::from(2))
                ),
                (
                    Rc::new(Expression::from("bar")),
                    Rc::new(Expression::from(4))
                )
            ))
            .to_string(),
            "{foo: 2, bar: 4}"
        );
    }
}
