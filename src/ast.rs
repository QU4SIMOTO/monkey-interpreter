use crate::token::Token;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let {
        ident: Rc<String>,
        expression: Expression,
    },
    Return(Expression),
    Expression(Expression),
    Block(Block),
    Empty,
}

impl Statement {
    pub fn new_block(statements: Vec<Statement>) -> Self {
        Statement::Block(Block::new(statements))
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Block {
    statements: Vec<Statement>,
}

impl Block {
    pub fn new(statements: Vec<Statement>) -> Self {
        Block { statements }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Infix(InfixExpression),
    Prefix(PrefixExpression),
    IntegerLiteral(u64),
    Ident(Rc<String>),
    Boolean(bool),
    If(Box<IfExpression>),
    FunctionLiteral(Vec<Expression>, Block),
}

impl Expression {
    pub fn new_prefix(operator: PrefixOperator, operand: Self) -> Self {
        Self::Prefix(PrefixExpression {
            operator,
            operand: Box::from(operand),
        })
    }

    pub fn new_infix(operator: InfixOperator, lhs: Self, rhs: Self) -> Self {
        Self::Infix(InfixExpression {
            operator,
            operands: Box::from((lhs, rhs)),
        })
    }
}

impl From<u64> for Expression {
    fn from(value: u64) -> Self {
        Expression::IntegerLiteral(value)
    }
}

impl From<&str> for Expression {
    fn from(value: &str) -> Self {
        Expression::Ident(Rc::new(String::from(value)))
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

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub(crate) operator: PrefixOperator,
    pub(crate) operand: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub(crate) operator: InfixOperator,
    pub(crate) operands: Box<(Expression, Expression)>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub(crate) condition: Expression,
    pub(crate) consequence: Statement,
    pub(crate) alternative: Option<Statement>,
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
            // Call higher than prefix
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
            Statement::Empty => write!(f, ""),
            Statement::Block(block) => {
                write!(f, "{block}")
                /*
                f.write_str("{\n")?;
                for statement in statements {
                    write!(f, "{statement}\n")?;
                }
                f.write_str("}")
                */
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
            Expression::FunctionLiteral(args, block) => {
                let args = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "fn({args}){block}")
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
        write!(f, "({} {} {})", lhs, self.operator, rhs)
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
                consequence: Statement::Block(Block {
                    statements: vec![Statement::Return(Expression::from(2))]
                }),
                alternative: None
            }))
            .to_string(),
            "if (2) {\nreturn 2;\n}"
        );
        assert_eq!(
            Expression::If(Box::from(IfExpression {
                condition: Expression::from(2),
                consequence: Statement::Block(Block {
                    statements: vec![Statement::Return(Expression::from(2))]
                }),
                alternative: Some(Statement::Block(Block {
                    statements: vec![Statement::Expression(Expression::from(3),)]
                })),
            }))
            .to_string(),
            "if (2) {\nreturn 2;\n} else {\n3\n}"
        );
        assert_eq!(
            Expression::If(Box::from(IfExpression {
                condition: Expression::new_infix(
                    InfixOperator::LT,
                    Expression::from("x"),
                    Expression::from("y")
                ),
                consequence: Statement::new_block(vec![Statement::Expression(Expression::from(
                    "x"
                ))]),

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
                consequence: Statement::new_block(vec![Statement::Expression(Expression::from(
                    "x"
                ))]),
                alternative: Some(Statement::new_block(vec![Statement::Expression(
                    Expression::from("y")
                )]))
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
    }
}
