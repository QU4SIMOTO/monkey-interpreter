use crate::ast::{
    Block, Expression, IfExpression, InfixExpression, InfixOperator, PrefixExpression,
    PrefixOperator, Program, Statement,
};
use crate::object::{
    environment::Environment,
    object::{Object, FALSE, NULL, TRUE},
};
use std::borrow::Cow;

pub type Evaluation<'a> = Cow<'a, Object>;

impl From<Object> for Evaluation<'_> {
    fn from(value: Object) -> Self {
        Cow::Owned(value)
    }
}

impl<'a> From<&'a Object> for Evaluation<'a> {
    fn from(value: &'a Object) -> Self {
        Cow::Borrowed(value)
    }
}

pub trait Evaluatable<'a> {
    fn evaluate(&self, env: &'a mut Environment) -> Evaluation;
}

impl Evaluatable<'_> for Expression {
    fn evaluate(&self, env: &mut Environment) -> Evaluation {
        match self {
            Expression::IntegerLiteral(n) => Evaluation::from(Object::from(*n)),
            Expression::Boolean(true) => Evaluation::from(&TRUE),
            Expression::Boolean(false) => Evaluation::from(&FALSE),
            Expression::Prefix(e) => e.evaluate(env),
            Expression::Infix(e) => e.evaluate(env),
            Expression::If(e) => e.evaluate(env),
            Expression::Ident(i) => {
                // todo: fix this, shouldn't have to clone
                if let Some(v) = env.get(i) {
                    Cow::Owned(v.clone())
                } else {
                    Evaluation::from(Object::unknown_ident(i))
                }
            }
            Expression::Call(_, _) | Expression::FunctionLiteral(_, _) => {
                todo!()
            }
        }
    }
}

impl Evaluatable<'_> for PrefixExpression {
    fn evaluate(&self, env: &mut Environment) -> Evaluation {
        let rhs = self.operand.evaluate(env);
        match self.operator {
            PrefixOperator::Not => match *rhs {
                Object::Boolean { value: false, .. } | Object::Null { .. } => {
                    Evaluation::from(&TRUE)
                }
                _ => Evaluation::from(&FALSE),
            },
            PrefixOperator::Minus => match *rhs {
                Object::Integer { value: n, .. } => Evaluation::from(Object::from(n * -1)),
                _ => Evaluation::from(Object::unknown_prefix_operator(rhs.as_ref(), self.operator)),
            },
        }
    }
}

impl Evaluatable<'_> for InfixExpression {
    fn evaluate(&self, env: &mut Environment) -> Evaluation {
        let (lhs, rhs) = self.operands.as_ref();
        let (lhs, rhs) = (lhs.evaluate(env), rhs.evaluate(env));

        match (lhs.as_ref(), rhs.as_ref()) {
            (Object::Integer { value: x, .. }, Object::Integer { value: y, .. }) => {
                match self.operator {
                    InfixOperator::Plus => Evaluation::from(Object::from(x + y)),
                    InfixOperator::Minus => Evaluation::from(Object::from(x - y)),
                    InfixOperator::Mul => Evaluation::from(Object::from(x * y)),
                    InfixOperator::Divide => Evaluation::from(Object::from(x / y)),
                    InfixOperator::LT if x < y => Evaluation::from(&TRUE),
                    InfixOperator::LT => Evaluation::from(&FALSE),
                    InfixOperator::GT if x > y => Evaluation::from(&TRUE),
                    InfixOperator::GT => Evaluation::from(&FALSE),
                    InfixOperator::Eq if x == y => Evaluation::from(&TRUE),
                    InfixOperator::Eq => Evaluation::from(&FALSE),
                    InfixOperator::Neq if x != y => Evaluation::from(&TRUE),
                    InfixOperator::Neq => Evaluation::from(&FALSE),
                    _ => Evaluation::from(Object::unknown_infix_operator(
                        lhs.as_ref(),
                        rhs.as_ref(),
                        self.operator,
                    )),
                }
            }
            (Object::Boolean { value: a, .. }, Object::Boolean { value: b, .. }) => {
                match self.operator {
                    InfixOperator::Eq if a == b => Evaluation::from(&TRUE),
                    InfixOperator::Eq => Evaluation::from(&FALSE),
                    InfixOperator::Neq if a != b => Evaluation::from(&TRUE),
                    InfixOperator::Neq => Evaluation::from(&FALSE),
                    _ => Evaluation::from(Object::unknown_infix_operator(
                        lhs.as_ref(),
                        rhs.as_ref(),
                        self.operator,
                    )),
                }
            }
            _ => Evaluation::from(Object::type_mismatch_error(
                lhs.as_ref(),
                rhs.as_ref(),
                self.operator,
            )),
        }
    }
}

impl Evaluatable<'_> for IfExpression {
    fn evaluate(&self, env: &mut Environment) -> Evaluation {
        match self.condition.evaluate(env).as_ref() {
            Object::Null { .. } | Object::Boolean { value: false, .. }
                if self.alternative.is_some() =>
            {
                self.alternative.as_ref().unwrap().evaluate(env)
            }
            Object::Null { .. } | Object::Boolean { value: false, .. } => Evaluation::from(&NULL),
            _ => self.consequence.evaluate(env),
        }
    }
}

impl Evaluatable<'_> for Statement {
    fn evaluate(&self, env: &mut Environment) -> Evaluation {
        match self {
            Statement::Expression(e) => e.evaluate(env),
            Statement::Return(e) => Cow::Owned(e.evaluate(env).return_from()),
            Statement::Block(block) => block.evaluate(env),
            Statement::Let { ident, expression } => {
                let val = expression.evaluate(env);
                if val.is_error() {
                    return val;
                }
                env.set(ident, val.into_owned());
                Evaluation::from(&NULL)
            }
        }
    }
}

impl Evaluatable<'_> for Block {
    fn evaluate(&self, env: &mut Environment) -> Evaluation {
        let mut result = Evaluation::from(&NULL);
        for statement in self.statements.iter() {
            result = statement.evaluate(env);
            if result.is_return() || result.is_error() {
                return result;
            }
        }
        result
    }
}

impl Evaluatable<'_> for Program {
    fn evaluate(&self, env: &mut Environment) -> Evaluation {
        let mut result = Evaluation::from(&NULL);
        for statement in self.iter() {
            result = statement.evaluate(env);
            if result.is_error() {
                return result;
            }
            if result.is_return() {
                return Cow::Owned(result.eval_from());
            }
        }
        result
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn integer_expression() {
        assert_eq!(
            *Expression::from(5).evaluate(&mut &mut Environment::new()),
            Object::from(5)
        );
        assert_eq!(
            *Expression::new_prefix(PrefixOperator::Minus, Expression::from(5))
                .evaluate(&mut &mut Environment::new()),
            Object::from(-5)
        );
        assert_eq!(
            *Expression::from(10).evaluate(&mut &mut Environment::new()),
            Object::from(10)
        );
        assert_eq!(
            *Expression::new_prefix(PrefixOperator::Minus, 10).evaluate(&mut Environment::new()),
            Object::from(-10)
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::Plus, 5, 5).evaluate(&mut Environment::new()),
            Object::from(10)
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::Minus, 5, 5).evaluate(&mut Environment::new()),
            Object::from(0)
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::Mul, 5, 5).evaluate(&mut Environment::new()),
            Object::from(25)
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::Divide, 5, 5).evaluate(&mut Environment::new()),
            Object::from(1)
        );
    }

    #[test]
    fn boolean_expression() {
        assert_eq!(
            *Expression::from(true).evaluate(&mut Environment::new()),
            Object::from(true)
        );
        assert_eq!(
            *Expression::from(false).evaluate(&mut Environment::new()),
            Object::from(false)
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::LT, 1, 2).evaluate(&mut Environment::new()),
            TRUE
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::GT, 1, 2).evaluate(&mut Environment::new()),
            FALSE
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::LT, 1, 1).evaluate(&mut Environment::new()),
            FALSE
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::GT, 1, 1).evaluate(&mut Environment::new()),
            FALSE
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::Eq, 1, 1).evaluate(&mut Environment::new()),
            TRUE
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::Neq, 1, 1).evaluate(&mut Environment::new()),
            FALSE
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::Eq, 1, 2).evaluate(&mut Environment::new()),
            FALSE
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::Neq, 1, 2).evaluate(&mut Environment::new()),
            TRUE
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::Eq, true, true).evaluate(&mut Environment::new()),
            TRUE
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::Eq, false, false)
                .evaluate(&mut Environment::new()),
            TRUE
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::Eq, true, false)
                .evaluate(&mut Environment::new()),
            FALSE
        );
        assert_eq!(
            *Expression::new_infix(InfixOperator::Eq, false, true)
                .evaluate(&mut Environment::new()),
            FALSE
        );
        assert_eq!(
            *Expression::new_infix(
                InfixOperator::Eq,
                Expression::new_infix(InfixOperator::LT, 1, 2),
                true
            )
            .evaluate(&mut Environment::new()),
            TRUE
        );
        assert_eq!(
            *Expression::new_infix(
                InfixOperator::Eq,
                Expression::new_infix(InfixOperator::LT, 1, 2),
                Expression::from(false)
            )
            .evaluate(&mut Environment::new()),
            FALSE
        );
        assert_eq!(
            *Expression::new_infix(
                InfixOperator::Eq,
                Expression::new_infix(InfixOperator::GT, 1, 2),
                Expression::from(true)
            )
            .evaluate(&mut Environment::new()),
            FALSE
        );
        assert_eq!(
            *Expression::new_infix(
                InfixOperator::Eq,
                Expression::new_infix(InfixOperator::GT, 1, 2),
                Expression::from(false)
            )
            .evaluate(&mut Environment::new()),
            TRUE
        );
    }

    #[test]
    fn not_operator() {
        assert_eq!(
            *Expression::new_prefix(PrefixOperator::Not, true).evaluate(&mut Environment::new()),
            FALSE
        );
        assert_eq!(
            *Expression::new_prefix(PrefixOperator::Not, false).evaluate(&mut Environment::new()),
            TRUE
        );
        assert_eq!(
            *Expression::new_prefix(PrefixOperator::Not, 5).evaluate(&mut Environment::new()),
            FALSE
        );
        assert_eq!(
            *Expression::new_prefix(
                PrefixOperator::Not,
                Expression::new_prefix(PrefixOperator::Not, true)
            )
            .evaluate(&mut Environment::new()),
            TRUE
        );
        assert_eq!(
            *Expression::new_prefix(
                PrefixOperator::Not,
                Expression::new_prefix(PrefixOperator::Not, false)
            )
            .evaluate(&mut Environment::new()),
            FALSE
        );
        assert_eq!(
            *Expression::new_prefix(
                PrefixOperator::Not,
                Expression::new_prefix(PrefixOperator::Not, 5)
            )
            .evaluate(&mut Environment::new()),
            TRUE
        );
    }

    #[test]
    fn if_else_expressions() {
        assert_eq!(
            *Expression::new_if(
                Expression::from(true),
                Block::new([Statement::Expression(Expression::from(10))]),
            )
            .evaluate(&mut Environment::new()),
            Object::from(10)
        );
        assert_eq!(
            *Expression::new_if(
                Expression::from(false),
                Block::new([Statement::Expression(Expression::from(10))]),
            )
            .evaluate(&mut Environment::new()),
            NULL
        );
        assert_eq!(
            *Expression::new_if(
                Expression::from(1),
                Block::new([Statement::Expression(Expression::from(10))]),
            )
            .evaluate(&mut Environment::new()),
            Object::from(10)
        );
        assert_eq!(
            *Expression::new_if(
                Expression::new_infix(InfixOperator::LT, 1, 2),
                Block::new([Statement::Expression(Expression::from(10))]),
            )
            .evaluate(&mut Environment::new()),
            Object::from(10)
        );
        assert_eq!(
            *Expression::new_if(
                Expression::new_infix(InfixOperator::GT, 1, 2),
                Block::new([Statement::Expression(Expression::from(10))]),
            )
            .evaluate(&mut Environment::new()),
            NULL
        );
        assert_eq!(
            *Expression::new_if_else(
                Expression::new_infix(InfixOperator::GT, 1, 2),
                Block::new([Statement::Expression(Expression::from(10))]),
                Block::new([Statement::Expression(Expression::from(20))]),
            )
            .evaluate(&mut Environment::new()),
            Object::from(20)
        );
        assert_eq!(
            *Expression::new_if_else(
                Expression::new_infix(InfixOperator::LT, 1, 2),
                Block::new([Statement::Expression(Expression::from(10))]),
                Block::new([Statement::Expression(Expression::from(20))]),
            )
            .evaluate(&mut Environment::new()),
            Object::from(10)
        );
    }

    #[test]
    fn return_statements() {
        use crate::object::object::ObjectContext;

        assert_eq!(
            *Statement::Return(Expression::from(10)).evaluate(&mut Environment::new()),
            Object::Integer {
                value: 10,
                context: ObjectContext::Return
            }
        );
        assert_eq!(
            *vec![Statement::Return(Expression::from(10))].evaluate(&mut Environment::new()),
            Object::from(10)
        );
        assert_eq!(
            *vec![
                Statement::Return(Expression::from(10)),
                Statement::Expression(Expression::from(9))
            ]
            .evaluate(&mut Environment::new()),
            Object::from(10)
        );
        assert_eq!(
            *vec![
                Statement::Return(Expression::new_infix(InfixOperator::Mul, 2, 5)),
                Statement::Expression(Expression::from(9))
            ]
            .evaluate(&mut Environment::new()),
            Object::from(10)
        );
        assert_eq!(
            *vec![
                Statement::Expression(Expression::from(9)),
                Statement::Return(Expression::new_infix(InfixOperator::Mul, 2, 5)),
                Statement::Expression(Expression::from(9))
            ]
            .evaluate(&mut Environment::new()),
            Object::from(10)
        );
        assert_eq!(
            *vec![Statement::Expression(Expression::new_if(
                Expression::new_infix(InfixOperator::GT, 10, 1),
                Block::new([
                    Statement::Expression(Expression::new_if(
                        Expression::new_infix(InfixOperator::GT, 10, 1),
                        Block::new([Statement::Return(Expression::from(10))])
                    )),
                    Statement::Return(Expression::from(1))
                ])
            )),]
            .evaluate(&mut Environment::new()),
            Object::from(10)
        );
    }

    #[test]
    fn error_handling() {
        assert_eq!(
            *Statement::Expression(Expression::new_infix(
                InfixOperator::Plus,
                Expression::from(5),
                Expression::from(true)
            ))
            .evaluate(&mut Environment::new()),
            Object::error_from("type mismatch: INTEGER + BOOLEAN")
        );

        assert_eq!(
            *vec![
                Statement::Expression(Expression::new_infix(
                    InfixOperator::Plus,
                    Expression::from(5),
                    Expression::from(true)
                )),
                Statement::Expression(Expression::from(5))
            ]
            .evaluate(&mut Environment::new()),
            Object::error_from("type mismatch: INTEGER + BOOLEAN")
        );

        assert_eq!(
            *Statement::Expression(Expression::new_prefix(
                PrefixOperator::Minus,
                Expression::from(true)
            ))
            .evaluate(&mut Environment::new()),
            Object::error_from("unknown operator: -BOOLEAN")
        );

        assert_eq!(
            *Statement::Expression(Expression::new_infix(
                InfixOperator::Plus,
                Expression::from(true),
                Expression::from(false),
            ))
            .evaluate(&mut Environment::new()),
            Object::error_from("unknown operator: BOOLEAN + BOOLEAN")
        );

        assert_eq!(
            *vec![
                Statement::Expression(Expression::from(5)),
                Statement::Expression(Expression::new_infix(
                    InfixOperator::Plus,
                    Expression::from(true),
                    Expression::from(false),
                )),
                Statement::Expression(Expression::from(5))
            ]
            .evaluate(&mut Environment::new()),
            Object::error_from("unknown operator: BOOLEAN + BOOLEAN")
        );

        assert_eq!(
            *Expression::new_if(
                Expression::new_infix(InfixOperator::GT, Expression::from(10), Expression::from(1)),
                Block::new([Statement::Expression(Expression::new_infix(
                    InfixOperator::Plus,
                    Expression::from(true),
                    Expression::from(false)
                ))])
            )
            .evaluate(&mut Environment::new()),
            Object::error_from("unknown operator: BOOLEAN + BOOLEAN")
        );

        assert_eq!(
            *Expression::new_if(
                Expression::new_infix(InfixOperator::GT, Expression::from(10), Expression::from(1)),
                Block::new([Statement::Expression(Expression::new_if(
                    Expression::new_infix(
                        InfixOperator::GT,
                        Expression::from(10),
                        Expression::from(1)
                    ),
                    Block::new([Statement::Return(Expression::new_infix(
                        InfixOperator::Plus,
                        Expression::from(true),
                        Expression::from(false)
                    ))]),
                ))]),
            )
            .evaluate(&mut Environment::new()),
            Object::error_from("unknown operator: BOOLEAN + BOOLEAN")
        );

        assert_eq!(
            *Expression::from("foobar").evaluate(&mut Environment::new()),
            Object::error_from("identifier not found: foobar")
        );
    }

    #[test]
    fn let_statements() {
        use std::rc::Rc;

        assert_eq!(
            *vec![
                Statement::Let {
                    ident: Rc::new(String::from("a")),
                    expression: Expression::from(5)
                },
                Statement::Expression(Expression::from("a"))
            ]
            .evaluate(&mut Environment::new()),
            Object::from(5)
        );
        assert_eq!(
            *vec![
                Statement::Let {
                    ident: Rc::new(String::from("a")),
                    expression: Expression::new_infix(
                        InfixOperator::Mul,
                        Expression::from(5),
                        Expression::from(5)
                    )
                },
                Statement::Expression(Expression::from("a"))
            ]
            .evaluate(&mut Environment::new()),
            Object::from(25)
        );
        assert_eq!(
            *vec![
                Statement::Let {
                    ident: Rc::new(String::from("a")),
                    expression: Expression::from(5),
                },
                Statement::Let {
                    ident: Rc::new(String::from("b")),
                    expression: Expression::from("a")
                },
                Statement::Expression(Expression::from("b"))
            ]
            .evaluate(&mut Environment::new()),
            Object::from(5)
        );
        assert_eq!(
            *vec![
                Statement::Let {
                    ident: Rc::new(String::from("a")),
                    expression: Expression::from(5),
                },
                Statement::Let {
                    ident: Rc::new(String::from("b")),
                    expression: Expression::from("a")
                },
                Statement::Let {
                    ident: Rc::new(String::from("c")),
                    expression: Expression::new_infix(
                        InfixOperator::Plus,
                        Expression::new_infix(
                            InfixOperator::Plus,
                            Expression::from("a"),
                            Expression::from("b"),
                        ),
                        Expression::from(5)
                    )
                },
                Statement::Expression(Expression::from("c"))
            ]
            .evaluate(&mut Environment::new()),
            Object::from(15)
        );
    }
}
