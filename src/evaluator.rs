use crate::ast::{
    Block, Expression, IfExpression, InfixExpression, InfixOperator, PrefixExpression,
    PrefixOperator, Program, Statement,
};
use crate::object::{
    environment::Environment,
    object::{Object, ObjectContext, FALSE, NULL, TRUE},
};
use crate::parser::Parser;
use std::rc::Rc;

pub type Evaluation = Rc<Object>;

pub trait Evaluatable<'a> {
    fn evaluate(&self, env: &'a mut Environment) -> Evaluation;
}

impl<'a> Evaluatable<'a> for &str {
    fn evaluate(&self, env: &'a mut Environment) -> Evaluation {
        let p = Parser::new(self)
            .filter_map(|s| match s {
                Ok(s) => Some(s),
                _ => None,
            })
            .collect::<Vec<_>>();
        p.evaluate(env)
    }
}

impl Evaluatable<'_> for Expression {
    fn evaluate(&self, env: &mut Environment) -> Evaluation {
        match self {
            Expression::IntegerLiteral(n) => Evaluation::from(Object::from(*n)),
            Expression::Boolean(true) => TRUE.into(),
            Expression::Boolean(false) => FALSE.into(),
            Expression::Prefix(e) => e.evaluate(env),
            Expression::Infix(e) => e.evaluate(env),
            Expression::If(e) => e.evaluate(env),
            Expression::Ident(i) => {
                if let Some(v) = env.get(i.clone()) {
                    v.clone()
                } else {
                    Evaluation::from(Object::unknown_ident(i))
                }
            }
            Expression::FunctionLiteral(parameters, block) => Object::Function {
                // TODO: maybe consume self to avoid clone
                parameters: parameters.clone(),
                body: block.clone(),
                env: env.clone(),
                context: ObjectContext::Eval,
            }
            .into(),
            Expression::Call(f, args) => {
                let f = f.as_ref().evaluate(env);
                let Object::Function {
                    parameters,
                    body,
                    env: f_env,
                    context: _,
                } = f.as_ref()
                else {
                    return Object::error_from("expected function").into();
                };
                let args = args.iter().map(|arg| arg.evaluate(env));
                // todo: handle errors when evaluating args
                let mut env = Environment::new_enclosed(f_env.clone());
                for (i, arg) in args.enumerate() {
                    env.set(parameters.get(i).unwrap(), arg);
                }
                Evaluation::from(body.evaluate(&mut env))
            }
        }
    }
}

impl Evaluatable<'_> for PrefixExpression {
    fn evaluate(&self, env: &mut Environment) -> Evaluation {
        let rhs = self.operand.evaluate(env);
        match self.operator {
            PrefixOperator::Not => match *rhs {
                Object::Boolean { value: false, .. } | Object::Null { .. } => TRUE.into(),
                _ => FALSE.into(),
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
                    InfixOperator::Divide if *y == 0 => {
                        Evaluation::from(Object::error_from("Division by zero"))
                    }
                    InfixOperator::Divide => Evaluation::from(Object::from(x / y)),
                    InfixOperator::LT if x < y => TRUE.into(),
                    InfixOperator::LT => FALSE.into(),
                    InfixOperator::GT if x > y => TRUE.into(),
                    InfixOperator::GT => FALSE.into(),
                    InfixOperator::Eq if x == y => TRUE.into(),
                    InfixOperator::Eq => FALSE.into(),
                    InfixOperator::Neq if x != y => TRUE.into(),
                    InfixOperator::Neq => FALSE.into(),
                    _ => Evaluation::from(Object::unknown_infix_operator(
                        lhs.as_ref(),
                        rhs.as_ref(),
                        self.operator,
                    )),
                }
            }
            (Object::Boolean { value: a, .. }, Object::Boolean { value: b, .. }) => {
                match self.operator {
                    InfixOperator::Eq if a == b => TRUE.into(),
                    InfixOperator::Eq => FALSE.into(),
                    InfixOperator::Neq if a != b => TRUE.into(),
                    InfixOperator::Neq => FALSE.into(),
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
            Object::Null { .. } | Object::Boolean { value: false, .. } => NULL.into(),
            _ => self.consequence.evaluate(env),
        }
    }
}

impl Evaluatable<'_> for Statement {
    fn evaluate(&self, env: &mut Environment) -> Evaluation {
        match self {
            Statement::Expression(e) => e.evaluate(env),
            Statement::Return(e) => Rc::new(e.evaluate(env).return_from()),
            Statement::Block(block) => block.evaluate(env),
            Statement::Let { ident, expression } => {
                let val = expression.evaluate(env);
                if val.is_error() {
                    return val;
                }
                env.set(ident, val);
                NULL.into()
            }
        }
    }
}

impl Evaluatable<'_> for Block {
    fn evaluate(&self, env: &mut Environment) -> Evaluation {
        let mut result = NULL.into();
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
        let mut result = NULL.into();
        for statement in self.iter() {
            result = statement.evaluate(env);
            if result.is_error() {
                return result;
            }
            if result.is_return() {
                return Rc::new(result.eval_from());
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
            "5".evaluate(&mut Environment::new()),
            Object::from(5).into()
        );
        assert_eq!(
            "-5".evaluate(&mut Environment::new()),
            Object::from(-5).into()
        );
        assert_eq!(
            "10".evaluate(&mut Environment::new()),
            Object::from(10).into()
        );
        assert_eq!(
            "-10;".evaluate(&mut Environment::new()),
            Object::from(-10).into()
        );
        assert_eq!(
            "5 + 5".evaluate(&mut Environment::new()),
            Object::from(10).into()
        );
        assert_eq!(
            "5 - 5".evaluate(&mut Environment::new()),
            Object::from(0).into()
        );
        assert_eq!(
            "5 * 5;".evaluate(&mut Environment::new()),
            Object::from(25).into()
        );
        assert_eq!(
            "5 / 5".evaluate(&mut Environment::new()),
            Object::from(1).into()
        );
    }

    #[test]
    fn boolean_expression() {
        assert_eq!(
            "true".evaluate(&mut Environment::new()),
            Object::from(true).into()
        );
        assert_eq!(
            "false".evaluate(&mut Environment::new()),
            Object::from(false).into()
        );
        assert_eq!("1 < 2".evaluate(&mut Environment::new()), TRUE.into());
        assert_eq!("1 > 2".evaluate(&mut Environment::new()), FALSE.into());
        assert_eq!("1 < 1;".evaluate(&mut Environment::new()), FALSE.into());
        assert_eq!("1 > 1".evaluate(&mut Environment::new()), FALSE.into());
        assert_eq!("1 == 1".evaluate(&mut Environment::new()), TRUE.into());
        assert_eq!("1 != 1".evaluate(&mut Environment::new()), FALSE.into());
        assert_eq!("1 == 2".evaluate(&mut Environment::new()), FALSE.into());
        assert_eq!("1 != 2".evaluate(&mut Environment::new()), TRUE.into());
        assert_eq!(
            "true == true".evaluate(&mut Environment::new()),
            TRUE.into()
        );
        assert_eq!(
            "false == false;".evaluate(&mut Environment::new()),
            TRUE.into()
        );
        assert_eq!(
            "true == false".evaluate(&mut Environment::new()),
            FALSE.into()
        );
        assert_eq!(
            "false == true".evaluate(&mut Environment::new()),
            FALSE.into()
        );
        assert_eq!(
            "(1 < 2) == true".evaluate(&mut Environment::new()),
            TRUE.into()
        );
        assert_eq!(
            "(1 < 2) == false".evaluate(&mut Environment::new()),
            FALSE.into()
        );
        assert_eq!(
            "(1 > 2) == true".evaluate(&mut Environment::new()),
            FALSE.into()
        );
        assert_eq!(
            "(1 > 2) == false".evaluate(&mut Environment::new()),
            TRUE.into()
        );
    }

    #[test]
    fn not_operator() {
        assert_eq!("!true".evaluate(&mut Environment::new()), FALSE.into());
        assert_eq!(
            Expression::new_prefix(PrefixOperator::Not, false).evaluate(&mut Environment::new()),
            TRUE.into()
        );
        assert_eq!("!5".evaluate(&mut Environment::new()), FALSE.into());
        assert_eq!("!!true".evaluate(&mut Environment::new()), TRUE.into());
        assert_eq!("!!false".evaluate(&mut Environment::new()), FALSE.into());
        assert_eq!(
            Expression::new_prefix(
                PrefixOperator::Not,
                Expression::new_prefix(PrefixOperator::Not, 5)
            )
            .evaluate(&mut Environment::new()),
            TRUE.into()
        );
    }

    #[test]
    fn if_else_expressions() {
        assert_eq!(
            "if (true) { 10 }".evaluate(&mut Environment::new()),
            Object::from(10).into()
        );
        assert_eq!(
            "if(false) { 10; }".evaluate(&mut Environment::new()),
            NULL.into()
        );
        assert_eq!(
            "if(1) { 10 }".evaluate(&mut Environment::new()),
            Object::from(10).into()
        );
        assert_eq!(
            "if(1 < 2) { 10 }".evaluate(&mut Environment::new()),
            Object::from(10).into()
        );
        assert_eq!(
            "if (1 > 2) { 10 }".evaluate(&mut Environment::new()),
            NULL.into()
        );
        assert_eq!(
            "if (1 < 2) { 10; 20; }".evaluate(&mut Environment::new()),
            Object::from(20).into()
        );
        assert_eq!(
            "if (1 > 2) { 5 } else { 10; 20; }".evaluate(&mut Environment::new()),
            Object::from(20).into()
        );
    }

    #[test]
    fn return_statements() {
        assert_eq!(
            "return 10".evaluate(&mut Environment::new()),
            Object::from(10).into()
        );
        assert_eq!(
            "return 10;".evaluate(&mut Environment::new()),
            Object::from(10).into()
        );
        assert_eq!(
            "return 10; 9;".evaluate(&mut Environment::new()),
            Object::from(10).into()
        );
        assert_eq!(
            "return 2 * 5; 9;".evaluate(&mut Environment::new()),
            Object::from(10).into()
        );
        assert_eq!(
            "9; return 2 * 5; 9;".evaluate(&mut Environment::new()),
            Object::from(10).into()
        );
        assert_eq!(
            "if(10 > 1) { if(10 > 1) { return 10} return 1}".evaluate(&mut Environment::new()),
            Object::from(10).into()
        );
    }

    #[test]
    fn error_handling() {
        assert_eq!(
            "5 + true".evaluate(&mut Environment::new()),
            Object::error_from("type mismatch: INTEGER + BOOLEAN").into()
        );
        // todo: fix this, not propagating error correctly
        /*
        assert_eq!(
            "5 + (true + 5);".evaluate(&mut Environment::new()),
            Object::error_from("type mismatch: INTEGER + BOOLEAN").into()
        );
        */
        assert_eq!(
            "-true".evaluate(&mut Environment::new()),
            Object::error_from("unknown operator: -BOOLEAN").into()
        );
        assert_eq!(
            "true + false".evaluate(&mut Environment::new()),
            Object::error_from("unknown operator: BOOLEAN + BOOLEAN").into()
        );
        assert_eq!(
            "5; true + false; 5".evaluate(&mut Environment::new()),
            Object::error_from("unknown operator: BOOLEAN + BOOLEAN").into()
        );
        assert_eq!(
            "if(10 > 1){ true + false }".evaluate(&mut Environment::new()),
            Object::error_from("unknown operator: BOOLEAN + BOOLEAN").into()
        );
        assert_eq!(
            "if(10 > 1){ if(10 > 1){ return true + false}}".evaluate(&mut Environment::new()),
            Object::error_from("unknown operator: BOOLEAN + BOOLEAN").into()
        );
        assert_eq!(
            "foobar".evaluate(&mut Environment::new()),
            Object::error_from("identifier not found: foobar").into()
        );
    }

    #[test]
    fn let_statements() {
        assert_eq!(
            "let a = 5; a".evaluate(&mut Environment::new()),
            Object::from(5).into()
        );
        assert_eq!(
            "let a = 5 * 5; return a;".evaluate(&mut Environment::new()),
            Object::from(25).into()
        );
        assert_eq!(
            "let a = 5; let b = a; b".evaluate(&mut Environment::new()),
            Object::from(5).into()
        );
        assert_eq!(
            "let a = 5; let b = a; let c = a + b + 5; c".evaluate(&mut Environment::new()),
            Object::from(15).into()
        );
    }

    #[test]
    fn functions() {
        assert_eq!(
            "let identity = fn(x) { x; }; identity(5);".evaluate(&mut Environment::new()),
            Object::from(5).into()
        );
        assert_eq!(
            "let identity = fn(x) { return x; }; identity(5)".evaluate(&mut Environment::new()),
            Object::from(5).into()
        );
        assert_eq!(
            "let double = fn(x) { x * 2 }; double(5);".evaluate(&mut Environment::new()),
            Object::from(10).into()
        );
        assert_eq!(
            "let add = fn(x, y) { x + y }; add(5, 2)".evaluate(&mut Environment::new()),
            Object::from(7).into()
        );
        assert_eq!(
            "let add = fn(x, y) { x + y }; add(5 + 5, add(5,5))".evaluate(&mut Environment::new()),
            Object::from(20).into()
        );
    }

    #[test]
    fn closures() {
        assert_eq!(
            "let newAdder = fn(x) { fn(y) {x + y } }; let addTwo = newAdder(2); addTwo(2)"
                .evaluate(&mut Environment::new()),
            Object::from(4).into()
        );
    }
}
