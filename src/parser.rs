use crate::ast::{
    Block, Expression, IfExpression, InfixOperator, Precendence, PrefixOperator, Statement,
    LOWEST_PRECEDENCE,
};
use crate::lexer::Lexer;
use crate::token::Token;
use std::rc::Rc;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    next_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(&input);
        let current_token = lexer.next();
        let next_token = lexer.next();
        Self {
            lexer,
            current_token,
            next_token,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.next_token.take();
        self.next_token = self.lexer.next();
    }

    pub fn parse_program(input: &'a str) -> Vec<Result<Statement, String>> {
        // TODO: format output Result<Vec<Statement>, String>
        Self::new(input).collect()
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        self.expect_peek(Token::new_ident(""))?;
        let ident = self.parse_identifier()?;
        self.expect_peek(Token::Assign)?;
        self.next_token();
        let expression = self.parse_expression(LOWEST_PRECEDENCE)?;
        if let Some(next_token) = self.next_token.as_ref() {
            if next_token.is_same_variant(Token::Semicolon) {
                self.next_token();
            }
        }
        Ok(Statement::Let { ident, expression })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
        let expression = self.parse_expression(LOWEST_PRECEDENCE)?;
        if let Some(next_token) = self.next_token.as_ref() {
            if next_token.is_same_variant(Token::Semicolon) {
                self.next_token();
            }
        }
        Ok(Statement::Return(expression))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expression = self.parse_expression(LOWEST_PRECEDENCE)?;
        if let Some(next_token) = self.next_token.as_ref() {
            if next_token.is_same_variant(Token::Semicolon) {
                self.next_token();
            }
        }
        Ok(Statement::Expression(expression))
    }

    fn parse_expression(&mut self, precedence: u8) -> Result<Expression, String> {
        let mut lhs = self.parse_prefix_expression()?;

        while self.next_token.as_ref().map_or(false, |next_token| {
            !next_token.is_same_variant(Token::Semicolon) && precedence < self.peek_precedence()
        }) {
            let Some(next_token) = self.next_token.as_ref() else {
                return Ok(lhs);
            };
            match next_token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Eq
                | Token::Neq
                | Token::LT
                | Token::Lparen
                | Token::GT => {
                    self.next_token();
                    lhs = self.parse_infix_expression(lhs)?;
                }
                _ => return Ok(lhs),
            };
        }
        Ok(lhs)
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, String> {
        match &self.current_token {
            Some(Token::Int(_)) => Ok(Expression::from(self.parse_integer_literal()?)),
            Some(Token::Ident(_)) => Ok(Expression::Ident(self.parse_identifier()?)),
            Some(Token::Exclam) | Some(Token::Minus) => {
                let operator: PrefixOperator =
                    PrefixOperator::try_from(self.current_token.as_ref().unwrap()).unwrap();
                self.next_token();
                Ok(Expression::new_prefix(
                    operator,
                    self.parse_expression(operator.precedence())?,
                ))
            }
            Some(Token::True) => Ok(Expression::Boolean(true)),
            Some(Token::False) => Ok(Expression::Boolean(false)),
            Some(Token::Lparen) => self.parse_grouped_expression(),
            Some(Token::If) => self.parse_if_expression(),
            Some(Token::Function) => self.parse_function_literal(),
            Some(ref token) => Err(format!(
                "Unable to parse prefix expression starting with {token}"
            )),
            None => Err("Unable to parse prefix expression expected token but got none".into()),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, String> {
        self.next_token();
        let expression = self.parse_expression(LOWEST_PRECEDENCE)?;
        self.expect_peek(Token::Rparen)?;
        Ok(expression)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        self.expect_peek(Token::Lparen)?;
        self.next_token();
        let condition = self.parse_expression(LOWEST_PRECEDENCE)?;

        self.expect_peek(Token::Rparen)?;
        self.expect_peek(Token::Lbrace)?;
        let consequence = self.parse_block_statement()?;
        let alternative = if let Some(ref next_token) = self.next_token {
            if next_token.is_same_variant(Token::Else) {
                self.next_token();
                self.expect_peek(Token::Lbrace)?;
                Some(self.parse_block_statement()?)
            } else {
                None
            }
        } else {
            None
        };
        Ok(Expression::If(Box::new(IfExpression {
            condition,
            consequence,
            alternative,
        })))
    }

    fn parse_block_statement(&mut self) -> Result<Block, String> {
        let mut statements: Vec<Statement> = Vec::new();
        self.next_token();
        while self.current_token.as_ref().map_or(false, |current_token| {
            !(current_token.is_same_variant(Token::Rbrace)
                || current_token.is_same_variant(Token::EOF))
        }) {
            // TODO: remove duplication with parse_statement
            let statement = match self.current_token.as_ref().unwrap() {
                Token::Let => self.parse_let_statement(),
                Token::Return => self.parse_return_statement(),
                _ => self.parse_expression_statement(),
            };
            if let Ok(statement) = statement {
                statements.push(statement);
            }
            self.next_token();
        }
        Ok(Block::new(statements))
    }

    fn parse_infix_expression(&mut self, lhs: Expression) -> Result<Expression, String> {
        let Some(current_token) = self.current_token.as_ref() else {
            return Err("Unable to parse prefix expression expected token but got none".into());
        };
        let operator = match current_token {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::Eq
            | Token::Neq
            | Token::LT
            | Token::Lparen
            | Token::GT => InfixOperator::try_from(current_token),
            _ => {
                return Err(format!(
                    "Unable to parse infix expression starting with {current_token}"
                ));
            }
        };
        let Ok(operator) = operator else {
            return Err(format!("Expected infix operator found {current_token}"));
        };
        let precendence = self.current_precedence();
        self.next_token();
        if operator == InfixOperator::Call {
            let arguments = self.parse_call_arguments()?;
            return Ok(Expression::Call(Box::new(lhs), arguments));
        }
        let rhs = self.parse_expression(precendence)?;
        Ok(Expression::new_infix(operator, lhs, rhs))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, String> {
        let mut args = vec![];
        if self
            .current_token
            .as_ref()
            .map_or(false, |token| token.is_same_variant(Token::Rparen))
        {
            self.next_token();
            return Ok(args);
        }
        args.push(self.parse_expression(LOWEST_PRECEDENCE)?);

        while self
            .next_token
            .as_ref()
            .map_or(false, |token| token.is_same_variant(Token::Comma))
        {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(LOWEST_PRECEDENCE)?);
        }
        self.expect_peek(Token::Rparen)?;
        Ok(args)
    }

    fn parse_identifier(&self) -> Result<Rc<String>, String> {
        if let Some(Token::Ident(ident)) = self.current_token.as_ref() {
            Ok(ident.clone())
        } else {
            Err("Failed to parse identifier".into())
        }
    }

    fn parse_integer_literal(&self) -> Result<u64, String> {
        if let Some(Token::Int(n)) = self.current_token.as_ref() {
            Ok(*n)
        } else {
            Err("Failed to parse integer literal".into())
        }
    }

    fn parse_function_literal(&mut self) -> Result<Expression, String> {
        self.expect_peek(Token::Lparen)?;
        let parameters = self.parse_function_parameters()?;

        self.expect_peek(Token::Lbrace)?;
        // todo: fix this
        let block = self.parse_block_statement()?;
        Ok(Expression::FunctionLiteral(parameters, block))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Rc<String>>, String> {
        let mut identifiers = vec![];
        if self
            .next_token
            .as_ref()
            .map_or(true, |token| token.is_same_variant(Token::Rparen))
        {
            self.next_token();
            return Ok(identifiers);
        }
        self.next_token();
        let Some(Token::Ident(ident)) = self.current_token.as_ref() else {
            return Err("Expected identity in function params".into());
        };
        identifiers.push(ident.clone());

        while self
            .next_token
            .as_ref()
            .map_or(false, |token| token.is_same_variant(Token::Comma))
        {
            self.next_token();
            self.next_token();
            let Some(Token::Ident(ident)) = self.current_token.as_ref() else {
                return Err("Expected identity in function params".into());
            };
            identifiers.push(ident.clone());
        }

        self.expect_peek(Token::Rparen)?;

        Ok(identifiers)
    }

    fn expect_peek(&mut self, token: Token) -> Result<(), String> {
        match self.next_token.as_ref() {
            Some(next_token) if next_token.is_same_variant(&token) => {
                self.next_token();
                Ok(())
            }
            Some(next_token) => Err(format!("Expected {token} got {next_token}",)),
            None => Err(format!("Expected {token} got none")),
        }
    }

    fn peek_precedence(&self) -> u8 {
        if let Some(ref next_token) = self.next_token {
            if let Ok(operator) = InfixOperator::try_from(next_token) {
                return operator.precedence();
            }
            if let Ok(operator) = PrefixOperator::try_from(next_token) {
                return operator.precedence();
            }
        }
        LOWEST_PRECEDENCE
    }

    fn current_precedence(&self) -> u8 {
        if let Some(ref current_token) = self.current_token {
            if let Ok(operator) = InfixOperator::try_from(current_token) {
                return operator.precedence();
            }
            if let Ok(operator) = PrefixOperator::try_from(current_token) {
                return operator.precedence();
            }
        }
        LOWEST_PRECEDENCE
    }

    fn parse_statement(&mut self) -> Option<Result<Statement, String>> {
        let statement = match self.current_token.as_ref()? {
            Token::Let => Some(self.parse_let_statement()),
            Token::Return => Some(self.parse_return_statement()),
            Token::EOF => None,
            _ => Some(self.parse_expression_statement()),
        };
        self.next_token();
        statement
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Statement, String>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_statement()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn let_statements() {
        assert_eq!(
            Parser::new(
                "
                let x = 5;
                let y = 10;
                let foobar = 838383;
                ",
            )
            .filter_map(|r| match r {
                Ok(s) => Some(s.to_string()),
                Err(e) => panic!("Parsing error: {e}"),
            })
            .collect::<Vec<_>>(),
            vec!["let x = 5;", "let y = 10;", "let foobar = 838383;",]
        )
    }

    #[test]
    fn return_statements() {
        assert_eq!(
            Parser::new(
                "
                return 5;
                return 10;
                return 939393;
                "
            )
            .filter_map(|r| match r {
                Ok(s) => Some(s.to_string()),
                Err(e) => panic!("Parsing error: {e}"),
            })
            .collect::<Vec<_>>(),
            vec!["return 5;", "return 10;", "return 939393;",]
        )
    }

    #[test]
    fn prefix_expressions() {
        assert_eq!(
            Parser::new(
                "
                -5;
                !foobar;
                !true;
                !false;
                "
            )
            .filter_map(|r| match r {
                Ok(s) => Some(s.to_string()),
                Err(e) => panic!("Parsing error: {e}"),
            })
            .collect::<Vec<_>>(),
            vec!["(-5)", "(!foobar)", "(!true)", "(!false)"]
        )
    }

    #[test]
    fn infix_expressions() {
        assert_eq!(
            Parser::new(
                "
                    5 + 5;
                    5 - 5;
                    5 * 5;
                    5 / 5;
                    5 > 5;
                    5 < 5;
                    5 == 5;
                    5 != 5;
                    true == true;
                    true != false;
                    false == false;
                    "
            )
            .filter_map(|r| match r {
                Ok(s) => Some(s.to_string()),
                Err(e) => panic!("Parsing error: {e}"),
            })
            .collect::<Vec<_>>(),
            vec![
                "(5 + 5)",
                "(5 - 5)",
                "(5 * 5)",
                "(5 / 5)",
                "(5 > 5)",
                "(5 < 5)",
                "(5 == 5)",
                "(5 != 5)",
                "(true == true)",
                "(true != false)",
                "(false == false)",
            ]
        )
    }

    #[test]
    fn precedence_parsing() {
        assert_eq!(
            Parser::new(
                "
                -a * b;
                !-a;
                a + b + c;
                a + b - c;
                a * b * c;
                a * b / c;
                a + b / c;
                a + b * c + d / e - f;
                5 > 4 == 3 < 4;
                5 < 4 != 3 > 4;
                3 + 4 * 5 == 3 * 1 + 4 * 5;
                true;
                false;
                3 > 5 == false;
                3 < 5 == true;
                1 + (2 + 3) + 4;
                (5 + 5) * 2;
                2 / (5 + 5);
                -(5 + 5);
                !(true == true)
                a + add(b*c) + d
                add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))
                add(a + b + c * d / f + g)
                "
            )
            .filter_map(|r| match r {
                Ok(s) => Some(s.to_string()),
                Err(e) => panic!("Parsing error: {e}"),
            })
            .collect::<Vec<_>>(),
            vec![
                "((-a) * b)",
                "(!(-a))",
                "((a + b) + c)",
                "((a + b) - c)",
                "((a * b) * c)",
                "((a * b) / c)",
                "(a + (b / c))",
                "(((a + (b * c)) + (d / e)) - f)",
                "((5 > 4) == (3 < 4))",
                "((5 < 4) != (3 > 4))",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
                "true",
                "false",
                "((3 > 5) == false)",
                "((3 < 5) == true)",
                "((1 + (2 + 3)) + 4)",
                "((5 + 5) * 2)",
                "(2 / (5 + 5))",
                "(-(5 + 5))",
                "(!(true == true))",
                "((a + add((b * c))) + d)",
                "add(a,b,1,(2 * 3),(4 + 5),add(6,(7 * 8)))",
                "add((((a + b) + ((c * d) / f)) + g))",
            ]
        )
    }

    #[test]
    fn if_expression() {
        assert_eq!(
            Parser::new(
                "
                if (x < y) { x } else { y }
                if (x + 2 == 3) {
                    let y = 3 + x;
                    return y;
                }
                "
            )
            .filter_map(|r| match r {
                Ok(s) => Some(s.to_string()),
                Err(e) => panic!("Parsing error: {e}"),
            })
            .collect::<Vec<_>>(),
            vec![
                "if ((x < y)) {\nx\n} else {\ny\n}",
                "if (((x + 2) == 3)) {\nlet y = (3 + x);\nreturn y;\n}",
            ]
        );
    }

    #[test]
    fn function_parameters() {
        assert_eq!(
            Parser::new(
                "
                fn() {};
                fn(x) {};
                fn(x,y,z) {};
                "
            )
            .filter_map(|r| match r {
                Ok(s) => Some(s.to_string()),
                Err(e) => panic!("Parsing error: {e}"),
            })
            .collect::<Vec<_>>(),
            vec!["fn(){\n}", "fn(x){\n}", "fn(x,y,z){\n}",]
        );
    }

    #[test]
    fn call_expression() {
        assert_eq!(
            Parser::new(
                "
                foo();
                add(2,3);
                "
            )
            .filter_map(|r| match r {
                Ok(s) => Some(s.to_string()),
                Err(e) => panic!("Parsing error: {e}"),
            })
            .collect::<Vec<_>>(),
            vec!["foo()", "add(2,3)"]
        );
    }
}
