use crate::ast::{InfixOperator, PrefixOperator};
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum ObjectContext {
    Return,
    Eval,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer { value: i64, context: ObjectContext },
    Boolean { value: bool, context: ObjectContext },
    Null { context: ObjectContext },
    Error(String),
}

impl Object {
    pub fn is_return(&self) -> bool {
        match self {
            Object::Integer { context, .. }
            | Object::Boolean { context, .. }
            | Object::Null { context }
                if *context == ObjectContext::Return =>
            {
                true
            }
            _ => false,
        }
    }

    pub fn is_error(&self) -> bool {
        match self {
            Object::Error(_) => true,
            _ => false,
        }
    }

    fn with_context(&self, context: ObjectContext) -> Self {
        match *self {
            Object::Boolean { value, .. } => Object::Boolean { value, context },
            Object::Integer { value, .. } => Object::Integer { value, context },
            Object::Null { .. } => Object::Null { context },
            Object::Error { .. } => self.clone(),
        }
    }

    pub fn eval_from(&self) -> Self {
        self.with_context(ObjectContext::Eval)
    }

    pub fn return_from(&self) -> Self {
        self.with_context(ObjectContext::Return)
    }

    pub fn kind(&self) -> &'static str {
        match self {
            Object::Integer { .. } => "INTEGER",
            Object::Boolean { .. } => "BOOLEAN",
            Object::Null { .. } => "NULL",
            Object::Error { .. } => "ERROR",
        }
    }

    pub fn type_mismatch_error(a: &Object, b: &Object, operator: InfixOperator) -> Self {
        Object::Error(format!(
            "type mismatch: {} {} {}",
            a.kind(),
            operator.to_string(),
            b.kind()
        ))
    }

    pub fn unknown_prefix_operator(a: &Object, operator: PrefixOperator) -> Self {
        Object::Error(format!("unknown operator: {}{}", operator, a.kind(),))
    }

    pub fn unknown_infix_operator(a: &Object, b: &Object, operator: InfixOperator) -> Self {
        Object::Error(format!(
            "unknown operator: {} {} {}",
            a.kind(),
            operator,
            b.kind(),
        ))
    }

    pub fn unknown_ident(ident: &str) -> Self {
        Object::Error(format!("identifier not found: {ident}"))
    }

    pub fn error_from(message: impl Into<String>) -> Self {
        Object::Error(message.into())
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer { value, .. } => write!(f, "{value}"),
            Object::Boolean { value, .. } => write!(f, "{value}"),
            Object::Null { .. } => write!(f, "null"),
            Object::Error(value) => write!(f, "ERROR: {value}"),
        }
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        Object::Boolean {
            value,
            context: ObjectContext::Eval,
        }
    }
}

impl From<i64> for Object {
    fn from(value: i64) -> Self {
        Object::Integer {
            value,
            context: ObjectContext::Eval,
        }
    }
}

pub const TRUE: Object = Object::Boolean {
    value: true,
    context: ObjectContext::Eval,
};
pub const FALSE: Object = Object::Boolean {
    value: false,
    context: ObjectContext::Eval,
};
pub const NULL: Object = Object::Null {
    context: ObjectContext::Eval,
};
