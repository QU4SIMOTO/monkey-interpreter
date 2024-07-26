use crate::ast::{Block, InfixOperator, PrefixOperator};
use crate::object::environment::Environment;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum ObjectContext {
    Return,
    Eval,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer {
        value: i64,
        context: ObjectContext,
    },
    Boolean {
        value: bool,
        context: ObjectContext,
    },
    String {
        value: Rc<String>,
        context: ObjectContext,
    },
    Null {
        context: ObjectContext,
    },
    Function {
        parameters: Vec<Rc<String>>,
        body: Block,
        env: Environment,
        context: ObjectContext,
    },
    Error(String),
}

impl Object {
    pub fn is_return(&self) -> bool {
        match self {
            Self::Integer { context, .. }
            | Self::Boolean { context, .. }
            | Self::Null { context }
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
            Self::Boolean { value, .. } => Self::Boolean { value, context },
            Self::Integer { value, .. } => Self::Integer { value, context },
            Self::String { ref value, .. } => Self::String {
                value: value.clone(),
                context,
            },
            Self::Null { .. } => Self::Null { context },
            Self::Error { .. } => self.clone(),
            Self::Function {
                ref parameters,
                ref body,
                ref env,
                ..
            } => Self::Function {
                parameters: parameters.clone(),
                body: body.clone(),
                env: env.clone(),
                context,
            },
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
            Self::Integer { .. } => "INTEGER",
            Self::Boolean { .. } => "BOOLEAN",
            Self::Null { .. } => "NULL",
            Self::Error { .. } => "ERROR",
            Self::Function { .. } => "FUNCTION",
            Self::String { .. } => "STRING",
        }
    }

    pub fn type_mismatch_error(a: &Object, b: &Object, operator: InfixOperator) -> Self {
        Self::Error(format!(
            "type mismatch: {} {} {}",
            a.kind(),
            operator.to_string(),
            b.kind()
        ))
    }

    pub fn unknown_prefix_operator(a: &Object, operator: PrefixOperator) -> Self {
        Self::Error(format!("unknown operator: {}{}", operator, a.kind(),))
    }

    pub fn unknown_infix_operator(a: &Object, b: &Object, operator: InfixOperator) -> Self {
        Self::Error(format!(
            "unknown operator: {} {} {}",
            a.kind(),
            operator,
            b.kind(),
        ))
    }

    pub fn unknown_ident(ident: &str) -> Self {
        Self::Error(format!("identifier not found: {ident}"))
    }

    pub fn error_from(message: impl Into<String>) -> Self {
        Self::Error(message.into())
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer { value, .. } => write!(f, "{value}"),
            Self::Boolean { value, .. } => write!(f, "{value}"),
            Self::String { value, .. } => write!(f, "{value}"),
            Self::Null { .. } => write!(f, "null"),
            Self::Error(value) => write!(f, "ERROR: {value}"),
            Self::Function {
                parameters, body, ..
            } => {
                write!(f, "fn(")?;
                write!(
                    f,
                    "{}",
                    parameters
                        .iter()
                        .map(|p| p.as_str())
                        .collect::<Vec<_>>()
                        .join(",")
                )?;
                write!(f, ")")?;
                write!(f, "{body}")?;
                Ok(())
            }
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
