use crate::ast::{Block, InfixOperator, PrefixOperator};
use crate::object::environment::Environment;
use std::fmt;
use std::rc::Rc;

pub enum Builtins {
    Len,
    First,
    Last,
    Rest,
}

impl Builtins {
    pub fn get(name: &str) -> Option<Self> {
        match name {
            "len" => Some(Self::Len),
            "first" => Some(Self::First),
            "last" => Some(Self::Last),
            "rest" => Some(Self::Rest),
            _ => None,
        }
    }

    pub fn evaluate(&self, args: &[Rc<Object>]) -> Rc<Object> {
        match self {
            Self::Len if args.len() == 1 => match *args[0] {
                Object::String { ref value, .. } => Rc::new(Object::from(value.len() as i64)),
                Object::Array { ref elements, .. } => Rc::new(Object::from(elements.len() as i64)),
                ref o => Rc::new(Object::Error(format!(
                    "argument to `len` not supported, got {}",
                    o.kind()
                ))),
            },
            Self::First if args.len() == 1 => match *args[0] {
                Object::Array { ref elements, .. } => elements
                    .first()
                    .map(|elem| elem.clone())
                    .unwrap_or(Rc::new(NULL)),
                ref o => Rc::new(Object::Error(format!(
                    "argument to `first` not supported, got {}",
                    o.kind()
                ))),
            },
            Self::Last if args.len() == 1 => match *args[0] {
                Object::Array { ref elements, .. } => elements
                    .last()
                    .map(|elem| elem.clone())
                    .unwrap_or(Rc::new(NULL)),
                ref o => Rc::new(Object::Error(format!(
                    "argument to `last` not supported, got {}",
                    o.kind()
                ))),
            },
            Self::Rest if args.len() == 1 => match *args[0] {
                Object::Array { ref elements, .. } => Rc::new(Object::Array {
                    elements: elements
                        .iter()
                        .skip(1)
                        .map(|elem| elem.clone())
                        .collect::<Vec<_>>(),
                    context: ObjectContext::Eval,
                }),
                ref o => Rc::new(Object::Error(format!(
                    "argument to `rest` not supported, got {}",
                    o.kind()
                ))),
            },
            Self::Len | Self::First | Self::Last | Self::Rest => Rc::new(Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            ))),
        }
    }
}

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
    Array {
        elements: Vec<Rc<Object>>,
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
            Self::Array { ref elements, .. } => Self::Array {
                elements: elements.clone(),
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
            Self::Array { .. } => "ARRAY",
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
                        .join(", ")
                )?;
                write!(f, ")")?;
                write!(f, "{body}")?;
                Ok(())
            }
            Self::Array { ref elements, .. } => {
                let elements = elements
                    .iter()
                    .map(|elem| elem.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{elements}]")
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
