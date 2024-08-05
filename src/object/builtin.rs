use crate::object::object::{Object, ObjectContext, NULL};
use std::rc::Rc;

pub enum Builtins {
    Len,
    First,
    Last,
    Rest,
    Puts,
}

impl Builtins {
    pub fn get(name: &str) -> Option<Self> {
        match name {
            "len" => Some(Self::Len),
            "first" => Some(Self::First),
            "last" => Some(Self::Last),
            "rest" => Some(Self::Rest),
            "puts" => Some(Self::Puts),
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
            Self::Puts => {
                for arg in args {
                    println!("{arg}")
                }
                NULL.into()
            }
            Self::Len | Self::First | Self::Last | Self::Rest => Rc::new(Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            ))),
        }
    }
}
