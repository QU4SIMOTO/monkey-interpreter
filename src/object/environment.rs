use crate::object::object::Object;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<Rc<String>, Rc<Object>>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::default(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Environment) -> Self {
        Self {
            store: HashMap::default(),
            outer: Some(Box::from(outer)),
        }
    }

    pub fn get(&self, name: Rc<String>) -> Option<&Rc<Object>> {
        self.store.get(name.as_ref()).or_else(|| {
            if let Some(outer) = &self.outer {
                outer.get(name)
            } else {
                None
            }
        })
    }

    pub fn set(&mut self, name: &Rc<String>, val: Rc<Object>) {
        self.store.insert(name.clone(), val);
    }
}
