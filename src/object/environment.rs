use crate::object::object::Object;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Environment {
    store: HashMap<Rc<String>, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::default(),
        }
    }

    pub fn get(&self, name: &Rc<String>) -> Option<&Object> {
        self.store.get(name)
    }

    pub fn set(&mut self, name: &Rc<String>, val: Object) {
        self.store.insert(name.clone(), val);
    }
}
