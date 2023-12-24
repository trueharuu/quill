use std::collections::BTreeMap;

use super::value::Value;

pub struct Env {
    pub values: BTreeMap<String, Value>,
}

impl Env {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            values: BTreeMap::new(),
        }
    }
}
