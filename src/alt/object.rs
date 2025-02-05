use std::{collections::HashMap, fmt::Display};

use crate::alt::ast::statement::Block;

use super::builtins::{self, BuiltinFn};

// save some memory by stating constants
pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;
pub const INT_ZERO: Object = Object::Integer(0);

/// Evaluation results for individual statements
/// Clone is required to store objects in the environment
#[derive(Clone)]
pub enum Object {
    Integer(i64), // integer type of MPL uses i64 under the hood
    Boolean(bool),
    String(String),
    ReturnValue(Box<Object>),
    Fn(Function),
    BuiltinFn(BuiltinFn),
    Null,
}

impl Object {
    pub fn type_str(&self) -> &str {
        match self {
            Self::Integer(_) => "INTEGER",
            Self::Boolean(_) => "BOOLEAN",
            Self::ReturnValue(_) => "RETURN_VALUE",
            Self::Fn(_) => "FUNCTION",
            Self::String(_) => "STRING",
            Self::BuiltinFn(_) => "BUILTIN_FUNCTION",
            Self::Null => "NULL",
        }
    }

    /// whether an object equals to true or false
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Null => false,
            Object::Boolean(bool) => *bool,
            Object::Integer(int) => *int > 0,
            Object::String(val) => !val.is_empty(),
            _ => false,
        }
    }

    /// always return the same constant
    pub fn get_bool(val: bool) -> Self {
        if val {
            TRUE
        } else {
            FALSE
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(val) => write!(f, "{}", val),
            Self::Boolean(val) => write!(f, "{}", val),
            Self::ReturnValue(val) => write!(f, "{}", val),
            Self::String(val) => write!(f, "{}", val),
            Self::Fn(val) => {
                write!(f, "fn({}) {}", val.params.join(","), val.body)
            }
            Self::BuiltinFn(val) => write!(f, "builtin function: {}", val.desc),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Clone)]
pub struct Environment(HashMap<String, Object>);

impl Environment {
    pub fn new() -> Self {
        Environment(builtins::get_functions().into_iter().collect())
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.0.get(name)
    }

    pub fn set(&mut self, name: String, value: Object) -> Result<(), String> {
        match self.0.get(&name) {
            Some(Object::BuiltinFn(_)) => return Err("cannot override builtin `len'".to_string()),
            _ => {
                self.0.insert(name, value);
                Ok(())
            }
        }
    }
}

#[derive(Clone)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Block,
    pub env: Environment, // own local scope
}

#[cfg(test)]
mod test {
    use crate::alt::object::Object;

    #[test]
    fn test_booleans_cast() {
        assert!(!super::NULL.is_truthy());
        assert!(Object::get_bool(true).is_truthy());
        assert!(!Object::get_bool(false).is_truthy());
        assert!(!Object::Integer(-1).is_truthy());
        assert!(!Object::Integer(0).is_truthy());
        assert!(Object::Integer(1).is_truthy());
        assert!(!Object::String("".to_string()).is_truthy());
        assert!(Object::String(" ".to_string()).is_truthy());
    }
}
