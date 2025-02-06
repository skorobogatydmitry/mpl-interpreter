use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    fmt::Display,
    hash::{Hash, Hasher},
};

use crate::{
    ast::{BlockStatement, Identifier, Node},
    builtins::Builtins,
};

// save some memory by stating constants
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub type BuiltinFunction = fn(Vec<Object>) -> Object;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64), // integer type of MPL uses i64 under the hood
    Boolean(bool),
    String(String),
    ReturnValue(Box<Object>),
    Fn(Function),
    BuiltinFunction(BuiltinFunction),
    Array(Vec<Object>),
    Hash(HashObj),
    Error(String),
    Null,
}

impl Object {
    pub fn type_str(&self) -> &str {
        match self {
            Self::Integer(_) => "INTEGER",
            Self::Boolean(_) => "BOOLEAN",
            Self::ReturnValue(_) => "RETURN_VALUE",
            Self::Error(_) => "ERROR",
            Self::Fn(_) => "FUNCTION",
            Self::String(_) => "STRING",
            Self::BuiltinFunction(_) => "BUILTIN",
            Self::Array(_) => "ARRAY",
            Self::Hash(_) => "HASH",
            Self::Null => "NULL",
        }
    }

    /// always return the same constant
    pub fn get_bool(val: bool) -> Object {
        if val {
            TRUE
        } else {
            FALSE
        }
    }

    pub fn get_null() -> Object {
        NULL
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Null => false,
            Object::Boolean(bool) => *bool,
            Object::Integer(int) => *int > 0,
            _ => false,
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
            Self::Error(val) => write!(f, "{}", val),
            Self::Fn(val) => {
                let params = val
                    .params
                    .iter()
                    .map(|p| p.print())
                    .collect::<Vec<String>>();
                write!(f, "fn({}) {{\n{}\n}}", params.join(","), val.body.print())
            }
            Self::BuiltinFunction(_) => write!(f, "builtin function"),
            Self::Array(data) => write!(
                f,
                "[{}]",
                data.iter()
                    .map(|el| format!("{}", el))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Hash(data) => write!(
                f,
                "[{{{}}}]",
                data.pairs
                    .iter()
                    .map(|(_, el)| format!("{}: {}", el.key, el.value))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment(HashMap<String, Object>);

impl Environment {
    pub fn new() -> Self {
        Environment(Self::init_builtins())
    }

    fn init_builtins() -> HashMap<String, Object> {
        let mut map = HashMap::new();
        for (name, obj) in Builtins::get_all() {
            map.insert(name, obj);
        }
        map
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.0.get(name)
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.0.insert(name, value);
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment, // own local scope
}

#[derive(Debug, Clone)]
pub struct HashPair {
    pub key: Object,
    pub value: Object,
}

#[derive(Debug, Clone)]
pub struct HashObj {
    pub pairs: HashMap<HashKey, HashPair>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HashKey {
    kind: String,
    hash: i64,
}

pub trait Hashable {
    fn hash_key(&self) -> Result<HashKey, String>;
}

impl Hashable for Object {
    fn hash_key(&self) -> Result<HashKey, String> {
        Ok(match self {
            Object::Boolean(val) => HashKey {
                kind: self.type_str().to_string(),
                hash: if *val { 1 } else { 0 },
            },
            Object::Integer(val) => HashKey {
                kind: self.type_str().to_string(),
                hash: *val,
            },
            Object::String(val) => {
                let mut hashier = DefaultHasher::new();
                val.hash(&mut hashier);
                HashKey {
                    kind: self.type_str().to_string(),
                    hash: hashier.finish() as i64,
                }
            }
            o => return Err(format!("can't be a hash key: {o}")),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_object_hash_key() {
        let some1 = Object::String("some".to_string());
        let some2 = Object::String("some".to_string());
        let other = Object::String("other".to_string());

        assert_eq!(some1.hash_key(), some2.hash_key());
        assert_ne!(some1.hash_key(), other.hash_key());
        assert_ne!(some2.hash_key(), other.hash_key());
    }
}
