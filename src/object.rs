use std::{collections::HashMap, fmt::Display};

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
