use crate::object::Object;

pub struct Builtins;

impl Builtins {
    pub fn get_all() -> Vec<(String, Object)> {
        vec![
            ("len".to_string(), Object::BuiltinFunction(Self::len)),
            ("first".to_string(), Object::BuiltinFunction(Self::first)),
            ("last".to_string(), Object::BuiltinFunction(Self::last)),
            ("rest".to_string(), Object::BuiltinFunction(Self::rest)),
            ("push".to_string(), Object::BuiltinFunction(Self::push)),
            ("puts".to_string(), Object::BuiltinFunction(Self::puts)),
        ]
    }

    fn len(mut args: Vec<Object>) -> Object {
        if args.len() == 1 {
            match args.pop().unwrap() {
                Object::String(val) => Object::Integer(val.len() as i64),
                Object::Array(arr) => Object::Integer(arr.len() as i64),
                etc => Object::Error(format!("wrong argument for `len': {}", etc.type_str())),
            }
        } else {
            Object::Error(format!("len expects one argument, got {}", args.len()))
        }
    }

    fn first(mut args: Vec<Object>) -> Object {
        if args.len() == 1 {
            match args.pop().unwrap() {
                Object::Array(arr) => {
                    if arr.is_empty() {
                        Object::get_null()
                    } else {
                        arr.first().unwrap().clone()
                    }
                }
                etc => Object::Error(format!("wrong argument for `first': {}", etc.type_str())),
            }
        } else {
            Object::Error(format!("first expects one argument, got {}", args.len()))
        }
    }

    fn last(mut args: Vec<Object>) -> Object {
        if args.len() == 1 {
            match args.pop().unwrap() {
                Object::Array(arr) => {
                    if arr.is_empty() {
                        Object::get_null()
                    } else {
                        arr.last().unwrap().clone()
                    }
                }
                etc => Object::Error(format!("wrong argument for `last': {}", etc.type_str())),
            }
        } else {
            Object::Error(format!("last expects one argument, got {}", args.len()))
        }
    }

    fn rest(mut args: Vec<Object>) -> Object {
        if args.len() == 1 {
            match args.pop().unwrap() {
                Object::Array(arr) => {
                    if arr.is_empty() {
                        Object::get_null()
                    } else {
                        return Object::Array(arr[1..].to_vec());
                    }
                }
                etc => Object::Error(format!("wrong argument for `rest': {}", etc.type_str())),
            }
        } else {
            Object::Error(format!("rest expects one argument, got {}", args.len()))
        }
    }

    fn push(mut args: Vec<Object>) -> Object {
        if args.len() == 2 {
            let object = args.pop().unwrap();
            let array = args.pop().unwrap();
            match array {
                Object::Array(mut arr) => {
                    arr.push(object);
                    Object::Array(arr)
                }
                etc => Object::Error(format!("wrong argument for `push': {}", etc.type_str())),
            }
        } else {
            Object::Error(format!("push expects two arguments, got {}", args.len()))
        }
    }

    fn puts(args: Vec<Object>) -> Object {
        for arg in args {
            println!("{}", arg);
        }
        Object::get_null()
    }
}
