use super::object::{Object, NULL};

#[derive(Debug, Clone)]
pub struct BuiltinFn {
    pub f: fn(Vec<Object>) -> Result<Object, String>,
    pub desc: String,
}

/// return all available builtin functions to construct env
pub fn get_functions() -> Vec<(String, Object)> {
    vec![
        (
            "len".to_string(),
            Object::BuiltinFn(BuiltinFn {
                f: len,
                desc: "get # of characters in a single provided String".to_string(),
            }),
        ),
        (
            "typeof".to_string(),
            Object::BuiltinFn(BuiltinFn {
                f: type_of,
                desc: "get object type name as a string".to_string(),
            }),
        ),
        (
            "first".to_string(),
            Object::BuiltinFn(BuiltinFn {
                f: first,
                desc: "get first element of a collection".to_string(),
            }),
        ),
        (
            "last".to_string(),
            Object::BuiltinFn(BuiltinFn {
                f: last,
                desc: "get last element of a collection".to_string(),
            }),
        ),
        (
            "rest".to_string(),
            Object::BuiltinFn(BuiltinFn {
                f: rest,
                desc: "get all but the first element of a collection".to_string(),
            }),
        ),
        (
            "push".to_string(),
            Object::BuiltinFn(BuiltinFn {
                f: push,
                desc: "takes a collection and another object, return new collection extended by the element".to_string(),
            }),
        ),
    ]
}

fn len(args: Vec<Object>) -> Result<Object, String> {
    match expect_one_arg(args)? {
        Object::String(val) => Ok(Object::Integer(val.len() as i64)),
        Object::Array(val) => Ok(Object::Integer(val.len() as i64)),
        etc => Err(format!("wrong argument for `len': {}", etc.type_str())),
    }
}

fn type_of(args: Vec<Object>) -> Result<Object, String> {
    Ok(Object::String(expect_one_arg(args)?.type_str().to_string()))
}

fn first(args: Vec<Object>) -> Result<Object, String> {
    match expect_one_arg(args)? {
        Object::Array(arr) => Ok(if arr.is_empty() {
            NULL
        } else {
            arr.first().unwrap().clone()
        }),
        etc => Err(format!("wrong argument for `first': {}", etc.type_str())),
    }
}

fn last(args: Vec<Object>) -> Result<Object, String> {
    match expect_one_arg(args)? {
        Object::Array(arr) => Ok(if arr.is_empty() {
            NULL
        } else {
            arr.last().unwrap().clone()
        }),
        etc => Err(format!("wrong argument for `last': {}", etc.type_str())),
    }
}

fn rest(args: Vec<Object>) -> Result<Object, String> {
    match expect_one_arg(args)? {
        Object::Array(arr) => Ok(if arr.is_empty() {
            NULL
        } else {
            Object::Array(arr[1..].to_vec())
        }),
        etc => Err(format!("wrong argument for `rest': {}", etc.type_str())),
    }
}

fn push(mut args: Vec<Object>) -> Result<Object, String> {
    if args.len() == 2 {
        let object = args.pop().unwrap();
        let array = args.pop().unwrap();
        match array {
            Object::Array(mut arr) => {
                // TODO: make it inplace once got rid of .clone in the evaluator
                arr.push(object);
                Ok(Object::Array(arr))
            }
            etc => Err(format!("cannot `push' to `{}'", etc.type_str())),
        }
    } else {
        Err(format!("push expects two arguments, got {}", args.len()))
    }
}

/// helper to check for # of args
fn expect_one_arg(mut args: Vec<Object>) -> Result<Object, String> {
    if args.len() == 1 {
        Ok(args.pop().unwrap())
    } else {
        Err(format!("expected exactly 1 argument, got {}", args.len()))
    }
}
