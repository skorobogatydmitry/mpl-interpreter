use super::object::Object;

#[derive(Clone)]
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
    ]
}

fn len(mut args: Vec<Object>) -> Result<Object, String> {
    if args.len() == 1 {
        match args.pop().unwrap() {
            Object::String(val) => Ok(Object::Integer(val.len() as i64)),
            etc => Err(format!("wrong argument for `len': {}", etc.type_str())),
        }
    } else {
        Err(format!("len expects one argument, got {}", args.len()))
    }
}

fn type_of(mut args: Vec<Object>) -> Result<Object, String> {
    if args.len() == 1 {
        Ok(Object::String(args.pop().unwrap().type_str().to_string()))
    } else {
        Err(format!("len expects one argument, got {}", args.len()))
    }
}
