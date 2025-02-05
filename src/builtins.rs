use crate::object::Object;

pub struct Builtins;

impl Builtins {
    pub fn get_all() -> Vec<(String, Object)> {
        vec![("len".to_string(), Object::BuiltinFunction(Self::len))]
    }

    fn len(mut args: Vec<Object>) -> Object {
        if args.len() == 1 {
            match args.pop().unwrap() {
                Object::String(val) => Object::Integer(val.len() as i64),
                etc => Object::Error(format!("wrong argument for `len': {}", etc.type_str())),
            }
        } else {
            Object::Error(format!("len expects one argument, got {}", args.len()))
        }
    }
}
