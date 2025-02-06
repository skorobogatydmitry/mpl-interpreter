use super::*;
use crate::alt::{lexer::Lexer, object::Object, parser::Parser};

/// Program consists of only a block statement
#[test]
fn test_eval_block_statement() {
    let input = "{ let x = 5; return x; }";
    let result = eval_ok_program(input);
    assert_integer_object(result, 5);
}

#[test]
fn test_eval_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("--5", 5),
        ("--10", 10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for (input, expected) in tests {
        let result = eval_ok_program(input);
        assert_integer_object(result, expected);
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 > 2", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];

    for (input, expected) in tests {
        let result = eval_ok_program(input);
        assert_boolean_object(result, expected);
    }
}

#[test]
fn test_string_eval() {
    let input = r#""hello world!""#;
    let result = eval_ok_program(input);

    match result {
        Object::String(val) => assert_eq!("hello world!", val),
        etc => panic!("expected string obj, got {etc}"),
    };
}

#[test]
fn test_string_concatenation() {
    let input = r#""hello" + " " + "world""#;

    let result = eval_ok_program(input);

    match result {
        Object::String(val) => assert_eq!("hello world", val),
        etc => panic!("expected string obj, got {etc}"),
    };
}

#[test]
fn test_eval_bang_operator() {
    let tests = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false), // MPL considers positive integers as true, etc (truthy and falsy values)
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
        ("!!0", false),
    ];

    for (input, expected) in tests {
        let object = eval_ok_program(input);
        assert_boolean_object(object, expected);
    }
}

#[test]
fn test_eval_if_else_expression() {
    let tests = vec![
        ("if (true) { 10; }", Expectation::Int(10)),
        ("if (false) { 10; }", Expectation::Null),
        ("if (1) { 10; }", Expectation::Int(10)),
        ("if (1 < 2) { 10; }", Expectation::Int(10)),
        ("if (1 > 2) { 10; }", Expectation::Null),
        ("if (1 > 2) { 10; } else { 20; }", Expectation::Int(20)),
        ("if (1 < 2) { 10; } else { 20; }", Expectation::Int(10)),
    ];

    for (input, expectation) in tests {
        let result = eval_program(input);
        expectation.assert(result);
    }
}

#[test]
fn test_eval_return_statement() {
    let test = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        ("if (10 > 1) { if (10 > 1) { return 10; }; return 1; }", 10),
    ];

    for (input, expected) in test {
        let object = eval_ok_program(input);
        assert_integer_object(object, expected);
    }
}

#[test]
fn test_eval_errors() {
    let tests = vec![
        ("5 + true;", "unsupported operation: INTEGER + BOOLEAN"),
        ("5 + true; 5;", "unsupported operation: INTEGER + BOOLEAN"),
        ("-true", "unsupported operator: - for BOOLEAN"),
        ("true + false;", "unsupported operation: BOOLEAN + BOOLEAN"),
        (
            "5; true + false; 5",
            "unsupported operation: BOOLEAN + BOOLEAN",
        ),
        (
            "if (10 > 1) { true + false; }",
            "unsupported operation: BOOLEAN + BOOLEAN",
        ),
        (
            "if (10 > 1) {
              if (10 > 1) {
                return true + false;
              };
              return 1;
            }",
            "unsupported operation: BOOLEAN + BOOLEAN",
        ),
        ("foobar", "unknown identifier 'foobar'"),
        (
            r#""hello" - "world""#,
            "unsupported operation: STRING - STRING",
        ),
    ];

    for (input, expected) in tests {
        let result = eval_program(input);
        match result {
            Err(msg) => assert_eq!(expected, msg),
            Ok(obj) => panic!("expected an error, got {}", obj),
        }
    }
}

#[test]
fn test_eval_let_statement() {
    let tests = vec![
        ("let x = 5; x", 5),
        ("let x = 5 * 5; x", 25),
        ("let x = 5; let y = x; y", 5),
        ("let x = 5; let y = x; let z = x + y + 5; z", 15),
    ];

    for (input, expected) in tests {
        let object = eval_ok_program(input);
        assert_integer_object(object, expected);
    }
}

#[test]
fn test_eval_simple_function() {
    let input = "fn(x) { x + 2; }";
    let evaluated = eval_ok_program(input);
    match evaluated {
        Object::Fn(mut func) => {
            assert_eq!(vec!["x"], func.params.into_iter().collect::<Vec<String>>());
            match func.body.statements.pop().unwrap() {
                Statement::Expression(Expression::Infix(expr)) => {
                    assert_eq!(Expression::Identifier("x".to_string()), *expr.left);
                    assert_eq!(Token::Plus, expr.operator);
                    assert_eq!(Expression::Integer(2), *expr.right);
                }
                stmt => panic!("expected infix expression, got {stmt}"),
            }
            assert!(func.body.statements.is_empty());
        }
        _ => panic!("not a function but {}", evaluated),
    };
}

#[test]
fn test_eval_function_call() {
    let tests = vec![
        ("let identity = fn(x) { x; }; identity(5)", 5), // implicit return
        ("let identity = fn(x) { return x; }; identity(5)", 5), // explicit return
        ("fn(x) { x; }(5)", 5),                          // immediate call
        ("let double = fn(x) { x * 2; }; double(5)", 10),
        ("let add = fn(x,y) { return x + y; }; add(5,77)", 82),
        ("let add = fn(x,y) { x + y; }; add(5+5,add(6,7))", 23),
        ("let x = 5; let add = fn(x) { x + 10; }; add(20); x", 5), // env gets preserved
        ("let x = 5; let add = fn(x) { x + 10; }; add(20)", 30), // outer scope doesn't affect the inner
        ("let x = 5; let add = fn() { x + 10; }; add()", 15),    // outer scope vars can be used
    ];

    for (input, expected) in tests {
        let object = eval_ok_program(input);
        assert_integer_object(object, expected);
    }
}

#[test]
fn test_eval_closures() {
    let input = r#"
        let adder = fn(x) {
          fn(y) { x + y; };
        };
        let add_two = adder(2);
        add_two(3);
    "#;

    assert_integer_object(eval_ok_program(input), 5);
}

#[test]
fn test_builtin_function_len() {
    let tests: Vec<(&str, _)> = vec![
        (r#"len("")"#, Expectation::Int(0i64)),
        (r#"len("asd")"#, Expectation::Int(3i64)),
        (r#"len([1,2,3,4])"#, Expectation::Int(4i64)),
        (r#"len([])"#, Expectation::Int(0i64)),
        (
            r#"len(1)"#,
            Expectation::Error("wrong argument for `len': INTEGER".to_string()),
        ),
        (
            r#"len("one", "two")"#,
            Expectation::Error("expected exactly 1 argument, got 2".to_string()),
        ),
    ];

    for (input, expectation) in tests {
        expectation.assert(eval_program(input));
    }
}

#[test]
fn test_builtin_function_first_n_last() {
    let tests: Vec<(&str, _)> = vec![
        (r#"first([])"#, Expectation::Null),
        (r#"first(["asd"])"#, Expectation::String("asd".to_string())),
        (r#"first([1,2,3,4])"#, Expectation::Int(1)),
        (
            r#"first(1)"#,
            Expectation::Error("wrong argument for `first': INTEGER".to_string()),
        ),
        (
            r#"first("one", "two")"#,
            Expectation::Error("expected exactly 1 argument, got 2".to_string()),
        ),
        (r#"last([])"#, Expectation::Null),
        (r#"last(["asd"])"#, Expectation::String("asd".to_string())),
        (r#"last([1,2,3,4])"#, Expectation::Int(4)),
        (
            r#"last(1)"#,
            Expectation::Error("wrong argument for `last': INTEGER".to_string()),
        ),
        (
            r#"last("one", "two")"#,
            Expectation::Error("expected exactly 1 argument, got 2".to_string()),
        ),
    ];

    for (input, expectation) in tests {
        expectation.assert(eval_program(input));
    }
}

#[test]
fn test_builtin_function_rest() {
    let tests: Vec<(&str, _)> = vec![
        (r#"rest([])"#, Expectation::Null),
        (r#"rest(["asd"])"#, Expectation::Array(vec![])),
        (
            r#"rest([1,2,"3",4])"#,
            Expectation::Array(vec![
                Box::new(Expectation::Int(2)),
                Box::new(Expectation::String("3".to_string())),
                Box::new(Expectation::Int(4)),
            ]),
        ),
        (
            r#"rest(1)"#,
            Expectation::Error("wrong argument for `rest': INTEGER".to_string()),
        ),
        (
            r#"rest("one", "two")"#,
            Expectation::Error("expected exactly 1 argument, got 2".to_string()),
        ),
    ];

    for (input, expectation) in tests {
        expectation.assert(eval_program(input));
    }
}

#[test]
fn test_builtin_function_push() {
    let tests: Vec<(&str, _)> = vec![
        (
            r#"push([], 1)"#,
            Expectation::Array(vec![Box::new(Expectation::Int(1))]),
        ),
        (
            r#"push([1,2,"3",4], 5)"#,
            Expectation::Array(vec![
                Box::new(Expectation::Int(1)),
                Box::new(Expectation::Int(2)),
                Box::new(Expectation::String("3".to_string())),
                Box::new(Expectation::Int(4)),
                Box::new(Expectation::Int(5)),
            ]),
        ),
        (
            r#"push(1)"#,
            Expectation::Error("push expects two arguments, got 1".to_string()),
        ),
        (
            r#"push("one", "two")"#,
            Expectation::Error("cannot `push' to `STRING'".to_string()),
        ),
    ];

    for (input, expectation) in tests {
        expectation.assert(eval_program(input));
    }
}

#[test]
fn test_builtin_function_typeof() {
    let tests = vec![
        ("typeof(1)", "INTEGER"),
        ("typeof(\"x\")", "STRING"),
        ("typeof(len)", "BUILTIN_FUNCTION"),
    ];
    for (input, expected) in tests {
        let obj = eval_ok_program(input);
        match obj {
            Object::String(val) => assert_eq!(expected, val),
            etc => panic!("expected string, got {etc}"),
        }
    }
}

#[test]
fn test_disallow_builtin_override() {
    let input = "let len = 10;";
    let result = eval_program(input);
    Expectation::Error("cannot override builtin `len'".to_string()).assert(result);
}

#[test]
fn test_eval_array() {
    let input = "[1,2*2,3+3]";
    let object = eval_ok_program(input);

    match object {
        Object::Array(mut arr) => {
            match arr.pop().unwrap() {
                Object::Integer(val) => assert_eq!(6, val),
                etc => panic!("expected integer, got {etc}"),
            };
            match arr.pop().unwrap() {
                Object::Integer(val) => assert_eq!(4, val),
                etc => panic!("expected integer, got {etc}"),
            };
            match arr.pop().unwrap() {
                Object::Integer(val) => assert_eq!(1, val),
                etc => panic!("expected integer, got {etc}"),
            };
        }
        etc => panic!("expected an array, got {etc}"),
    }
}

#[test]
fn test_eval_index() {
    let tests = vec![
        ("[1, 2, 3][0]", Expectation::Int(1_i64)),
        ("[1, 2, 3][1]", Expectation::Int(2_i64)),
        ("[1, 2, 3][2]", Expectation::Int(3_i64)),
        ("let i = 0; [1][i];", Expectation::Int(1_i64)),
        ("[1, 2, 3][1 + 1];", Expectation::Int(3_i64)),
        (
            "let myArray = [1, 2, 3]; myArray[2];",
            Expectation::Int(3_i64),
        ),
        (
            "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            Expectation::Int(6_i64),
        ),
        (
            "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            Expectation::Int(2_i64),
        ),
        ("[1, 2, 3][3]", Expectation::Null),
        ("[1, 2, 3][-1]", Expectation::Null),
    ];

    for (input, expectation) in tests {
        let object = eval_program(input);
        expectation.assert(object);
    }
}

#[test]
fn test_hash_literal() {
    // TODO: add trisky keys & values e.g. let x = { fn(a,b) { a + b }: "sum" };
    let input = r#"
        let two = "two";
        let x = {
          "one": 10-9,
          "two": 1+1,
          "thr" + "ee": 6/2,
          4: 4,
          true: 5,
          false:6
        }"#;
    let object = eval_ok_program(input);
    match object {
        Object::Hash(mut result) => {
            let expected = vec![
                (Object::String("one".to_string()), 1),
                (Object::String("two".to_string()), 2),
                (Object::String("three".to_string()), 3),
                (Object::Integer(4), 4),
                (Object::get_bool(true), 5),
                (Object::get_bool(false), 6),
            ];

            for (expected_key, expected_val) in expected {
                match result.remove(&expected_key) {
                    Some(actual_val) => {
                        assert_integer_object(actual_val, expected_val);
                    }
                    None => panic!("no value for key {expected_key:?}"),
                }
            }
        }
        other => panic!("expected hash, got {other}"),
    }
}

#[test]
fn test_hash_index_expressions() {
    let tests = vec![
        (r#"let x = {"foo": 5}["foo"]"#, Expectation::Int(5_i64)),
        (r#"let x = {"foo": 5}["bar"]"#, Expectation::Null),
        (
            r#"let key = "foo"; let x = {"foo": 5}[key]"#,
            Expectation::Int(5_i64),
        ),
        (r#"let x = {}["foo"]"#, Expectation::Null),
        (r#"let x = {5: 5}[5]"#, Expectation::Int(5_i64)),
        (r#"let x = {true: 5}[true]"#, Expectation::Int(5_i64)),
        (r#"let x = {false: 5}[false]"#, Expectation::Int(5_i64)),
    ];

    for (input, expectation) in tests {
        let object = eval_program(input);
        expectation.assert(object);
    }
}

#[derive(Debug)]
enum Expectation {
    Int(i64),
    String(String),
    Error(String),
    Array(Vec<Box<Expectation>>),
    Null,
}

impl Expectation {
    fn assert(self, obj: Result<Object, String>) {
        match (self, obj) {
            (Expectation::Int(exp), Ok(obj)) => assert_integer_object(obj, exp),
            (Expectation::String(exp), Ok(Object::String(str))) => assert_eq!(exp, str),
            (Expectation::Array(exps), Ok(Object::Array(arr))) => {
                if exps.len() != arr.len() {
                    panic!(
                        "expected arry len ({}) != received array len ({})",
                        exps.len(),
                        arr.len()
                    );
                }
                for (exp, obj) in exps.into_iter().zip(arr) {
                    exp.assert(Ok(obj));
                }
            }
            (Expectation::Error(exp), Err(msg)) => {
                assert_eq!(exp, msg)
            }
            (Expectation::Null, Ok(Object::Null)) => (),
            (exp, obj) => panic!("cannot compare {:?} and {:?}", exp, obj),
        };
    }
}

/// evaluate the program by input
fn eval_program(input: &str) -> Result<Object, String> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    match program {
        Ok(program) => Evaluator::new().eval_program(program),
        Err(msg) => panic!("{}", msg),
    }
}

/// expects program to return Ok
fn eval_ok_program(input: &str) -> Object {
    let obj = eval_program(input);
    match obj {
        Ok(obj) => obj,
        Err(msg) => panic!("{}", msg),
    }
}

fn assert_integer_object(obj: Object, expected: i64) {
    assert_eq!("INTEGER", obj.type_str());
    match obj {
        Object::Integer(val) => assert_eq!(expected, val),
        other => panic!("not an integer but {other}"),
    }
}

fn assert_boolean_object(obj: Object, expected: bool) {
    assert_eq!("BOOLEAN", obj.type_str());
    match obj {
        Object::Boolean(val) => assert_eq!(expected, val),
        other => panic!("not a bool but {other}"),
    }
}
