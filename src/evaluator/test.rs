use super::*;
use crate::{
    ast::{Node, Program},
    lexer::Lexer,
    object::Object,
    parser::Parser,
};

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
        let evaluated = Evaluator::new().eval_program(make_program(input));
        assert_integer_object(evaluated, expected);
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
        let evaluated = Evaluator::new().eval_program(make_program(input));
        assert_boolean_object(evaluated, expected);
    }
}

#[test]
fn test_string_eval() {
    let input = r#""hello world!""#;
    let evaluated = Evaluator::new().eval_program(make_program(input));

    match evaluated {
        Object::String(val) => assert_eq!("hello world!", val),
        etc => panic!("expected string obj, got {etc:?}"),
    };
}

#[test]
fn test_string_concatenation() {
    let input = r#""hello" + " " + "world""#;

    let evaluated = Evaluator::new().eval_program(make_program(input));

    match evaluated {
        Object::String(val) => assert_eq!("hello world", val),
        etc => panic!("expected string obj, got {etc:?}"),
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
        let evaluated = Evaluator::new().eval_program(make_program(input));
        assert_boolean_object(evaluated, expected);
    }
}

#[test]
fn test_eval_if_else_expression() {
    let tests = vec![
        ("if (true) { 10 }", Expectation::Int(10)),
        ("if (false) { 10 }", Expectation::Null),
        ("if (1) { 10 }", Expectation::Int(10)),
        ("if (1 < 2) { 10 }", Expectation::Int(10)),
        ("if (1 > 2) { 10 }", Expectation::Null),
        ("if (1 > 2) { 10 } else { 20 }", Expectation::Int(20)),
        ("if (1 < 2) { 10 } else { 20 }", Expectation::Int(10)),
    ];

    for (input, expected) in tests {
        let evaluated = Evaluator::new().eval_program(make_program(input));
        expected.assert(evaluated);
    }
}

#[test]
fn test_eval_return_statement() {
    let test = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        ("if (10 > 1) { if (10 > 1) { return 10 } return 1; }", 10),
    ];

    for (input, expected) in test {
        let evaluated = Evaluator::new().eval_program(make_program(input));
        assert_integer_object(evaluated, expected);
    }
}

#[test]
fn test_eval_errors() {
    let tests = vec![
        ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
        ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
        ("-true", "unknown operator: -BOOLEAN"),
        ("true + false;", "type mismatch: BOOLEAN + BOOLEAN"),
        ("5; true + false; 5", "type mismatch: BOOLEAN + BOOLEAN"),
        (
            "if (10 > 1) { true + false; }",
            "type mismatch: BOOLEAN + BOOLEAN",
        ),
        (
            "if (10 > 1) {
            if (10 > 1) {
            return true + false;
            }
            return 1;
            }
            ",
            "type mismatch: BOOLEAN + BOOLEAN",
        ),
        ("foobar", "unknown identifier 'foobar'"),
        (r#""hello" - "world""#, "type mismatch: STRING - STRING"),
        // TODO: ("10 ! 5", "unknown operation ! for INTEGER"),
    ];

    for (input, expected) in tests {
        let evaluated = Evaluator::new().eval_program(make_program(input));
        match evaluated {
            Object::Error(msg) => assert_eq!(expected, msg),
            obj => panic!("expected an error, got {}", obj),
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
        let evaluated = Evaluator::new().eval_program(make_program(input));
        assert_integer_object(evaluated, expected);
    }
}

#[test]
fn test_eval_simple_function() {
    let input = "fn(x) { x + 2; }";
    let evaluated = Evaluator::new().eval_program(make_program(input));
    match evaluated {
        Object::Fn(func) => {
            assert_eq!(
                vec!["x"],
                func.params
                    .into_iter()
                    .map(|p| p.value)
                    .collect::<Vec<String>>()
            );
            assert_eq!("(x + 2)", func.body.print());
        }
        _ => panic!("not a function but {:?}", evaluated),
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
        let evaluated = Evaluator::new().eval_program(make_program(input));
        assert_integer_object(evaluated, expected);
    }
}

#[test]
fn test_eval_closures() {
    let input = r#"
        let adder = fn(x) {
          fn(y) { x + y };
        };
        let add_two = adder(2);
        add_two(3);
    "#;

    assert_integer_object(Evaluator::new().eval_program(make_program(input)), 5);
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
            Expectation::Error("len expects one argument, got 2".to_string()),
        ),
    ];

    for (input, expectation) in tests {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("should not be an error");
        let object = Evaluator::new().eval_program(program);

        expectation.assert(object);
    }
}

#[test]
fn test_eval_array() {
    let input = "[1,2*2,3+3]";
    let object = Evaluator::new().eval_program(make_program(input));

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
        let object = Evaluator::new().eval_program(make_program(input));
        expectation.assert(object);
    }
}

#[derive(Debug)]
enum Expectation {
    Int(i64),
    Error(String),
    Null,
}

impl Expectation {
    fn assert(self, obj: Object) {
        match (self, obj) {
            (Expectation::Int(exp), obj) => assert_integer_object(obj, exp),
            (Expectation::Null, Object::Null) => (),
            (Expectation::Error(exp), Object::Error(msg)) => assert_eq!(exp, msg),
            (exp, obj) => panic!("cannot compare {:?} and {:?}", exp, obj),
        };
    }
}

fn make_program(input: &str) -> Program {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    assert!(program.is_some());
    program.unwrap()
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
