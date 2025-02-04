use super::*;
use crate::alt::{lexer::Lexer, object::Object, parser::Parser};

/// Program consists of only a block statement
#[test]
fn test_eval_block_statement() {
    let input = "{ let x = 5; return x; }";
    let result = eval_program(input);
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
        let result = eval_program(input);
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
        let result = eval_program(input);
        assert_boolean_object(result, expected);
    }
}

#[test]
fn test_string_eval() {
    let input = r#""hello world!""#;
    let result = eval_program(input);

    match result {
        Object::String(val) => assert_eq!("hello world!", val),
        etc => panic!("expected string obj, got {etc}"),
    };
}

#[test]
fn test_string_concatenation() {
    let input = r#""hello" + " " + "world""#;

    let result = eval_program(input);

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
        let obj = eval_program(input);
        assert_boolean_object(obj, expected);
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
        let evaluated = eval_program(input);
        expectation.assert(evaluated);
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
        let object = eval_program(input);
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
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().expect("should not be an error");
        let object = Evaluator::new().eval_program(program);
        match object {
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
        let object = eval_program(input);
        assert_integer_object(object, expected);
    }
}

#[test]
fn test_eval_simple_function() {
    let input = "fn(x) { x + 2; }";
    let evaluated = eval_program(input);
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
        let object = eval_program(input);
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

    assert_integer_object(eval_program(input), 5);
}

#[derive(Debug)]
enum Expectation {
    Int(i64),
    Null,
}

impl Expectation {
    fn assert(self, obj: Object) {
        match (self, obj) {
            (Expectation::Int(exp), obj) => assert_integer_object(obj, exp),
            (Expectation::Null, Object::Null) => (),
            (exp, obj) => panic!("cannot compare {:?} and {}", exp, obj),
        };
    }
}

fn eval_program(input: &str) -> Object {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    match program {
        Ok(program) => {
            let obj = Evaluator::new().eval_program(program);
            match obj {
                Ok(obj) => obj,
                Err(msg) => panic!("{}", msg),
            }
        }
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
