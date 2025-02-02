use super::*;
use crate::alt::ast::Statement;

#[test]
fn test_let_statement() {
    let input = r#"
          let x = 5;
          let y = true;
          let foo_bar = y;
          "#;

    let program = make_program_from(input, Some(3));

    match &program.statements[0] {
        Statement::Let(data) => {
            assert_eq!("x", data.name);
            match &data.value {
                Some(Expression::Integer(val)) => assert_eq!(5, *val),
                _ => panic!("value is not an integer but {:?}", data.value),
            }
        }
        some => panic!("not a let statement but {some:?}"),
    }

    match &program.statements[1] {
        Statement::Let(data) => {
            assert_eq!("y", data.name);
            match &data.value {
                Some(Expression::Boolean(val)) => assert_eq!(true, *val),
                _ => panic!("value is not a bool but {:?}", data.value),
            }
        }
        some => panic!("not a let statement but {some:?}"),
    }

    match &program.statements[2] {
        Statement::Let(data) => {
            assert_eq!("foo_bar", data.name);
            match &data.value {
                Some(Expression::Identifier(idf)) => assert_eq!("y", idf),
                _ => panic!("value is not an identifier but {:?}", data.value),
            }
        }
        some => panic!("not a let statement but {some:?}"),
    }
}

#[test]
fn test_parsing_errors() {
    let inputs = vec![
        ("let", "no tokens after `let'"),
        (
            "let ! = 2;",
            "error parsing let statement: expected identifier, got Bang",
        ),
        ("let x", "no tokens after `let x', expected `='"),
        (
            "let x = !=1;",
            "wrong expression for the let statement: no prefix parsing fn for token kind NotEq",
        ),
        (
            "let x ! 2;",
            "error parsing let statement: expected `=', got Bang",
        ),
        (
            "9999999999999999999999999",
            "error parsing expression: cannot parse 9999999999999999999999999 as integer(i64): ParseIntError { kind: PosOverflow }",
        ),
        ("(!=2)", "error parsing expression: (groupped expression): inner expression parsing failed: no prefix parsing fn for token kind NotEq"),
        ("(!0; let x = 5;", "error parsing expression: (groupped expression): expected `)`, got Some(Semicolon)"),
        ("let x = 5; if", "error parsing expression: (if expression): no token after `if' to parse"),
        ("if 2 { 5 };", "error parsing expression: (if expression): expected ( for if condition, got Int(\"2\")"),
        ("if (!=1)", "error parsing expression: (if expression): cannot parse condition of the if expression: inner expression parsing failed: no prefix parsing fn for token kind NotEq"),
        ("if (true) !=", "error parsing expression: (if expression): invalid syntax: expected `{', got Some(NotEq)"),
        ("if (true) { !=5 }", "error parsing expression: (if expression): error parsing consequence: error parsing expression: no prefix parsing fn for token kind NotEq"),
        ("if (true) { 5 } else { !=5 }", "error parsing expression: (if expression): else's block: error parsing expression: no prefix parsing fn for token kind NotEq"),
        ("if (true) { 5 } else !=", "error parsing expression: (if expression): expected `else {', got `else Some(NotEq)'"),
        ("let x = 5; fn", "error parsing expression: (fn expression): no token after `fn'"),
        ("let x = 5; fn==", "error parsing expression: (fn expression): no `(' after `fn'"),
        ("fn(a,b)", "error parsing expression: (fn expression): no tokens after fn's parameters fn(a,b)"),
        ("fn(a) ==", "error parsing expression: (fn expression): no `{' after fn(a)"),
        ("fn(a|b)", "error parsing expression: (fn expression): wrong parameters separator: expected `,' or `)', got Some(Illegal(\"|\"))"),
        ("fn(a,b,c, { 5 }", "error parsing expression: (fn expression): incorrect parameters declaration: missing `,' or `)'"),
        ("fn() { 5;", "error parsing expression: (fn expression): block is not closed with `}'"),
        ("fn() { let x != 5; }", "error parsing expression: (fn expression): error parsing let statement: expected `=', got NotEq"),
        ("!=5", "error parsing expression: no prefix parsing fn for token kind NotEq"),
        ("!", "error parsing expression: no token to parse an expression"),
        ("add(a + 5|b)", "error parsing expression: (infix expression): wrong fn call args separator: Illegal(\"|\")"),
        ("add(1,2,3 + 4, !=5", "error parsing expression: (infix expression): error parsing fn call args: no prefix parsing fn for token kind NotEq (already parsed: [Integer(1), Integer(2), Infix(Infix { operator: Plus, left: Integer(3), right: Integer(4) })])"),
        ("add(1,2,3", "error parsing expression: (infix expression): found fn call args [Integer(1), Integer(2), Integer(3)] but no `)' seen"),
    ];

    for (input, expected) in inputs {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let err = parser.parse_program().expect_err("should be an error");

        assert_eq!(format!("error parsing the program: {}", expected), err);
    }
}

#[test]
fn test_return_statement() {
    let input = r#"
            return 1;
            return 10;
            return x + t;
            "#;

    // let input = r#"
    //         return x + t;
    //         "#;

    let mut program = make_program_from(input, Some(3));

    match program.statements.pop().unwrap() {
        Statement::Return(stmt) => {
            assert!(stmt.ret_expr.is_some());
            if let Expression::Infix(expr) = stmt.ret_expr.unwrap() {
                assert_infix_expression(
                    expr,
                    Expectation::String("x".to_string()),
                    Token::Plus,
                    Expectation::String("t".to_string()),
                );
            }
        }
        some => panic!("not a return statement but {some:?}"),
    }

    match program.statements.pop().unwrap() {
        Statement::Return(stmt) => {
            assert!(stmt.ret_expr.is_some());
            assert_literal_expr(stmt.ret_expr.unwrap(), Expectation::Int(10));
        }
        some => panic!("not a return statement but {some:?}"),
    }

    match program.statements.pop().unwrap() {
        Statement::Return(stmt) => {
            assert!(stmt.ret_expr.is_some());
            assert_literal_expr(stmt.ret_expr.unwrap(), Expectation::Int(1));
        }
        some => panic!("not a return statement but {some:?}"),
    }
}

#[test]
fn test_indentifier_expression() {
    let inputs = vec!["foobar", "foobar;"];
    for input in inputs {
        let mut program = make_program_from(input, Some(1));
        match program.statements.pop().unwrap() {
            Statement::Expression(Expression::Identifier(idf)) => assert_eq!("foobar", idf),
            expr => panic!("expected identifier expression statement: {expr:?}"),
        }
    }
}

#[test]
fn test_empty_expression_statement() {
    let input = "5;;";
    let mut program = make_program_from(input, Some(2));
    if let Some(Statement::Expression(Expression::Empty)) = program.statements.pop() {}
}

#[test]
fn test_literal_expression() {
    let inputs = vec!["5", "5;"];

    for input in inputs {
        let mut program = make_program_from(input, Some(1));
        match program.statements.pop().unwrap() {
            Statement::Expression(Expression::Integer(integer)) => assert_eq!(5, integer),
            stmt => panic!("statement is no an int expression but {stmt:?}"),
        }
    }
}

#[test]
fn test_parse_prefix_expression() {
    // tuple of (sample, operator, operand)
    let code_samples_n_results = vec![
        ("!5", Token::Bang, Expectation::Int(5)),
        ("-15", Token::Minus, Expectation::Int(15)),
        ("!true", Token::Bang, Expectation::Bool(true)),
        ("!false", Token::Bang, Expectation::Bool(false)),
    ];

    for (input, op, operand) in code_samples_n_results {
        let mut program = make_program_from(input, Some(1));

        match program.statements.pop().unwrap() {
            Statement::Expression(Expression::Prefix(expr)) => {
                assert_eq!(op, expr.operator);
                assert_literal_expr(*expr.operand, operand);
            }
            stmt => panic!("not a statement expression: {stmt:?}"),
        }
    }
}

#[test]
fn test_parse_infix_expression_int() {
    // tuple of (sample, left_operand, operator, right_operand)
    let int_expr_expectations = vec![
        (
            "5 + 5",
            Expectation::Int(5),
            Token::Plus,
            Expectation::Int(5),
        ),
        (
            "5 - 5",
            Expectation::Int(5),
            Token::Minus,
            Expectation::Int(5),
        ),
        (
            "5 * 5",
            Expectation::Int(5),
            Token::Asterisk,
            Expectation::Int(5),
        ),
        (
            "5 / 5",
            Expectation::Int(5),
            Token::Slash,
            Expectation::Int(5),
        ),
        ("5 > 5", Expectation::Int(5), Token::Gt, Expectation::Int(5)),
        ("5 < 5", Expectation::Int(5), Token::Lt, Expectation::Int(5)),
        (
            "5 == 5",
            Expectation::Int(5),
            Token::Eq,
            Expectation::Int(5),
        ),
        (
            "5 != 5",
            Expectation::Int(5),
            Token::NotEq,
            Expectation::Int(5),
        ),
        (
            "true == true",
            Expectation::Bool(true),
            Token::Eq,
            Expectation::Bool(true),
        ),
        (
            "true != false",
            Expectation::Bool(true),
            Token::NotEq,
            Expectation::Bool(false),
        ),
        (
            "false == false",
            Expectation::Bool(false),
            Token::Eq,
            Expectation::Bool(false),
        ),
    ];

    for (input, left_op, operator, right_op) in int_expr_expectations {
        let mut program = make_program_from(input, Some(1));

        match program.statements.pop().unwrap() {
            Statement::Expression(Expression::Infix(expr)) => {
                assert_infix_expression(expr, left_op, operator, right_op)
            }
            stmt => panic!("not an infix statement expression: {stmt:?}"),
        }
    }
}

#[test]
fn test_operator_precedence() {
    let code_samples_n_results = vec![
        ("-a * b", "((-a) * b)", 1),
        ("!-a", "(!(-a))", 1),
        ("a + b + c", "((a + b) + c)", 1),
        ("a + b - c", "((a + b) - c)", 1),
        ("a * b * c", "((a * b) * c)", 1),
        ("a * b / c", "((a * b) / c)", 1),
        ("a + b / c", "(a + (b / c))", 1),
        (
            "a + b * c + d / e - f",
            "(((a + (b * c)) + (d / e)) - f)",
            1,
        ),
        ("3 + 4; -5 * 5", "(3 + 4); ((-5) * 5)", 2),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))", 1),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))", 1),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            1,
        ),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            1,
        ),
        ("true", "true", 1),
        ("false", "false", 1),
        ("3 > 5 == false", "((3 > 5) == false)", 1),
        ("3 < 5 == true", "((3 < 5) == true)", 1),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)", 1),
        ("(5 + 5) * 2", "((5 + 5) * 2)", 1),
        ("2 / (5 + 5)", "(2 / (5 + 5))", 1),
        ("-(5 + 5)", "(-(5 + 5))", 1),
        ("!(true == true)", "(!(true == true))", 1),
        ("a + add(b * c) + d", "((a + add((b * c))) + d)", 1),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            1,
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
            1,
        ),
        // TODO: vectors and idexes are not supported yet
        // (
        //     "a * [1, 2, 3, 4][b * c] * d",
        //     "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        // ),
        // (
        //     "add(a * b[2], b[1], 2 * [1, 2][1])",
        //     "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        // ),
    ];

    for (input, expectation, statements_count) in code_samples_n_results {
        let parsed_program = make_program_from(input, Some(statements_count));
        let expected_program = make_program_from(expectation, Some(statements_count));
        assert_eq!(expected_program, parsed_program);
    }
}

#[test]
fn test_boolean_expression() {
    let input = r#"
        true; false;
        false
        "#;

    let expected_values = vec![true, false, false];

    let program = make_program_from(input, Some(3));

    for (expected, stmt) in expected_values.into_iter().zip(program.statements) {
        match stmt {
            Statement::Expression(Expression::Boolean(bool)) => assert_eq!(expected, bool),
            stmt => panic!("expected a boolean expression, got {stmt:?}"),
        }
    }
}

#[test]
fn test_if_expression() {
    let input = r#"
    if (x < y) { x }
    "#;

    let mut program = make_program_from(input, Some(1));

    match program.statements.pop().unwrap() {
        Statement::Expression(Expression::If(mut if_expr)) => {
            if let Expression::Infix(expr) = *if_expr.condition {
                assert_infix_expression(
                    expr,
                    Expectation::String("x".to_string()),
                    Token::Lt,
                    Expectation::String("y".to_string()),
                );
            }
            assert_eq!(1, if_expr.consequence.statements.len());
            match if_expr.consequence.statements.pop().unwrap() {
                Statement::Expression(cons) => {
                    assert_literal_expr(cons, Expectation::String("x".to_string()))
                }
                some => panic!("consequence is not an expression, but {some:?}"),
            }

            assert!(if_expr.alternative.is_none());
        }
        etc => panic!("not an if expression in the statement but {etc:?}"),
    }
}

#[test]
fn test_if_else_expression() {
    let input = r#"
    if (x < y) { x } else { y }
    "#;

    let mut program = make_program_from(input, Some(1));

    match program.statements.pop().unwrap() {
        Statement::Expression(Expression::If(mut if_expr)) => {
            if let Expression::Infix(expr) = *if_expr.condition {
                assert_infix_expression(
                    expr,
                    Expectation::String("x".to_string()),
                    Token::Lt,
                    Expectation::String("y".to_string()),
                );
            }
            assert_eq!(1, if_expr.consequence.statements.len());
            match if_expr.consequence.statements.pop().unwrap() {
                Statement::Expression(cons) => {
                    assert_literal_expr(cons, Expectation::String("x".to_string()))
                }
                some => panic!("consequence is not an expression, but {some:?}"),
            }

            match if_expr.alternative.unwrap().statements.pop().unwrap() {
                Statement::Expression(alt) => {
                    assert_literal_expr(alt, Expectation::String("y".to_string()))
                }
                some => panic!("consequence is not an expression, but {some:?}"),
            }
        }
        some => panic!("not an if expression but {some:?}"),
    }
}

#[test]
fn test_function_expression() {
    let input = "fn(x, y) { x + y }";
    let mut program = make_program_from(input, Some(1));

    match program.statements.pop().unwrap() {
        Statement::Expression(Expression::Fn(mut fn_expr)) => {
            assert_eq!(2, fn_expr.parameters.len());
            assert_eq!(
                vec!["x", "y"],
                fn_expr
                    .parameters
                    .iter()
                    .map(|p| p.as_str())
                    .collect::<Vec<&str>>()
            );
            assert_eq!(1, fn_expr.body.statements.len());

            match fn_expr.body.statements.pop().unwrap() {
                Statement::Expression(Expression::Infix(expr)) => assert_infix_expression(
                    expr,
                    Expectation::String("x".to_string()),
                    Token::Plus,
                    Expectation::String("y".to_string()),
                ),
                some => panic!("not an infix expression but {some:?}"),
            }
        }
        some => panic!("not a fn expression but {some:?}"),
    }
}

#[test]
fn test_fn_params_parsing() {
    let test_samples = vec![
        ("fn() {};", vec![]),
        ("fn(x) {};", vec!["x"]),
        ("fn(x,y, z) { x + 5 };", vec!["x", "y", "z"]),
    ];

    for (input, expectation) in test_samples {
        let program = make_program_from(input, Some(1));

        match &program.statements[0] {
            Statement::Expression(Expression::Fn(fn_expr)) => {
                assert_eq!(
                    expectation,
                    fn_expr
                        .parameters
                        .iter()
                        .map(|p| p.as_str())
                        .collect::<Vec<&str>>()
                );
            }
            some => panic!("not a fn expression but {some:?}"),
        }
    }
}

#[test]
fn test_call_fn() {
    let input = "add(1, 2 * 3, 4 + 5)";

    let mut program = make_program_from(input, Some(1));

    match program.statements.pop().unwrap() {
        Statement::Expression(Expression::Call(mut call_expr)) => {
            assert_literal_expr(*call_expr.function, Expectation::String("add".to_string()));
            if let Expression::Infix(expr) = call_expr.arguments.pop().unwrap() {
                assert_infix_expression(
                    expr,
                    Expectation::Int(4),
                    Token::Plus,
                    Expectation::Int(5),
                );
            }
            if let Expression::Infix(expr) = call_expr.arguments.pop().unwrap() {
                assert_infix_expression(
                    expr,
                    Expectation::Int(2),
                    Token::Asterisk,
                    Expectation::Int(3),
                );
            }
            assert_literal_expr(call_expr.arguments.pop().unwrap(), Expectation::Int(1));
        }
        some => panic!("not an expression but {some:?}"),
    }
}

#[derive(Debug)]
enum Expectation {
    String(String),
    Bool(bool),
    Int(i64),
}

fn assert_literal_expr(expr: Expression, expected: Expectation) {
    match (expr, expected) {
        (Expression::Boolean(data), Expectation::Bool(val)) => assert_eq!(val, data),
        (Expression::Integer(data), Expectation::Int(val)) => assert_eq!(val, data),
        (Expression::Identifier(data), Expectation::String(val)) => assert_eq!(val, data),
        (expr, expected) => panic!("cannot compare {expr:?} and {expected:?}"),
    }
}

fn assert_infix_expression(
    expr: expression::Infix,
    left: Expectation,
    operator: Token,
    right: Expectation,
) {
    assert_literal_expr(*expr.left, left);
    assert_eq!(operator, expr.operator);
    assert_literal_expr(*expr.right, right);
}

fn make_program_from(input: &str, statements_count: Option<usize>) -> Program {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    assert!(
        parser.error.is_empty(),
        "expected no parsing errors, got {:?}",
        parser.error
    );
    assert!(program.is_ok());
    if let Some(statements_count) = statements_count {
        assert_eq!(statements_count, program.as_ref().unwrap().statements.len());
    }
    program.unwrap()
}
