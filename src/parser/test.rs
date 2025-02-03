use crate::ast::Node;
use crate::ast::Statement;

use super::*;

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
            assert_eq!("x", data.name.value);
            assert_eq!("x", data.name.token_literal());
            match &data.value {
                Some(Expression::Integer(int_expr)) => assert_eq!(5, int_expr.value),
                _ => panic!("value is not an integer but {:?}", data.value),
            }
        }
        some => panic!("not a let statement but {some:?}"),
    }

    match &program.statements[1] {
        Statement::Let(data) => {
            assert_eq!("y", data.name.value);
            assert_eq!("y", data.name.token_literal());
            match &data.value {
                Some(Expression::Boolean(bool_expr)) => assert_eq!(true, bool_expr.value),
                _ => panic!("value is not a bool but {:?}", data.value),
            }
        }
        some => panic!("not a let statement but {some:?}"),
    }

    match &program.statements[2] {
        Statement::Let(data) => {
            assert_eq!("foo_bar", data.name.value);
            assert_eq!("foo_bar", data.name.token_literal());
            match &data.value {
                Some(Expression::Identifier(idf_expr)) => assert_eq!("y", idf_expr.value),
                _ => panic!("value is not an identifier but {:?}", data.value),
            }
        }
        some => panic!("not a let statement but {some:?}"),
    }
}

#[test]
fn test_let_statement_errors() {
    let input = r#"
        let x = 5;
        let y > 10;
        let foo_bar baz = 83838;
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    parser.parse_program();

    assert_eq!(
        vec![
            "expected next token to be Assign, got Gt",
            "no prefix parsing fn for token kind Gt",
            "expected next token to be Assign, got Identifier",
            "no prefix parsing fn for token kind Assign"
        ],
        parser.errors
    );
}

#[test]
fn test_return_statement() {
    let input = r#"
            return 1;
            return 10;
            return x + t;
            "#;

    let program = make_program_from(input, Some(3));

    match &program.statements[0] {
        Statement::Return(stmt) => {
            assert_eq!("return", stmt.token_literal());
            assert!(stmt.ret_val.is_some());
            stmt.ret_val.as_ref().unwrap().assert_literal_expr(1);
        }
        some => panic!("not a return statement but {some:?}"),
    }

    match &program.statements[1] {
        Statement::Return(stmt) => {
            assert_eq!("return", stmt.token_literal());
            assert!(stmt.ret_val.is_some());
            stmt.ret_val.as_ref().unwrap().assert_literal_expr(10);
        }
        some => panic!("not a return statement but {some:?}"),
    }

    match &program.statements[2] {
        Statement::Return(stmt) => {
            assert_eq!("return", stmt.token_literal());
            assert!(stmt.ret_val.is_some());
            if let Expression::Infix(expr) = stmt.ret_val.as_ref().unwrap() {
                expr.assert_infix_expression("x".to_string(), "+", "t".to_string());
            }
        }
        some => panic!("not a return statement but {some:?}"),
    }
}

#[test]
fn test_indentifier_expression() {
    let input = "foobar";

    let program = make_program_from(input, Some(1));

    match &program.statements[0] {
        Statement::Expression(expr) => {
            assert!(expr.expression.is_some());
            match expr.expression.as_ref().unwrap() {
                Expression::Identifier(idf) => {
                    assert_eq!("foobar", idf.value);
                    assert_eq!("foobar", idf.token_literal());
                }
                _ => panic!("unexpected expression type: {:?}", expr),
            }
        }

        expr => panic!("unexpected statement type: {expr:?}"),
    }
}

#[test]
fn test_literal_expression() {
    let input = "5;";

    let program = make_program_from(input, Some(1));
    match &program.statements[0] {
        Statement::Expression(expr) => {
            assert!(expr.expression.is_some());
            match expr.expression.as_ref().unwrap() {
                Expression::Integer(int_expr) => {
                    assert_eq!(5, int_expr.value);
                    assert_eq!("5", int_expr.token.literal);
                }
                expr => panic!("expression not an integer literal but {expr:?}"),
            }
        }
        stmt => panic!("statement is no an expression but {stmt:?}"),
    }
}

#[test]
fn test_string_expression() {
    let input = r#"
        "hello world"
    "#;

    let mut program = make_program_from(input, Some(1));
    match program.statements.pop().unwrap() {
        Statement::Expression(ExpressionStatement {
            expression: Some(Expression::StringExp(string_literal)),
            ..
        }) => assert_eq!("hello world", string_literal.value),
        etc => panic!("not a string expression statement, but {etc:?}"),
    }
}

#[test]
fn test_parse_prefix_expression_int() {
    // tuple of (sample, operator, operand)
    let code_samples_n_results = vec![("!5", "!", 5), ("-15", "-", 15)];

    for (input, op, operand) in code_samples_n_results {
        let program = make_program_from(input, Some(1));

        match &program.statements[0] {
            Statement::Expression(expr) => {
                assert!(expr.expression.is_some());
                match expr.expression.as_ref().unwrap() {
                    Expression::Prefix(expr) => {
                        assert_eq!(op, expr.operator);
                        expr.operand.assert_literal_expr(operand);
                    }
                    prefix_expr => panic!("not a prefix expression: {prefix_expr:?}"),
                }
            }
            stmt => panic!("not a statement expression: {stmt:?}"),
        }
    }
}

#[test]
fn test_parse_prefix_expression_bool() {
    // tuple of (sample, operator, operand)
    let code_samples_n_results = vec![("!true", "!", true), ("!false", "!", false)];

    for (input, op, operand) in code_samples_n_results {
        let program = make_program_from(input, Some(1));

        match &program.statements[0] {
            Statement::Expression(expr) => {
                assert!(expr.expression.is_some());
                match expr.expression.as_ref().unwrap() {
                    Expression::Prefix(expr) => {
                        assert_eq!(op, expr.operator);
                        expr.operand.assert_literal_expr(operand);
                    }
                    prefix_expr => panic!("not a prefix expression: {prefix_expr:?}"),
                }
            }
            stmt => panic!("not a statement expression: {stmt:?}"),
        }
    }
}

#[test]
fn test_parse_infix_expression_int() {
    // tuple of (sample, left_operand, operator, right_operand)
    let int_expr_expectations = vec![
        ("5 + 5", 5, "+", 5),
        ("5 - 5", 5, "-", 5),
        ("5 * 5", 5, "*", 5),
        ("5 / 5", 5, "/", 5),
        ("5 > 5", 5, ">", 5),
        ("5 < 5", 5, "<", 5),
        ("5 == 5", 5, "==", 5),
        ("5 != 5", 5, "!=", 5),
    ];

    for (input, left_op, operator, right_op) in int_expr_expectations {
        let program = make_program_from(input, Some(1));

        match &program.statements[0] {
            Statement::Expression(expr) => {
                assert!(expr.expression.is_some());
                if let Expression::Infix(expr) = expr.expression.as_ref().unwrap() {
                    expr.assert_infix_expression(left_op, operator, right_op);
                }
            }
            stmt => panic!("not a statement expression: {stmt:?}"),
        }
    }
}

#[test]
fn test_parse_infix_expression_bool() {
    // tuple of (sample, left_operand, operator, right_operand)
    let bool_expr_expectations = vec![
        ("true == true", true, "==", true),
        ("true != false", true, "!=", false),
        ("false == false", false, "==", false),
    ];

    for (input, left_op, operator, right_op) in bool_expr_expectations {
        let program = make_program_from(input, Some(1));

        match &program.statements[0] {
            Statement::Expression(expr) => {
                assert!(expr.expression.is_some());
                if let Expression::Infix(expr) = expr.expression.as_ref().unwrap() {
                    expr.assert_infix_expression(left_op, operator, right_op);
                }
            }
            stmt => panic!("not a statement expression: {stmt:?}"),
        }
    }
}

#[test]
fn test_operator_precedence() {
    let code_samples_n_results = vec![
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        ),
        // TODO: figure the issue
        // (
        //     "a * [1, 2, 3, 4][b * c] * d",
        //     "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        // ),
        // (
        //     "add(a * b[2], b[1], 2 * [1, 2][1])",
        //     "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        // ),
    ];

    for (input, expectation) in code_samples_n_results {
        let program = make_program_from(input, None);
        assert_eq!(expectation, program.print());
    }
}

#[test]
fn test_boolean_expression() {
    let input = r#"
        true; false;
        false
        "#;

    let expected_values = vec![
        (TokenKind::True, "true"),
        (TokenKind::False, "false"),
        (TokenKind::False, "false"),
    ];

    let program = make_program_from(input, Some(3));

    for ((expected_token, expected_literal), stmt) in
        expected_values.into_iter().zip(program.statements)
    {
        match stmt {
            Statement::Expression(expr) => {
                assert!(expr.expression.is_some());
                match expr.expression.as_ref().unwrap() {
                    Expression::Boolean(expr) => {
                        assert_eq!(expected_token, expr.token.kind);
                        assert_eq!(expected_literal, expr.token_literal());
                    }
                    expr => panic!("expected boolean expression, got {expr:?}"),
                }
            }
            stmt => panic!("expected expression, got {stmt:?}"),
        }
    }
}

#[test]
fn test_if_expression() {
    let input = r#"
    if (x < y) { x }
    "#;

    let program = make_program_from(input, Some(1));

    match &program.statements[0] {
        Statement::Expression(expr) => {
            assert!(expr.expression.is_some());
            match expr.expression.as_ref().unwrap() {
                Expression::If(if_expr) => {
                    if let Expression::Infix(expr) = if_expr.condition.as_ref() {
                        expr.assert_infix_expression("x".to_string(), "<", "y".to_string());
                    }
                    assert_eq!(1, if_expr.consequence.statements.len());
                    match &if_expr.consequence.statements[0] {
                        Statement::Expression(cons) => cons
                            .expression
                            .as_ref()
                            .unwrap()
                            .assert_literal_expr("x".to_string()),
                        some => panic!("consequence is not an expression, but {some:?}"),
                    }

                    assert!(if_expr.alternative.is_none());
                }
                some => panic!("not an if expression but {some:?}"),
            }
        }
        some => panic!("not an expression but {some:?}"),
    }
}

#[test]
fn test_if_else_expression() {
    let input = r#"
    if (x < y) { x } else { y }
    "#;

    let program = make_program_from(input, Some(1));

    match &program.statements[0] {
        Statement::Expression(expr) => {
            assert!(expr.expression.is_some());
            match expr.expression.as_ref().unwrap() {
                Expression::If(if_expr) => {
                    if let Expression::Infix(expr) = if_expr.condition.as_ref() {
                        expr.assert_infix_expression("x".to_string(), "<", "y".to_string());
                    }
                    assert_eq!(1, if_expr.consequence.statements.len());
                    match &if_expr.consequence.statements[0] {
                        Statement::Expression(cons) => cons
                            .expression
                            .as_ref()
                            .unwrap()
                            .assert_literal_expr("x".to_string()),
                        some => panic!("consequence is not an expression, but {some:?}"),
                    }

                    assert_eq!(1, if_expr.alternative.as_ref().unwrap().statements.len());
                    match &if_expr.alternative.as_ref().unwrap().statements[0] {
                        Statement::Expression(alt) => alt
                            .expression
                            .as_ref()
                            .unwrap()
                            .assert_literal_expr("y".to_string()),
                        some => panic!("consequence is not an expression, but {some:?}"),
                    }
                }
                some => panic!("not an if expression but {some:?}"),
            }
        }
        some => panic!("not an expression but {some:?}"),
    }
}

#[test]
fn test_function_expression() {
    let input = "fn(x, y) { x + y }";
    let program = make_program_from(input, Some(1));

    match &program.statements[0] {
        Statement::Expression(expr) => {
            assert!(expr.expression.is_some());
            match expr.expression.as_ref().unwrap() {
                Expression::Fn(fn_expr) => {
                    assert_eq!(2, fn_expr.parameters.len());
                    assert_eq!(
                        vec!["x", "y"],
                        fn_expr
                            .parameters
                            .iter()
                            .map(|p| p.value.as_str())
                            .collect::<Vec<&str>>()
                    );
                    assert_eq!(1, fn_expr.body.statements.len());

                    match &fn_expr.body.statements[0] {
                        Statement::Expression(stmt) => {
                            if let Expression::Infix(expr) = stmt.expression.as_ref().unwrap() {
                                expr.assert_infix_expression("x".to_string(), "+", "y".to_string())
                            }
                        }
                        some => panic!("not an expression but {some:?}"),
                    }
                }
                some => panic!("not a fn expression but {some:?}"),
            }
        }
        some => panic!("not an expression but {some:?}"),
    }
}

#[test]
fn test_fn_params_parsing() {
    let test_samples = vec![
        ("fn() {};", vec![]),
        ("fn(x) {};", vec!["x"]),
        ("fn(x,y, z) {};", vec!["x", "y", "z"]),
    ];

    for (input, expectation) in test_samples {
        let program = make_program_from(input, Some(1));

        match &program.statements[0] {
            Statement::Expression(expr) => {
                assert!(expr.expression.is_some());
                match expr.expression.as_ref().unwrap() {
                    Expression::Fn(fn_expr) => {
                        assert_eq!(
                            expectation,
                            fn_expr
                                .parameters
                                .iter()
                                .map(|p| p.value.as_str())
                                .collect::<Vec<&str>>()
                        );
                    }
                    some => panic!("not a fn expression but {some:?}"),
                }
            }
            some => panic!("not an expression but {some:?}"),
        }
    }
}

#[test]
fn test_call_fn() {
    let input = "add(1, 2 * 3, 4 + 5)";

    let program = make_program_from(input, Some(1));

    match &program.statements[0] {
        Statement::Expression(expr) => {
            assert!(expr.expression.is_some());
            match expr.expression.as_ref().unwrap() {
                Expression::Call(call_expr) => {
                    call_expr.function.assert_literal_expr("add".to_string());
                    if let Expression::Infix(expr) = &call_expr.arguments[0] {
                        expr.assert_literal_expr(1);
                    }
                    if let Expression::Infix(expr) = &call_expr.arguments[1] {
                        expr.assert_infix_expression(2, "*", 3);
                    }
                    if let Expression::Infix(expr) = &call_expr.arguments[2] {
                        expr.assert_infix_expression(4, "+", 5);
                    }
                }
                some => panic!("not a call expression but {some:?}"),
            }
        }
        some => panic!("not an expression but {some:?}"),
    }
}

trait Assertable<Y> {
    fn assert_literal_expr(&self, _expected: Y) {}
    fn assert_infix_expression(&self, _left: Y, _operator: &str, _right: Y) {
        panic!("not implemented");
    }
}

impl Assertable<i64> for Expression {
    fn assert_literal_expr(&self, expected: i64) {
        match self {
            Expression::Integer(data) => {
                assert_eq!(expected, data.value);
                assert_eq!(expected.to_string(), data.token.literal);
            }
            expr => panic!("not an integer expression but {expr:?}"),
        }
    }
}

impl Assertable<String> for Expression {
    fn assert_literal_expr(&self, expected: String) {
        match self {
            Expression::Identifier(data) => {
                assert_eq!(expected, data.value);
                assert_eq!(expected, data.token_literal());
            }
            expr => panic!("expression is not identifier but {expr:?}"),
        }
    }
}

impl Assertable<bool> for Expression {
    fn assert_literal_expr(&self, expected: bool) {
        match self {
            Expression::Boolean(data) => {
                assert_eq!(expected, data.value);
                assert_eq!(expected.to_string(), data.token_literal())
            }
            expr => panic!("expected boolean expr, got {:?}", expr),
        }
    }
}

impl<Y> Assertable<Y> for InfixExpression
where
    Expression: Assertable<Y>,
{
    fn assert_infix_expression(&self, left: Y, operator: &str, right: Y) {
        self.left.assert_literal_expr(left);
        assert_eq!(operator, self.operator);
        self.right.assert_literal_expr(right);
        assert_eq!(operator, self.operator);
    }
}

fn make_program_from(input: &str, statements_count: Option<usize>) -> Program {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    assert!(
        parser.errors.is_empty(),
        "expected no parsing errors, got {:?}",
        parser.errors
    );
    assert!(program.is_some());
    if let Some(statements_count) = statements_count {
        assert_eq!(statements_count, program.as_ref().unwrap().statements.len());
    }
    program.unwrap()
}
