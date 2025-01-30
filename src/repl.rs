use std::io::{Stdin, Stdout, Write};

use crate::{evaluator::Evaluator, lexer::Lexer, parser::Parser};

static PROMPT: &str = ">> ";

pub fn start(stdin: Stdin, mut stdout: Stdout) {
    let mut evaluator = Evaluator::new();
    loop {
        write!(stdout, "{}", PROMPT).expect("unable to write to stdout");
        stdout.flush().expect("unable to flush stdout");
        let mut input = String::new();

        if let Err(e) = stdin.read_line(&mut input) {
            writeln!(stdout, "Cannot read from stdout: {e}")
                .expect("unable to write an error to stdout");
            return;
        }

        let lexer = Lexer::new(&input);

        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("unable to parse program");

        if parser.errors.len() != 0 {
            show_parse_errors(&mut stdout, parser.errors);
        }

        let program_result = evaluator.eval_program(program);

        writeln!(stdout, "{program_result}").expect("unable to write program result to stdout");
    }

    fn show_parse_errors(stdout: &mut Stdout, errors: Vec<String>) {
        writeln!(stdout, " _____________________________")
            .expect("unable to print errors to stdout");
        for err in errors {
            writeln!(stdout, "< {}", err).expect("unable to print errors to stdout");
        }
        writeln!(
            stdout,
            " ------------------------------
        \\   ^__^
         \\  (==)\\_______
            (__)\\       )\\/\\
                ||----w |
                ||     ||"
        )
        .expect("unable to print errors to stdout");
    }
}
