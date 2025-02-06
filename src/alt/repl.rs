use std::io::{Stdin, Stdout, Write};

use super::{evaluator::Evaluator, lexer::Lexer, parser::Parser};

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
        match parser.parse_program() {
            Ok(program) => match evaluator.eval_program(program) {
                Ok(program_result) => {
                    writeln!(stdout, "{program_result}")
                        .expect("unable to write program result to stdout");
                }
                Err(error) => show_parse_error(&mut stdout, error),
            },
            Err(error) => show_parse_error(&mut stdout, error),
        }
    }

    fn show_parse_error(stdout: &mut Stdout, error: String) {
        writeln!(
            stdout,
            "⠀{}",
            (0..error.len() + 10)
                .map(|_| "⣀")
                .collect::<Vec<&str>>()
                .join("")
        )
        .expect("unable to print errors to stdout");
        writeln!(stdout, "⣞⠀UwU⠀{}⠀UwU⠀⡇", error).expect("unable to print errors to stdout");

        writeln!(
            stdout,
            "⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⠟{}
⠙⠦⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⠶⠋⠀⠀⠀⢰⠒⢦⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡤⢄⠀⠀⠀⠀⠀
⠀⠀⠀⠉⠛⠲⠤⢤⣀⠀⠀⠀⠀⠀⢶⠶⠛⠉⠀⠀⠀⠀⠀⠀⣼⠀⠈⢣⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡴⠋⠁⠀⢳⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠈⠙⠳⢦⣄⡀⠈⠳⣄⠀⠀⠀⠀⠀⠀⠀⣇⠀⠀⠀⢱⣀⣀⣀⣀⣀⣀⠀⠀⢀⡴⠋⠀⠀⠀⠀⢸⡆⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠙⠓⢦⣝⣦⡀⠀⠀⠀⠀⣰⠟⠀⠀⠀⠉⠀⠀⠀⠀⠀⠉⠉⠙⠋⠀⠀⠀⠀⠀⠀⠀⣿⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠙⣓⠀⢀⡤⠚⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢤⣇⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣴⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠙⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡞⠁⠀⣰⢻⣿⡷⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢧⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣸⠁⠀⠀⠉⣙⡿⠃⠀⠀⠀⠀⢠⣞⣻⣿⣷⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢺⡆⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⡇⠀⠀⣀⡐⠋⠀⠀⠀⠀⠀⠀⠈⠛⠿⠿⠋⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⡇⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠇⠀⢾⢿⣿⡿⣶⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢇⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡏⠀⠀⠘⢿⣿⣷⠮⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⡄⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠀⠀⠠⣧⣇⡈⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢹⠀⠀⠀⠈⠙⠛⠛⠒⠒⠤⠤⠞⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢱⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⡄",
            (0..error.len() - 12)
                .map(|_| "⠉")
                .collect::<Vec<&str>>()
                .join("")
        )
        .expect("unable to print errors to stdout");
    }
}
