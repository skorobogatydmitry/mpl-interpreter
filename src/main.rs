use std::env::args;
use std::io;

use mpl_interpreter::alt::repl as alt_repl;
use mpl_interpreter::repl;

fn main() {
    let args = args();
    match args.len() {
        0 | 1 => {
            println!("That's a Monkey Programming Language (MPL) interpreter.");
            repl::start(io::stdin(), io::stdout());
        }
        2 => match args.last().unwrap().as_str() {
            "--alt" => {
                println!("Alternative Monkey Programming Language (MPL) interpreter");
                println!("   ... don't hesitate");
                alt_repl::start(io::stdin(), io::stdout());
            }
            other => panic!("only --alt is supported, got {other}"),
        },
        n => panic!("expected 1 argument at most, got {n}"),
    }
}
