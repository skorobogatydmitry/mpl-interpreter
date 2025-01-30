use std::io;

use mpl_interpreter::repl;

fn main() {
    println!("That's a Monkey Programming Language (MPL) interpreter.");
    repl::start(io::stdin(), io::stdout());
}
