use std::io;

use mpl_interpreter::alt::repl;

fn main() {
    // TODO: invoke alt with flag
    println!("That's a Monkey Programming Language (MPL) interpreter.");
    repl::start(io::stdin(), io::stdout());
}
