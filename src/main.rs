use std::io::{Read, Write};

use atty::Stream;

use subulisp_rs::Lisp;

fn repl(mut env: Lisp) {
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        if !input.trim().is_empty() {
            let result = env.evaluate(input).unwrap();
            println!("{}", result);
        }
    }
}

fn batch(mut env: Lisp) {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    let input = format!("({})", input.replace('\n', ""));
    let result = env.evaluate(input).unwrap();
    println!("{}", result);
}

fn main() {
    let env = Lisp::new();
    if atty::is(Stream::Stdin) {
        repl(env)
    } else {
        batch(env)
    }
}
