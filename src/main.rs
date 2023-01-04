use std::io::Write;

use subulisp_rs::Lisp;

fn main() {
    let mut env = Lisp::new();
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
