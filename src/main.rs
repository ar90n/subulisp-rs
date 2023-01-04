use subulisp_rs::Lisp;

fn main() {
    let mut env = Lisp::new();
    let result = env.evaluate("(+ 1 2)".to_string()).unwrap();
    println!("{}", result);
}
