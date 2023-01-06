use super::expr::Expr;
use once_cell::sync::Lazy;
use std::collections::HashMap;

use rand::distributions::Alphanumeric;
use rand::{thread_rng, Rng};

#[derive(Debug)]
pub(crate) struct Func {
    pub args: Vec<String>,
    pub body: Expr,
}

impl Func {
    pub fn new(args: Vec<String>, body: Expr) -> Self {
        Self { args, body }
    }

    pub fn remap_arg_symbol(&self) -> (Vec<String>, Expr) {
        fn remap_body(e: &Expr, args: &Vec<String>, suffix: &str) -> Expr {
            match e {
                Expr::Symbol(s) if args.contains(s) => Expr::Symbol(format!("{}_{}", s, suffix)),
                Expr::List(es) => {
                    Expr::List(es.iter().map(|e| remap_body(e, args, suffix)).collect())
                }
                _ => e.clone(),
            }
        }

        let suffix = create_suffix();
        let args = self
            .args
            .iter()
            .map(|s| format!("{}_{}", s, suffix))
            .collect();
        let body = remap_body(&self.body, &self.args, &suffix);
        (args, body)
    }
}

#[derive(Debug)]
pub(crate) struct Context {
    env: HashMap<String, Func>,
    parent: Option<Box<Context>>,
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
            parent: None,
        }
    }

    pub fn resolve(&self, name: &str) -> Option<&Func> {
        self.env.get(name).or_else(|| match self.parent {
            Some(ref p) => p.resolve(name),
            None => ROOT_ENV.get(name),
        })
    }

    pub fn register(&mut self, name: &str, func: Func) {
        self.env.insert(name.to_string(), func);
    }

    pub fn with(self, env: HashMap<String, Func>) -> Context {
        Self {
            env,
            parent: Some(Box::new(self)),
        }
    }
}

fn create_suffix() -> String {
    let mut rng = thread_rng();
    (0..10)
        .map(|_| rng.sample(Alphanumeric) as char)
        .collect::<String>()
}

static ROOT_ENV: Lazy<HashMap<String, Func>> = Lazy::new(|| {
    [
        (
            "|".to_string(),
            Func {
                args: vec!["lhs".to_string(), "rhs".to_string()],
                body: Expr::List(vec![
                    Expr::Symbol("!".to_string()),
                    Expr::List(vec![
                        Expr::Symbol("&".to_string()),
                        Expr::List(vec![
                            Expr::Symbol("!".to_string()),
                            Expr::List(vec![Expr::Symbol("lhs".to_string())]),
                        ]),
                        Expr::List(vec![
                            Expr::Symbol("!".to_string()),
                            Expr::List(vec![Expr::Symbol("rhs".to_string())]),
                        ]),
                    ]),
                ]),
            },
        ),
        (
            "!=".to_string(),
            Func {
                args: vec!["lhs".to_string(), "rhs".to_string()],
                body: Expr::List(vec![
                    Expr::Symbol("!".to_string()),
                    Expr::List(vec![
                        Expr::Symbol("=".to_string()),
                        Expr::List(vec![Expr::Symbol("lhs".to_string())]),
                        Expr::List(vec![Expr::Symbol("rhs".to_string())]),
                    ]),
                ]),
            },
        ),
        (
            "<=".to_string(),
            Func {
                args: vec!["lhs".to_string(), "rhs".to_string()],
                body: Expr::List(vec![
                    Expr::Symbol("|".to_string()),
                    Expr::List(vec![
                        Expr::Symbol("<".to_string()),
                        Expr::List(vec![Expr::Symbol("lhs".to_string())]),
                        Expr::List(vec![Expr::Symbol("rhs".to_string())]),
                    ]),
                    Expr::List(vec![
                        Expr::Symbol("=".to_string()),
                        Expr::List(vec![Expr::Symbol("lhs".to_string())]),
                        Expr::List(vec![Expr::Symbol("rhs".to_string())]),
                    ]),
                ]),
            },
        ),
        (
            ">".to_string(),
            Func {
                args: vec!["lhs".to_string(), "rhs".to_string()],
                body: Expr::List(vec![
                    Expr::Symbol("!".to_string()),
                    Expr::List(vec![
                        Expr::Symbol("<=".to_string()),
                        Expr::List(vec![Expr::Symbol("lhs".to_string())]),
                        Expr::List(vec![Expr::Symbol("rhs".to_string())]),
                    ]),
                ]),
            },
        ),
        (
            ">=".to_string(),
            Func {
                args: vec!["lhs".to_string(), "rhs".to_string()],
                body: Expr::List(vec![
                    Expr::Symbol("!".to_string()),
                    Expr::List(vec![
                        Expr::Symbol("<".to_string()),
                        Expr::List(vec![Expr::Symbol("lhs".to_string())]),
                        Expr::List(vec![Expr::Symbol("rhs".to_string())]),
                    ]),
                ]),
            },
        ),
        (
            "incf".to_string(),
            Func {
                args: vec!["v".to_string()],
                body: Expr::List(vec![
                    Expr::Symbol("+".to_string()),
                    Expr::List(vec![Expr::Symbol("v".to_string())]),
                    Expr::Number(1.0),
                ]),
            },
        ),
        (
            "decf".to_string(),
            Func {
                args: vec!["v".to_string()],
                body: Expr::List(vec![
                    Expr::Symbol("-".to_string()),
                    Expr::List(vec![Expr::Symbol("v".to_string())]),
                    Expr::Number(1.0),
                ]),
            },
        ),
        (
            "fold".to_string(),
            Func {
                args: vec!["f".to_string(), "init".to_string(), "args".to_string()],
                body: Expr::List(vec![
                    Expr::Symbol("if".to_string()),
                    Expr::List(vec![
                        Expr::Symbol("=".to_string()),
                        Expr::List(vec![Expr::Symbol("args".to_string())]),
                        Expr::List(vec![]),
                    ]),
                    Expr::List(vec![Expr::Symbol("init".to_string())]),
                    Expr::List(vec![
                        Expr::Symbol("fold".to_string()),
                        Expr::List(vec![Expr::Symbol("f".to_string())]),
                        Expr::List(vec![
                            Expr::List(vec![Expr::Symbol("f".to_string())]),
                            Expr::List(vec![Expr::Symbol("init".to_string())]),
                            Expr::List(vec![
                                Expr::Symbol("car".to_string()),
                                Expr::List(vec![Expr::Symbol("args".to_string())]),
                            ]),
                        ]),
                        Expr::List(vec![
                            Expr::Symbol("cdr".to_string()),
                            Expr::List(vec![Expr::Symbol("args".to_string())]),
                        ]),
                    ]),
                ]),
            },
        ),
    ]
    .into_iter()
    .collect()
});
