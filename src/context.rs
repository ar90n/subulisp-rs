use super::expr::Expr;
use once_cell::sync::Lazy;
use std::collections::HashMap;

#[derive(Debug)]
pub(crate) struct Func {
    pub args: Vec<String>,
    pub body: Expr,
}

#[derive(Debug)]
pub(crate) struct Context<'a> {
    env: HashMap<String, Func>,
    parent: Option<&'a Context<'a>>,
}

impl<'a> Context<'a> {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
            parent: Some(&ROOT_CONTEXT),
        }
    }

    fn new_root() -> Self {
        Self {
            env: create_root_env(),
            parent: None,
        }
    }

    pub fn resolve(&self, name: &str) -> Option<&Func> {
        self.env
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|p| p.resolve(name)))
    }

    pub fn register(&mut self, name: &str, func: Func) {
        self.env.insert(name.to_string(), func);
    }

    pub fn with(&'a self, env: HashMap<String, Func>) -> Context<'a> {
        Self {
            env,
            parent: Some(self),
        }
    }
}

fn create_root_env() -> HashMap<String, Func> {
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
                            Expr::Symbol("lhs".to_string()),
                        ]),
                        Expr::List(vec![
                            Expr::Symbol("!".to_string()),
                            Expr::Symbol("rhs".to_string()),
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
                        Expr::Symbol("lhs".to_string()),
                        Expr::Symbol("rhs".to_string()),
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
                        Expr::Symbol("lhs".to_string()),
                        Expr::Symbol("rhs".to_string()),
                    ]),
                    Expr::List(vec![
                        Expr::Symbol("=".to_string()),
                        Expr::Symbol("lhs".to_string()),
                        Expr::Symbol("rhs".to_string()),
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
                        Expr::Symbol("lhs".to_string()),
                        Expr::Symbol("rhs".to_string()),
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
                        Expr::Symbol("lhs".to_string()),
                        Expr::Symbol("rhs".to_string()),
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
                    Expr::Symbol("v".to_string()),
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
                    Expr::Symbol("v".to_string()),
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
                    Expr::Symbol("v".to_string()),
                    Expr::Number(1.0),
                ]),
            },
        ),
    ]
    .into_iter()
    .collect()
}

static ROOT_CONTEXT: Lazy<Context<'static>> = Lazy::new(Context::new_root);
