mod built_in;
mod context;
mod eval;
mod expr;
mod token;

use context::Context;
use expr::Expr;
use std::fmt::{Debug, Display};

#[derive(Debug, PartialEq)]
pub enum Result {
    Number(f64),
    List(Vec<Result>),
}

impl From<Expr> for Result {
    fn from(expr: Expr) -> Self {
        match expr {
            Expr::Number(n) => Result::Number(n),
            Expr::List(list) => Result::List(list.into_iter().map(Result::from).collect()),
            _ => unimplemented!(),
        }
    }
}

impl Display for Result {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Result::Number(n) => write!(f, "{}", n),
            Result::List(list) => {
                write!(f, "(")?;
                for (i, item) in list.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
        }
    }
}

pub struct Lisp<'a> {
    ctx: Context<'a>,
}

impl Default for Lisp<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl Lisp<'_> {
    pub fn new() -> Self {
        Self {
            ctx: Context::new(),
        }
    }

    pub fn evaluate(&mut self, expr: String) -> anyhow::Result<Result> {
        let tokens = token::tokenize(&expr)?;
        let expr = expr::parse(&tokens)?;
        eval::evaluate(expr, &mut self.ctx).map(Result::from)
    }
}
