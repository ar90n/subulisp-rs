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
            Result::Number(n) => write!(f, "{n}"),
            Result::List(list) => {
                write!(f, "(")?;
                for (i, item) in list.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
        }
    }
}

pub struct Lisp {
    ctx: Option<Context>,
}

impl Default for Lisp {
    fn default() -> Self {
        Self::new()
    }
}

impl Lisp {
    pub fn new() -> Self {
        Self {
            ctx: Some(Context::new()),
        }
    }

    pub fn evaluate(&mut self, expr: String) -> anyhow::Result<Result> {
        let tokens = token::tokenize(&expr)?;
        let expr = expr::parse(&tokens)?;
        self.evaluate_expr(expr).map(Result::from)
    }

    fn evaluate_expr(&mut self, epxr: Expr) -> anyhow::Result<Result> {
        match self.ctx.take() {
            Some(ctx) => {
                let (expr, ctx) = eval::evaluate(epxr, ctx)?;
                self.ctx = Some(ctx);
                Ok(Result::from(expr))
            }
            None => anyhow::bail!("Context is not initialized"),
        }
    }
}
