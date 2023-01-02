mod built_in;
mod eval;
mod expr;
mod token;

use std::fmt::Debug;

#[derive(Debug, PartialEq)]
enum Result {
    Number(f64),
}

#[allow(dead_code)]
fn evaluate(expr: expr::Expr, ctx: &eval::Context) -> anyhow::Result<Result> {
    match eval::evaluate(expr, ctx)? {
        expr::Expr::Number(n) => Ok(Result::Number(n)),
        _ => unimplemented!(),
    }
}
