use std::collections::HashMap;

use super::expr::Expr;

#[derive(Debug)]
pub(crate) struct Context<'a> {
    env: HashMap<String, Expr>,
    parent: Option<&'a Context<'a>>,
}

impl<'a> Context<'a> {
    #[allow(dead_code)]
    fn new() -> Self {
        Self {
            env: HashMap::new(),
            parent: None,
        }
    }

    pub fn resolve(&self, name: &str) -> Option<&Expr> {
        self.env
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|p| p.resolve(name)))
    }

    #[allow(dead_code)]
    pub fn register(&mut self, name: &str, expr: Expr) {
        self.env.insert(name.to_string(), expr);
    }

    #[allow(dead_code)]
    pub fn with(&'a self, env: HashMap<String, Expr>) -> Context<'a> {
        Self {
            env,
            parent: Some(self),
        }
    }
}

type BuiltInFunc = fn(Vec<Expr>, &Context) -> anyhow::Result<Expr>;

fn get_built_in_func(name: &str) -> Option<BuiltInFunc> {
    match name {
        "+" => Some(built_in::add),
        "-" => Some(built_in::sub),
        "*" => Some(built_in::mul),
        "/" => Some(built_in::div),
        "<" => Some(built_in::lt),
        "=" => Some(built_in::eq),
        "&" => Some(built_in::and),
        "!" => Some(built_in::not),
        "if" => Some(built_in::if_),
        _ => None,
    }
}

#[allow(dead_code)]
pub(crate) fn evaluate(expr: Expr, ctx: &Context) -> anyhow::Result<Expr> {
    match expr {
        Expr::List(elms) => {
            let mut elms = elms.into_iter();
            let func = elms.next().ok_or_else(|| {
                anyhow::anyhow!("failed to evaluate list: empty list is not allowed")
            })?;
            let args = elms.collect::<Vec<_>>();
            match func {
                Expr::Symbol(s) => {
                    if let Some(f) = get_built_in_func(&s) {
                        f(args, ctx)
                    } else {
                        anyhow::bail!("failed to evaluate list: unknown symbol: {}", s)
                    }
                }
                _ => anyhow::bail!("failed to evaluate list: invalid function: {:?}", func),
            }
        }
        Expr::Symbol(s) => {
            if let Some(expr) = ctx.resolve(&s) {
                Ok(expr.clone())
            } else {
                anyhow::bail!("failed to evaluate symbol: unknown symbol: {}", s)
            }
        }
        _ => Ok(expr),
    }
}

mod built_in {
    use super::evaluate;
    use super::Context;
    use super::Expr;

    pub(crate) fn add(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
        if args.len() != 2 {
            anyhow::bail!("failed to add: invalid number of arguments: {:?}", args);
        }

        let lhs = evaluate(args[0].clone(), ctx)?;
        let rhs = evaluate(args[1].clone(), ctx)?;
        match (lhs, rhs) {
            (Expr::Number(n1), Expr::Number(n2)) => Ok(Expr::Number(n1 + n2)),
            _ => anyhow::bail!("failed to add: invalid arguments: {:?}", args),
        }
    }

    pub(crate) fn sub(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
        if args.len() != 2 {
            anyhow::bail!("failed to sub: invalid number of arguments: {:?}", args);
        }

        let lhs = evaluate(args[0].clone(), ctx)?;
        let rhs = evaluate(args[1].clone(), ctx)?;
        match (lhs, rhs) {
            (Expr::Number(n1), Expr::Number(n2)) => Ok(Expr::Number(n1 - n2)),
            _ => anyhow::bail!("failed to sub: invalid arguments: {:?}", args),
        }
    }

    pub(crate) fn mul(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
        if args.len() != 2 {
            anyhow::bail!("failed to mul: invalid number of arguments: {:?}", args);
        }

        let lhs = evaluate(args[0].clone(), ctx)?;
        let rhs = evaluate(args[1].clone(), ctx)?;
        match (lhs, rhs) {
            (Expr::Number(n1), Expr::Number(n2)) => Ok(Expr::Number(n1 * n2)),
            _ => anyhow::bail!("failed to mul: invalid arguments: {:?}", args),
        }
    }

    pub(crate) fn div(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
        if args.len() != 2 {
            anyhow::bail!("failed to div: invalid number of arguments: {:?}", args);
        }

        let lhs = evaluate(args[0].clone(), ctx)?;
        let rhs = evaluate(args[1].clone(), ctx)?;
        match (lhs, rhs) {
            (Expr::Number(lv), Expr::Number(rv)) if rv != 0.0 => Ok(Expr::Number(lv / rv)),
            _ => anyhow::bail!("failed to div: invalid arguments: {:?}", args),
        }
    }

    pub(crate) fn lt(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
        if args.len() != 2 {
            anyhow::bail!("failed to lt: invalid number of arguments: {:?}", args);
        }

        let lhs = evaluate(args[0].clone(), ctx)?;
        let rhs = evaluate(args[1].clone(), ctx)?;
        match (lhs, rhs) {
            (Expr::Number(lv), Expr::Number(rv)) => Ok(Expr::Bool(lv < rv)),
            _ => anyhow::bail!("failed to lt: invalid arguments: {:?}", args),
        }
    }

    pub(crate) fn eq(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
        if args.len() != 2 {
            anyhow::bail!("failed to eq: invalid number of arguments: {:?}", args);
        }

        let lhs = evaluate(args[0].clone(), ctx)?;
        let rhs = evaluate(args[1].clone(), ctx)?;
        match (lhs, rhs) {
            (Expr::Number(lv), Expr::Number(rv)) => Ok(Expr::Bool(lv == rv)),
            _ => anyhow::bail!("failed to eq: invalid arguments: {:?}", args),
        }
    }

    pub(crate) fn and(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
        if args.len() != 2 {
            anyhow::bail!("failed to and: invalid number of arguments: {:?}", args);
        }

        let lhs = evaluate(args[0].clone(), ctx)?;
        match lhs {
            Expr::Bool(v) if v => Ok(evaluate(args[1].clone(), ctx)?),
            Expr::Bool(v) if !v => Ok(Expr::Bool(false)),
            _ => anyhow::bail!("failed to if: invalid arguments: {:?}", args),
        }
    }
    pub(crate) fn not(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
        if args.len() != 1 {
            anyhow::bail!("failed to not: invalid number of arguments: {:?}", args);
        }

        let exp = evaluate(args[0].clone(), ctx)?;
        match exp {
            Expr::Bool(v) => Ok(Expr::Bool(!v)),
            _ => anyhow::bail!("failed to not: invalid arguments: {:?}", args),
        }
    }

    pub(crate) fn if_(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
        if args.len() != 3 {
            anyhow::bail!("failed to if: invalid number of arguments: {:?}", args);
        }

        let cond = evaluate(args[0].clone(), ctx)?;
        match cond {
            Expr::Bool(c) if c => Ok(evaluate(args[1].clone(), ctx)?),
            Expr::Bool(c) if !c => Ok(evaluate(args[2].clone(), ctx)?),
            _ => anyhow::bail!("failed to if: invalid arguments: {:?}", args),
        }
    }
}

#[cfg(test)]
mod test {
    use std::hash::Hash;

    use super::*;

    #[test]
    fn test_evaluate() {
        let mut ctx = Context::new();
        assert_eq!(
            Expr::Number(-3.25),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("*".to_string()),
                    Expr::List(vec![
                        Expr::Symbol("+".to_string()),
                        Expr::Number(1.25),
                        Expr::Number(2.0),
                    ]),
                    Expr::List(vec![
                        Expr::Symbol("-".to_string()),
                        Expr::Number(1.0),
                        Expr::Number(2.0),
                    ])
                ]),
                &ctx
            )
            .unwrap()
        );
        assert_eq!(
            Expr::Bool(true),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("<".to_string()),
                    Expr::Number(1.25),
                    Expr::Number(2.0),
                ]),
                &ctx
            )
            .unwrap()
        );
        assert_eq!(
            Expr::Bool(true),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("=".to_string()),
                    Expr::Number(1.25),
                    Expr::Number(1.25),
                ]),
                &ctx
            )
            .unwrap()
        );
        assert_eq!(
            Expr::Bool(true),
            evaluate(
                Expr::List(vec![Expr::Symbol("!".to_string()), Expr::Bool(false),]),
                &ctx
            )
            .unwrap()
        );
        assert_eq!(
            Expr::Bool(true),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("&".to_string()),
                    Expr::Bool(true),
                    Expr::Bool(true),
                ]),
                &ctx
            )
            .unwrap()
        );
        assert_eq!(
            Expr::Number(1.25),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("if".to_string()),
                    Expr::Bool(true),
                    Expr::Number(1.25),
                    Expr::Number(2.0),
                ]),
                &ctx
            )
            .unwrap()
        );

        ctx.register("A", Expr::Number(1.0));
        ctx.register("B", Expr::Number(2.0));
        assert_eq!(
            Expr::Number(1.0),
            evaluate(Expr::Symbol("A".to_string()), &ctx).unwrap()
        );

        let mut env2 = HashMap::new();
        env2.insert("A".to_string(), Expr::Number(3.0));
        let ctx2 = ctx.with(env2);
        assert_eq!(
            Expr::Number(3.0),
            evaluate(Expr::Symbol("A".to_string()), &ctx2).unwrap()
        );
        assert_eq!(
            Expr::Number(2.0),
            evaluate(Expr::Symbol("B".to_string()), &ctx2).unwrap()
        );
    }
}
