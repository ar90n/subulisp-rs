use super::context::Context;
use super::expr::Expr;

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
        Expr::List(elms) => match elms.clone().split_first() {
            Some((Expr::Symbol(s), args)) => {
                if let Some(f) = get_built_in_func(s) {
                    let vs = args.to_vec();
                    f(vs, ctx)
                } else if let Some(_expr) = ctx.resolve(s) {
                    built_in::call(elms, ctx)
                } else {
                    anyhow::bail!("failed to evaluate list: unknown symbol: {}", s)
                }
            }
            _ => Ok(Expr::List(elms)),
        },
        Expr::Symbol(s) => {
            if let Some(_func) = ctx.resolve(&s) {
                built_in::call(vec![Expr::Symbol(s)], ctx)
            } else {
                anyhow::bail!("failed to evaluate symbol: unknown symbol: {}", s)
            }
        }
        _ => Ok(expr),
    }
}

mod built_in {
    use std::collections::HashMap;

    use super::super::context::Func;
    use super::evaluate;
    use super::Context;
    use super::Expr;
    use rand::distributions::Alphanumeric;
    use rand::{thread_rng, Rng};

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

    pub(crate) fn call(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
        match args.split_first() {
            Some((Expr::Symbol(s), args)) => match ctx.resolve(s) {
                Some(f) => {
                    let mut rng = thread_rng();
                    let suffix = (0..10)
                        .map(|_| rng.sample(Alphanumeric) as char)
                        .collect::<String>();

                    fn remap_symbol(e: Expr, suffix: &str, args: &Vec<String>) -> Expr {
                        match e {
                            Expr::Symbol(s) => {
                                if args.contains(&s) {
                                    Expr::Symbol(format!("{}_{}", s, suffix))
                                } else {
                                    Expr::Symbol(s)
                                }
                            }
                            Expr::List(es) => Expr::List(
                                es.into_iter()
                                    .map(|e| remap_symbol(e, suffix, args))
                                    .collect(),
                            ),
                            _ => e,
                        }
                    }
                    let body = remap_symbol(f.body.clone(), &suffix, &f.args);

                    let env = args
                        .iter()
                        .zip(f.args.iter())
                        .map(|(e, n)| {
                            (
                                format!("{}_{}", n, suffix),
                                Func {
                                    args: vec![],
                                    body: e.clone(),
                                },
                            )
                        })
                        .collect::<HashMap<String, Func>>();
                    let ctx = ctx.with(env);
                    evaluate(body, &ctx)
                }
                None => anyhow::bail!("failed to call: invalid arguments: {:?}", args),
            },
            _ => anyhow::bail!("failed to call: invalid arguments 22 : {:?}", args),
        }
    }
}

#[cfg(test)]
mod test {
    use std::hash::Hash;

    use super::super::context::{create_root_context, Context, Func};
    use super::*;
    use std::collections::HashMap;

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

        ctx.register(
            "A",
            Func {
                args: vec![],
                body: Expr::Number(1.0),
            },
        );
        ctx.register(
            "B",
            Func {
                args: vec![],
                body: Expr::Number(2.0),
            },
        );

        assert_eq!(
            Expr::Number(1.0),
            evaluate(Expr::Symbol("A".to_string()), &ctx).unwrap()
        );

        let mut env2 = HashMap::new();
        env2.insert(
            "A".to_string(),
            Func {
                args: vec![],
                body: Expr::Number(3.0),
            },
        );
        let ctx2 = ctx.with(env2);
        assert_eq!(
            Expr::Number(3.0),
            evaluate(Expr::Symbol("A".to_string()), &ctx2).unwrap()
        );
        assert_eq!(
            Expr::Number(2.0),
            evaluate(Expr::Symbol("B".to_string()), &ctx2).unwrap()
        );
        let ctx3 = create_root_context();
        assert_eq!(
            Expr::Bool(true),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("|".to_string()),
                    Expr::Bool(true),
                    Expr::Bool(true),
                ]),
                &ctx3
            )
            .unwrap()
        );
        assert_eq!(
            Expr::Bool(true),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("|".to_string()),
                    Expr::Bool(false),
                    Expr::Bool(true),
                ]),
                &ctx3
            )
            .unwrap()
        );
        assert_eq!(
            Expr::Bool(false),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("|".to_string()),
                    Expr::Bool(false),
                    Expr::Bool(false),
                ]),
                &ctx3
            )
            .unwrap()
        );
        assert_eq!(
            Expr::Bool(true),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("<=".to_string()),
                    Expr::Number(1.0),
                    Expr::Number(1.0),
                ]),
                &ctx3
            )
            .unwrap()
        );
    }
}
