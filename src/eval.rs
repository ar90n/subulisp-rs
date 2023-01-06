use super::context::{Context, Func};
use super::expr::Expr;
use std::collections::HashMap;

pub(crate) fn call(f: &Func, args: Vec<Expr>) -> anyhow::Result<(HashMap<String, Func>, Expr)> {
    let (func_args, body) = f.remap_arg_symbol();
    let env = func_args
        .iter()
        .zip(args.iter())
        .map(|(n, e)| (n.to_string(), Func::new(vec![], e.clone())))
        .collect::<HashMap<String, Func>>();
    Ok((env, body))
}

#[allow(dead_code)]
pub(crate) fn evaluate(expr: Expr, ctx: Context) -> anyhow::Result<(Expr, Context)> {
    match expr {
        Expr::List(mut elms) if !elms.is_empty() => {
            let (head_expr, rests) = {
                let rests = elms.split_off(1);
                let head_expr = elms.into_iter().next().unwrap();
                (head_expr, rests)
            };
            let (head_expr, ctx) = evaluate(head_expr, ctx)?;

            match (head_expr, rests) {
                (Expr::Symbol(s), args) => {
                    if let Some(f) = super::built_in::BUILT_IN_FUNCS.get(&s) {
                        f(args, ctx)
                    } else if let Some(f) = ctx.resolve(&s) {
                        let (env, expr) = call(f, args)?;
                        evaluate(expr, ctx.with(env))
                    } else {
                        Err(anyhow::anyhow!(
                            "failed to evaluate list: unknown symbol: {}",
                            s
                        ))
                    }
                }
                (head_expr, args) => {
                    let (es, ctx) =
                        args.into_iter()
                            .try_fold((vec![head_expr], ctx), |(mut es, ctx), e| {
                                let (e, ctx) = evaluate(e, ctx)?;
                                es.push(e);
                                anyhow::Ok((es, ctx))
                            })?;
                    Ok((Expr::List(es), ctx))
                }
            }
        }

        _ => Ok((expr, ctx)),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_builtin() {
        let ctx = Context::new();
        let (actual, ctx) = evaluate(
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
                ]),
            ]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::Number(-3.25), actual);

        let (actual, ctx) = evaluate(
            Expr::List(vec![
                Expr::Symbol("<".to_string()),
                Expr::Number(1.25),
                Expr::Number(2.0),
            ]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::Bool(true), actual);

        let (actual, ctx) = evaluate(
            Expr::List(vec![
                Expr::Symbol("=".to_string()),
                Expr::Number(1.25),
                Expr::Number(1.25),
            ]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::Bool(true), actual);

        let (actual, ctx) = evaluate(
            Expr::List(vec![Expr::Symbol("!".to_string()), Expr::Bool(false)]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::Bool(true), actual);

        let (actual, ctx) = evaluate(
            Expr::List(vec![
                Expr::Symbol("&".to_string()),
                Expr::Bool(true),
                Expr::Bool(true),
            ]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::Bool(true), actual);

        let (actual, _) = evaluate(
            Expr::List(vec![
                Expr::Symbol("if".to_string()),
                Expr::Bool(true),
                Expr::Number(1.25),
                Expr::Number(2.0),
            ]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::Number(1.25), actual);
    }

    #[test]
    fn test_predfined() {
        let ctx = Context::new();
        let (actual, ctx) = evaluate(
            Expr::List(vec![
                Expr::Symbol("|".to_string()),
                Expr::Bool(true),
                Expr::Bool(true),
            ]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::Bool(true), actual);

        let (actual, ctx) = evaluate(
            Expr::List(vec![
                Expr::Symbol("|".to_string()),
                Expr::Bool(false),
                Expr::Bool(true),
            ]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::Bool(true), actual);

        let (actual, ctx) = evaluate(
            Expr::List(vec![
                Expr::Symbol("|".to_string()),
                Expr::Bool(false),
                Expr::Bool(false),
            ]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::Bool(false), actual);

        let (actual, _) = evaluate(
            Expr::List(vec![
                Expr::Symbol("<=".to_string()),
                Expr::Number(1.0),
                Expr::Number(1.0),
            ]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::Bool(true), actual);
    }

    #[test]
    fn test_assign() {
        let ctx = Context::new();

        let (_, ctx) = evaluate(
            Expr::List(vec![
                Expr::Symbol("assign".to_string()),
                Expr::Symbol("A".to_string()),
                Expr::Number(1.0),
            ]),
            ctx,
        )
        .unwrap();
        let (_, ctx) = evaluate(
            Expr::List(vec![
                Expr::Symbol("assign".to_string()),
                Expr::Symbol("B".to_string()),
                Expr::Number(2.0),
            ]),
            ctx,
        )
        .unwrap();

        let (actual, _) = evaluate(Expr::List(vec![Expr::Symbol("A".to_string())]), ctx).unwrap();
        assert_eq!(Expr::Number(1.0), actual);
    }

    #[test]
    fn test_def() {
        let ctx = Context::new();
        let (_, ctx) = evaluate(
            Expr::List(vec![
                Expr::Symbol("def".to_string()),
                Expr::Symbol("ThreeTimes".to_string()),
                Expr::List(vec![Expr::Symbol("A".to_string())]),
                Expr::List(vec![
                    Expr::Symbol("*".to_string()),
                    Expr::Number(3.0),
                    Expr::List(vec![Expr::Symbol("A".to_string())]),
                ]),
            ]),
            ctx,
        )
        .unwrap();

        let (actual, _ctx) = evaluate(
            Expr::List(vec![
                Expr::Symbol("ThreeTimes".to_string()),
                Expr::Number(2.0),
            ]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::Number(6.0), actual);
    }

    #[test]
    fn test_list() {
        let ctx = Context::new();
        let (actual, ctx) = evaluate(
            Expr::List(vec![
                Expr::Symbol("list".to_string()),
                Expr::Number(1.0),
                Expr::Number(2.0),
            ]),
            ctx,
        )
        .unwrap();
        assert_eq!(
            Expr::List(vec![Expr::Number(1.0), Expr::Number(2.0)]),
            actual
        );

        let (actual, ctx) = evaluate(
            Expr::List(vec![
                Expr::Symbol("car".to_string()),
                Expr::List(vec![Expr::Number(1.0), Expr::Number(2.0)]),
            ]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::Number(1.0), actual);
        let (actual, _) = evaluate(
            Expr::List(vec![
                Expr::Symbol("cdr".to_string()),
                Expr::List(vec![Expr::Number(1.0), Expr::Number(2.0)]),
            ]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::List(vec![Expr::Number(2.0)]), actual);
    }

    #[test]
    fn test_fibo() {
        let ctx = Context::new();
        let (_, ctx) = evaluate(
            Expr::List(vec![
                Expr::Symbol("def".to_string()),
                Expr::Symbol("fibo".to_string()),
                Expr::List(vec![Expr::Symbol("n".to_string())]),
                Expr::List(vec![
                    Expr::Symbol("if".to_string()),
                    Expr::List(vec![
                        Expr::Symbol("<=".to_string()),
                        Expr::List(vec![Expr::Symbol("n".to_string())]),
                        Expr::Number(1.0),
                    ]),
                    Expr::Number(1.0),
                    Expr::List(vec![
                        Expr::Symbol("+".to_string()),
                        Expr::List(vec![
                            Expr::Symbol("fibo".to_string()),
                            Expr::List(vec![
                                Expr::Symbol("-".to_string()),
                                Expr::List(vec![Expr::Symbol("n".to_string())]),
                                Expr::Number(1.0),
                            ]),
                        ]),
                        Expr::List(vec![
                            Expr::Symbol("fibo".to_string()),
                            Expr::List(vec![
                                Expr::Symbol("-".to_string()),
                                Expr::List(vec![Expr::Symbol("n".to_string())]),
                                Expr::Number(2.0),
                            ]),
                        ]),
                    ]),
                ]),
            ]),
            ctx,
        )
        .unwrap();

        let (actual, _) = evaluate(
            Expr::List(vec![Expr::Symbol("fibo".to_string()), Expr::Number(11.0)]),
            ctx,
        )
        .unwrap();
        assert_eq!(Expr::Number(144.0), actual);
    }

    #[test]
    fn test_fold() {
        let ctx = Context::new();
        assert_eq!(
            Expr::Number(10.0),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("fold".to_string()),
                    Expr::Symbol("+".to_string()),
                    Expr::Number(0.0),
                    Expr::List(vec![
                        Expr::Number(1.0),
                        Expr::Number(2.0),
                        Expr::Number(3.0),
                        Expr::Number(4.0)
                    ]),
                ]),
                ctx
            )
            .map(|(e, _)| e)
            .unwrap()
        );
    }
}
