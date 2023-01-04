use super::context::Context;
use super::context::Func;
use super::expr::Expr;
use rand::distributions::Alphanumeric;
use rand::{thread_rng, Rng};
use std::collections::HashMap;

fn create_suffix() -> String {
    let mut rng = thread_rng();
    (0..10)
        .map(|_| rng.sample(Alphanumeric) as char)
        .collect::<String>()
}

pub(crate) fn call(args: Vec<Expr>, ctx: &mut Context) -> anyhow::Result<Expr> {
    match args.split_first() {
        Some((Expr::Symbol(s), args)) => match ctx.resolve(s) {
            Some(f) => {
                let suffix = create_suffix();
                let (func_args, body) = f.remap_arg_symbol(&suffix);
                let env = func_args
                    .iter()
                    .zip(args.iter())
                    .map(|(n, e)| (n.to_string(), Func::new(vec![], e.clone())))
                    .collect::<HashMap<String, Func>>();
                let mut ctx = ctx.with(env);
                evaluate(body, &mut ctx)
            }
            None => Err(anyhow::anyhow!(
                "failed to call: invalid arguments: {:?}",
                args
            )),
        },
        _ => Err(anyhow::anyhow!(
            "failed to call: invalid arguments 22 : {:?}",
            args
        )),
    }
}
#[allow(dead_code)]
pub(crate) fn evaluate(expr: Expr, ctx: &mut Context) -> anyhow::Result<Expr> {
    match expr {
        Expr::List(elms) => match elms.clone().split_first() {
            Some((Expr::Symbol(s), args)) => {
                if let Some(f) = super::built_in::BUILT_IN_FUNCS.get(s) {
                    let vs = args.to_vec();
                    f(vs, ctx)
                } else if let Some(_expr) = ctx.resolve(s) {
                    call(elms, ctx)
                } else {
                    Err(anyhow::anyhow!(
                        "failed to evaluate list: unknown symbol: {}",
                        s
                    ))
                }
            }
            _ => Ok(Expr::List(elms)),
        },
        Expr::Symbol(s) => {
            if let Some(_func) = ctx.resolve(&s) {
                call(vec![Expr::Symbol(s)], ctx)
            } else {
                Err(anyhow::anyhow!(
                    "failed to evaluate symbol: unknown symbol: {}",
                    s
                ))
            }
        }
        _ => Ok(expr),
    }
}

#[cfg(test)]
mod test {
    use super::super::context::{Context, Func};
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_evaluate() {
        let mut ctx = Context::new();
        evaluate(
            Expr::List(vec![
                Expr::Symbol("assign".to_string()),
                Expr::Symbol("A".to_string()),
                Expr::Number(1.0),
            ]),
            &mut ctx,
        )
        .unwrap();
        evaluate(
            Expr::List(vec![
                Expr::Symbol("assign".to_string()),
                Expr::Symbol("B".to_string()),
                Expr::Number(2.0),
            ]),
            &mut ctx,
        )
        .unwrap();
        evaluate(
            Expr::List(vec![
                Expr::Symbol("def".to_string()),
                Expr::Symbol("ThreeTimes".to_string()),
                Expr::List(vec![Expr::Symbol("A".to_string())]),
                Expr::List(vec![
                    Expr::Symbol("*".to_string()),
                    Expr::Number(3.0),
                    Expr::Symbol("A".to_string()),
                ]),
            ]),
            &mut ctx,
        )
        .unwrap();

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
                &mut ctx
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
                &mut ctx
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
                &mut ctx
            )
            .unwrap()
        );
        assert_eq!(
            Expr::Bool(true),
            evaluate(
                Expr::List(vec![Expr::Symbol("!".to_string()), Expr::Bool(false),]),
                &mut ctx
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
                &mut ctx
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
                &mut ctx
            )
            .unwrap()
        );

        assert_eq!(
            Expr::Number(1.0),
            evaluate(Expr::Symbol("A".to_string()), &mut ctx).unwrap()
        );

        let mut env2 = HashMap::new();
        env2.insert("A".to_string(), Func::new(vec![], Expr::Number(3.0)));
        let mut ctx2 = ctx.with(env2);
        assert_eq!(
            Expr::Number(3.0),
            evaluate(Expr::Symbol("A".to_string()), &mut ctx2).unwrap()
        );
        assert_eq!(
            Expr::Number(2.0),
            evaluate(Expr::Symbol("B".to_string()), &mut ctx2).unwrap()
        );
        assert_eq!(
            Expr::Bool(true),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("|".to_string()),
                    Expr::Bool(true),
                    Expr::Bool(true),
                ]),
                &mut ctx
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
                &mut ctx
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
                &mut ctx
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
                &mut ctx
            )
            .unwrap()
        );

        assert_eq!(
            Expr::List(vec![Expr::Number(1.0), Expr::Number(2.0)]),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("list".to_string()),
                    Expr::Number(1.0),
                    Expr::Number(2.0),
                ]),
                &mut ctx
            )
            .unwrap()
        );
        assert_eq!(
            Expr::Number(1.0),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("car".to_string()),
                    Expr::List(vec![Expr::Number(1.0), Expr::Number(2.0)]),
                ]),
                &mut ctx
            )
            .unwrap()
        );
        assert_eq!(
            Expr::List(vec![Expr::Number(2.0)]),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("cdr".to_string()),
                    Expr::List(vec![Expr::Number(1.0), Expr::Number(2.0)]),
                ]),
                &mut ctx
            )
            .unwrap()
        );
        assert_eq!(
            Expr::Number(6.0),
            evaluate(
                Expr::List(vec![
                    Expr::Symbol("ThreeTimes".to_string()),
                    Expr::Number(2.0),
                ]),
                &mut ctx
            )
            .unwrap()
        );
    }
    #[test]
    fn test_fibo() {
        let mut ctx = Context::new();
        evaluate(
            Expr::List(vec![
                Expr::Symbol("def".to_string()),
                Expr::Symbol("fibo".to_string()),
                Expr::List(vec![Expr::Symbol("n".to_string())]),
                Expr::List(vec![
                    Expr::Symbol("if".to_string()),
                    Expr::List(vec![
                        Expr::Symbol("<=".to_string()),
                        Expr::Symbol("n".to_string()),
                        Expr::Number(1.0),
                    ]),
                    Expr::Number(1.0),
                    Expr::List(vec![
                        Expr::Symbol("+".to_string()),
                        Expr::List(vec![
                            Expr::Symbol("fibo".to_string()),
                            Expr::List(vec![
                                Expr::Symbol("-".to_string()),
                                Expr::Symbol("n".to_string()),
                                Expr::Number(1.0),
                            ]),
                        ]),
                        Expr::List(vec![
                            Expr::Symbol("fibo".to_string()),
                            Expr::List(vec![
                                Expr::Symbol("-".to_string()),
                                Expr::Symbol("n".to_string()),
                                Expr::Number(2.0),
                            ]),
                        ]),
                    ]),
                ]),
            ]),
            &mut ctx,
        )
        .unwrap();
        assert_eq!(
            Expr::Number(377.0),
            evaluate(
                Expr::List(vec![Expr::Symbol("fibo".to_string()), Expr::Number(13.0),]),
                &mut ctx
            )
            .unwrap()
        );
    }
}
