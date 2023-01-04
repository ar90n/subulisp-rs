use super::context::Context;
use super::expr::Expr;

type BuiltInFunc = fn(Vec<Expr>, &mut Context) -> anyhow::Result<Expr>;

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
        "list" => Some(built_in::list),
        "car" => Some(built_in::car),
        "cdr" => Some(built_in::cdr),
        "if" => Some(built_in::if_),
        "assign" => Some(built_in::assign),
        "def" => Some(built_in::def),
        _ => None,
    }
}

#[allow(dead_code)]
pub(crate) fn evaluate(expr: Expr, ctx: &mut Context) -> anyhow::Result<Expr> {
    match expr {
        Expr::List(elms) => match elms.clone().split_first() {
            Some((Expr::Symbol(s), args)) => {
                if let Some(f) = get_built_in_func(s) {
                    let vs = args.to_vec();
                    f(vs, ctx)
                } else if let Some(_expr) = ctx.resolve(s) {
                    built_in::call(elms, ctx)
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
                built_in::call(vec![Expr::Symbol(s)], ctx)
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

mod built_in {
    use std::collections::HashMap;

    use super::super::context::Func;
    use super::evaluate;
    use super::Context;
    use super::Expr;
    use rand::distributions::Alphanumeric;
    use rand::{thread_rng, Rng};

    macro_rules! unary_op {
        ( $name:ident, $p:pat, $e:expr, $c:expr ) => {
            pub(crate) fn $name(args: Vec<Expr>, ctx: &mut Context) -> anyhow::Result<Expr> {
                if args.len() != 1 {
                    anyhow::bail!(
                        "failed to {}: invalid number of arguments: {:?}",
                        stringify!($name),
                        args
                    );
                }

                let mut args = args;
                evaluate(args.pop().unwrap(), ctx).and_then(|exp| match exp {
                    $p if $c => Ok($e),
                    _ => Err(anyhow::anyhow!(
                        "failed to {}: invalid arguments: {:?}",
                        stringify!($name),
                        args
                    )),
                })
            }
        };
        ( $name:ident, $p:pat, $e:expr) => {
            unary_op!($name, $p, $e, true);
        };
    }

    macro_rules! binary_op {
        ( $name:ident, $p:pat, $e:expr, $c:expr ) => {
            pub(crate) fn $name(args: Vec<Expr>, ctx: &mut Context) -> anyhow::Result<Expr> {
                if args.len() != 2 {
                    anyhow::bail!(
                        "failed to {}: invalid number of arguments: {:?}",
                        stringify!($name),
                        args
                    );
                }

                let mut args = args;
                let rhs = evaluate(args.pop().unwrap(), ctx)?;
                let lhs = evaluate(args.pop().unwrap(), ctx)?;
                match (lhs, rhs) {
                    $p if $c => Ok($e),
                    _ => Err(anyhow::anyhow!(
                        "failed to {}: invalid arguments: {:?}",
                        stringify!($name),
                        args
                    )),
                }
            }
        };
        ( $name:ident, $p:pat, $e:expr) => {
            binary_op!($name, $p, $e, true);
        };
    }

    binary_op!(
        add,
        (Expr::Number(lv), Expr::Number(rv)),
        Expr::Number(lv + rv)
    );

    binary_op!(
        sub,
        (Expr::Number(lv), Expr::Number(rv)),
        Expr::Number(lv - rv)
    );

    binary_op!(
        mul,
        (Expr::Number(lv), Expr::Number(rv)),
        Expr::Number(lv * rv)
    );

    binary_op!(
        div,
        (Expr::Number(lv), Expr::Number(rv)),
        Expr::Number(lv / rv),
        rv != 0.0
    );

    binary_op!(
        lt,
        (Expr::Number(lv), Expr::Number(rv)),
        Expr::Bool(lv < rv)
    );

    binary_op!(
        eq,
        (Expr::Number(lv), Expr::Number(rv)),
        Expr::Bool(lv == rv)
    );

    binary_op!(and, (Expr::Bool(lv), Expr::Bool(rv)), Expr::Bool(lv && rv));

    unary_op!(not, Expr::Bool(v), Expr::Bool(!v));

    pub(crate) fn list(args: Vec<Expr>, _ctx: &mut Context) -> anyhow::Result<Expr> {
        Ok(Expr::List(args))
    }

    unary_op!(
        car,
        Expr::List(es),
        es.split_first().map(|(e, _)| e.clone()).unwrap(),
        !es.is_empty()
    );

    unary_op!(
        cdr,
        Expr::List(es),
        es.split_first()
            .map(|(_, es)| es.to_vec())
            .or(Some(vec![]))
            .map(Expr::List)
            .unwrap()
    );

    pub(crate) fn call(args: Vec<Expr>, ctx: &mut Context) -> anyhow::Result<Expr> {
        fn create_suffix() -> String {
            let mut rng = thread_rng();
            (0..10)
                .map(|_| rng.sample(Alphanumeric) as char)
                .collect::<String>()
        }

        fn create_remapped_body(f: &Func, suffix: &str) -> Expr {
            fn remap_body(e: &Expr, args: &Vec<String>, suffix: &str) -> Expr {
                match e {
                    Expr::Symbol(s) => {
                        if args.contains(s) {
                            Expr::Symbol(format!("{}_{}", s, suffix))
                        } else {
                            Expr::Symbol(s.clone())
                        }
                    }
                    Expr::List(es) => {
                        Expr::List(es.iter().map(|e| remap_body(e, args, suffix)).collect())
                    }
                    _ => e.clone(),
                }
            }
            remap_body(&f.body, &f.args, suffix)
        }

        match args.split_first() {
            Some((Expr::Symbol(s), args)) => match ctx.resolve(s) {
                Some(f) => {
                    let suffix = create_suffix();
                    let body = create_remapped_body(f, &suffix);
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

    pub(crate) fn assign(args: Vec<Expr>, ctx: &mut Context) -> anyhow::Result<Expr> {
        let args: [Expr; 2] = args.try_into().map_err(|args| {
            anyhow::anyhow!("failed to assign: invalid number of arguments: {:?}", args)
        })?;

        match args {
            [Expr::Symbol(name), body] => {
                ctx.register(&name, Func { args: vec![], body });
                Ok(Expr::List(vec![]))
            }
            _ => Err(anyhow::anyhow!(
                "failed to assign: invalid arguments: {:?}",
                args
            )),
        }
    }

    pub(crate) fn def(args: Vec<Expr>, ctx: &mut Context) -> anyhow::Result<Expr> {
        let args: [Expr; 3] = args.try_into().map_err(|args| {
            anyhow::anyhow!("failed to assign: invalid number of arguments: {:?}", args)
        })?;
        match args {
            [Expr::Symbol(s), Expr::List(args), body] => {
                let args = args
                    .into_iter()
                    .map(|e| match e {
                        Expr::Symbol(s) => Ok(s),
                        _ => Err(anyhow::anyhow!(
                            "failed to assign: found not symbol expr in args: {:?}",
                            e
                        )),
                    })
                    .collect::<anyhow::Result<Vec<String>>>()?;

                ctx.register(&s, Func { args, body });
                Ok(Expr::List(vec![]))
            }
            _ => Err(anyhow::anyhow!(
                "failed to assign: invalid arguments: {:?}",
                args
            )),
        }
    }

    pub(crate) fn if_(args: Vec<Expr>, ctx: &mut Context) -> anyhow::Result<Expr> {
        let [cond_expr, main_expr, else_expr]: [Expr; 3] = args.try_into().map_err(|args| {
            anyhow::anyhow!("failed to assign: invalid number of arguments: {:?}", args)
        })?;

        let cond = evaluate(cond_expr, ctx)?;
        match cond {
            Expr::Bool(c) if c => Ok(evaluate(main_expr, ctx)?),
            Expr::Bool(c) if !c => Ok(evaluate(else_expr, ctx)?),
            _ => Err(anyhow::anyhow!(
                "failed to if: invalid condition: {:?}",
                cond
            )),
        }
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
        env2.insert(
            "A".to_string(),
            Func {
                args: vec![],
                body: Expr::Number(3.0),
            },
        );
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
