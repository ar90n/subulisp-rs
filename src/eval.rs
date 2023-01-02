use super::expr::Expr;

#[derive(Debug)]
pub(crate) struct Context {}

impl Context {
    #[allow(dead_code)]
    fn new() -> Self {
        Self {}
    }
}

type BuiltInFunc = fn(Vec<Expr>, &Context) -> anyhow::Result<Expr>;

fn built_in_add(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
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

fn built_in_sub(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
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

fn built_in_mul(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
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

fn built_in_div(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
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

fn built_in_lt(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
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

fn built_in_eq(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
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

fn built_in_and(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
    if args.len() != 2 {
        anyhow::bail!("failed to and: invalid number of arguments: {:?}", args);
    }

    let lhs = evaluate(args[0].clone(), ctx)?;
    let rhs = evaluate(args[1].clone(), ctx)?;
    match (lhs, rhs) {
        (Expr::Bool(lv), Expr::Bool(rv)) => Ok(Expr::Bool(lv & rv)),
        _ => anyhow::bail!("failed to and: invalid arguments: {:?}", args),
    }
}

fn built_in_not(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
    if args.len() != 1 {
        anyhow::bail!("failed to not: invalid number of arguments: {:?}", args);
    }

    let exp = evaluate(args[0].clone(), ctx)?;
    match exp {
        Expr::Bool(v) => Ok(Expr::Bool(!v)),
        _ => anyhow::bail!("failed to not: invalid arguments: {:?}", args),
    }
}

fn get_built_in_func(name: &str) -> Option<BuiltInFunc> {
    match name {
        "+" => Some(built_in_add),
        "-" => Some(built_in_sub),
        "*" => Some(built_in_mul),
        "/" => Some(built_in_div),
        "<" => Some(built_in_lt),
        "=" => Some(built_in_eq),
        "&" => Some(built_in_and),
        "!" => Some(built_in_not),
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
        _ => Ok(expr),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_evaluate() {
        let ctx = Context::new();
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
    }
}
