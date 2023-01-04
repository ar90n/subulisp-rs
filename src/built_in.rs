use std::collections::HashMap;

use super::context::Context;
use super::context::Func;
use super::eval::evaluate;
use super::expr::Expr;
use once_cell::sync::Lazy;

type BuiltInFunc = fn(Vec<Expr>, &mut Context) -> anyhow::Result<Expr>;

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

pub(crate) static BUILT_IN_FUNCS: Lazy<HashMap<String, BuiltInFunc>> = Lazy::new(|| {
    let mut m: HashMap<String, BuiltInFunc> = HashMap::new();
    m.insert("+".to_string(), add);
    m.insert("-".to_string(), sub);
    m.insert("*".to_string(), mul);
    m.insert("/".to_string(), div);
    m.insert("<".to_string(), lt);
    m.insert("=".to_string(), eq);
    m.insert("&".to_string(), and);
    m.insert("!".to_string(), not);
    m.insert("list".to_string(), list);
    m.insert("car".to_string(), car);
    m.insert("cdr".to_string(), cdr);
    m.insert("if".to_string(), if_);
    m.insert("assign".to_string(), assign);
    m.insert("def".to_string(), def);
    m
});
