use std::fmt::Debug;

#[derive(Debug, PartialEq)]
enum Token {
    LeftParen,
    RightParen,
    Number(f64),
    Bool(bool),
    Symbol(String),
}

#[derive(Debug, PartialEq, Clone)]
enum Expr {
    Number(f64),
    Bool(bool),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, PartialEq)]
enum Result {
    Number(f64),
}

#[derive(Debug)]
struct Context {}

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

    let lhs = evaluate_impl(args[0].clone(), ctx)?;
    let rhs = evaluate_impl(args[1].clone(), ctx)?;
    match (lhs, rhs) {
        (Expr::Number(n1), Expr::Number(n2)) => Ok(Expr::Number(n1 + n2)),
        _ => anyhow::bail!("failed to add: invalid arguments: {:?}", args),
    }
}

fn built_in_sub(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
    if args.len() != 2 {
        anyhow::bail!("failed to sub: invalid number of arguments: {:?}", args);
    }

    let lhs = evaluate_impl(args[0].clone(), ctx)?;
    let rhs = evaluate_impl(args[1].clone(), ctx)?;
    match (lhs, rhs) {
        (Expr::Number(n1), Expr::Number(n2)) => Ok(Expr::Number(n1 - n2)),
        _ => anyhow::bail!("failed to sub: invalid arguments: {:?}", args),
    }
}

fn built_in_mul(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
    if args.len() != 2 {
        anyhow::bail!("failed to mul: invalid number of arguments: {:?}", args);
    }

    let lhs = evaluate_impl(args[0].clone(), ctx)?;
    let rhs = evaluate_impl(args[1].clone(), ctx)?;
    match (lhs, rhs) {
        (Expr::Number(n1), Expr::Number(n2)) => Ok(Expr::Number(n1 * n2)),
        _ => anyhow::bail!("failed to mul: invalid arguments: {:?}", args),
    }
}

fn built_in_div(args: Vec<Expr>, ctx: &Context) -> anyhow::Result<Expr> {
    if args.len() != 2 {
        anyhow::bail!("failed to div: invalid number of arguments: {:?}", args);
    }

    let lhs = evaluate_impl(args[0].clone(), ctx)?;
    let rhs = evaluate_impl(args[1].clone(), ctx)?;
    match (lhs, rhs) {
        (Expr::Number(lv), Expr::Number(rv)) if rv != 0.0 => Ok(Expr::Number(lv / rv)),
        _ => anyhow::bail!("failed to div: invalid arguments: {:?}", args),
    }
}

fn get_built_in_func(name: &str) -> Option<BuiltInFunc> {
    match name {
        "+" => Some(built_in_add),
        "-" => Some(built_in_sub),
        "*" => Some(built_in_mul),
        "/" => Some(built_in_div),
        _ => None,
    }
}

#[allow(dead_code)]
fn tokenize(code: &str) -> anyhow::Result<Vec<Token>> {
    fn parse_paren(s: &str) -> anyhow::Result<Token> {
        match s {
            "(" => Ok(Token::LeftParen),
            ")" => Ok(Token::RightParen),
            _ => Err(anyhow::anyhow!("failed to parse token as paren")),
        }
    }

    fn parse_number(s: &str) -> anyhow::Result<Token> {
        s.parse::<f64>()
            .map(Token::Number)
            .or(Err(anyhow::anyhow!("failed to parse token as number")))
    }

    fn parse_bool(s: &str) -> anyhow::Result<Token> {
        s.parse::<bool>()
            .map(Token::Bool)
            .or(Err(anyhow::anyhow!("failed to parse token as bool")))
    }

    fn parse_symbol(s: &str) -> anyhow::Result<Token> {
        Ok(Token::Symbol(s.to_string()))
    }

    fn parse(s: &str) -> anyhow::Result<Token> {
        parse_paren(s)
            .or_else(|_| parse_number(s))
            .or_else(|_| parse_bool(s))
            .or_else(|_| parse_symbol(s))
            .or(Err(anyhow::anyhow!("failed to parse token: {}", s)))
    }

    code.replace('(', " ( ")
        .replace(')', " ) ")
        .split_whitespace()
        .map(parse)
        .collect()
}

#[allow(dead_code)]
fn parse(tokens: &[Token]) -> anyhow::Result<Expr> {
    fn consume_token(tokens: &[Token], t: Token) -> anyhow::Result<&[Token]> {
        match tokens {
            [head, rest @ ..] if head == &t => Ok(rest),
            _ => anyhow::bail!("failed to consume token: {:?}", t),
        }
    }

    fn parse_number(tokens: &[Token]) -> anyhow::Result<(Expr, &[Token])> {
        match tokens {
            [Token::Number(n), rest @ ..] => Ok((Expr::Number(*n), rest)),
            _ => Err(anyhow::anyhow!(
                "failed to parse token as number. tokens: {:?}",
                tokens
            )),
        }
    }

    fn parse_bool(tokens: &[Token]) -> anyhow::Result<(Expr, &[Token])> {
        match tokens {
            [Token::Bool(b), rest @ ..] => Ok((Expr::Bool(*b), rest)),
            _ => Err(anyhow::anyhow!(
                "failed to parse token as bool. tokens: {:?}",
                tokens
            )),
        }
    }

    fn parse_symbol(tokens: &[Token]) -> anyhow::Result<(Expr, &[Token])> {
        match tokens {
            [Token::Symbol(s), rest @ ..] => Ok((Expr::Symbol(s.clone()), rest)),
            _ => Err(anyhow::anyhow!(
                "failed to parse token as symbol. tokens: {:?}",
                tokens
            )),
        }
    }

    fn parse_list(tokens: &[Token]) -> anyhow::Result<(Expr, &[Token])> {
        fn parse_elements(tokens: &[Token]) -> anyhow::Result<(Vec<Expr>, &[Token])> {
            let mut elms = Vec::new();
            let mut tokens = tokens;
            while let Ok((elm, rest)) = parse_impl(tokens) {
                tokens = rest;
                elms.push(elm);
            }
            Ok((elms, tokens))
        }

        let tokens = consume_token(tokens, Token::LeftParen)?;
        let (elms, tokens) = parse_elements(tokens)?;
        let tokens = consume_token(tokens, Token::RightParen)?;

        Ok((Expr::List(elms), tokens))
    }

    fn parse_impl(tokens: &[Token]) -> anyhow::Result<(Expr, &[Token])> {
        parse_list(tokens)
            .or_else(|_| parse_number(tokens))
            .or_else(|_| parse_bool(tokens))
            .or_else(|_| parse_symbol(tokens))
    }

    let (expr, rest) = parse_impl(tokens)?;
    if !rest.is_empty() {
        return Err(anyhow::anyhow!(
            "failed to parse all tokens. remaining: {:?}",
            rest
        ));
    }

    Ok(expr)
}

#[allow(dead_code)]
fn evaluate_impl(expr: Expr, ctx: &Context) -> anyhow::Result<Expr> {
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

#[allow(dead_code)]
fn evaluate(expr: Expr, ctx: &Context) -> anyhow::Result<Result> {
    match evaluate_impl(expr, ctx)? {
        Expr::Number(n) => Ok(Result::Number(n)),
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokenize() {
        assert_eq!(
            vec![
                Token::LeftParen,
                Token::Symbol("*".to_string()),
                Token::LeftParen,
                Token::Symbol("+".to_string()),
                Token::Number(1.25),
                Token::Number(2.0),
                Token::RightParen,
                Token::LeftParen,
                Token::Symbol("-".to_string()),
                Token::Number(1.0),
                Token::Number(2.0),
                Token::RightParen,
                Token::Bool(true),
                Token::Bool(false),
                Token::RightParen,
            ],
            tokenize("(* (+ 1.25 2.0) (- 1.0 2.0) true false)").unwrap()
        );
    }

    #[test]
    fn test_tokenize_empty() {
        assert_eq!(Vec::<Token>::new(), tokenize("").unwrap());
    }

    #[test]
    fn test_parse() {
        assert_eq!(Expr::Number(1.0), parse(&[Token::Number(1.0)]).unwrap());
        assert_eq!(
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
            parse(&[
                Token::LeftParen,
                Token::Symbol("*".to_string()),
                Token::LeftParen,
                Token::Symbol("+".to_string()),
                Token::Number(1.25),
                Token::Number(2.0),
                Token::RightParen,
                Token::LeftParen,
                Token::Symbol("-".to_string()),
                Token::Number(1.0),
                Token::Number(2.0),
                Token::RightParen,
                Token::RightParen,
            ])
            .unwrap()
        );
    }

    #[test]
    fn test_evaluate() {
        let ctx = Context::new();
        assert_eq!(
            Result::Number(-3.25),
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
    }
}
