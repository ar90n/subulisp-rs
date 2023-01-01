use std::fmt::Debug;

#[derive(Debug, PartialEq)]
enum Token {
    LeftParen,
    RightParen,
    Number(f64),
    Symbol(String),
}

#[derive(Debug, PartialEq)]
enum Expr {
    Number(f64),
    Symbol(String),
    List(Vec<Expr>),
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

    fn parse_symbol(s: &str) -> anyhow::Result<Token> {
        Ok(Token::Symbol(s.to_string()))
    }

    fn parse(s: &str) -> anyhow::Result<Token> {
        parse_paren(s)
            .or_else(|_| parse_number(s))
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
                Token::RightParen,
            ],
            tokenize("(* (+ 1.25 2.0) (- 1.0 2.0))").unwrap()
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
}
