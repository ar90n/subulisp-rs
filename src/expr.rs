use super::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Expr {
    Number(f64),
    Bool(bool),
    Symbol(String),
    List(Vec<Expr>),
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

#[cfg(test)]
mod test {
    use super::*;

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
