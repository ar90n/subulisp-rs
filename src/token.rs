#[derive(Debug, PartialEq)]
pub(crate) enum Token {
    LeftParen,
    RightParen,
    Number(f64),
    Bool(bool),
    Symbol(String),
}

fn tokenize_paren(s: &str) -> anyhow::Result<Token> {
    match s {
        "(" => Ok(Token::LeftParen),
        ")" => Ok(Token::RightParen),
        _ => Err(anyhow::anyhow!("failed to parse token as paren")),
    }
}

fn tokenize_number(s: &str) -> anyhow::Result<Token> {
    s.parse::<f64>()
        .map(Token::Number)
        .or(Err(anyhow::anyhow!("failed to parse token as number")))
}

fn tokenize_bool(s: &str) -> anyhow::Result<Token> {
    s.parse::<bool>()
        .map(Token::Bool)
        .or(Err(anyhow::anyhow!("failed to parse token as bool")))
}

fn tokenize_symbol(s: &str) -> anyhow::Result<Token> {
    Ok(Token::Symbol(s.to_string()))
}

fn tokenize_impl(s: &str) -> anyhow::Result<Token> {
    tokenize_paren(s)
        .or_else(|_| tokenize_number(s))
        .or_else(|_| tokenize_bool(s))
        .or_else(|_| tokenize_symbol(s))
        .or(Err(anyhow::anyhow!("failed to parse token: {}", s)))
}

#[allow(dead_code)]
pub(crate) fn tokenize(code: &str) -> anyhow::Result<Vec<Token>> {
    code.replace('(', " ( ")
        .replace(')', " ) ")
        .split_whitespace()
        .map(tokenize_impl)
        .collect()
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
}
