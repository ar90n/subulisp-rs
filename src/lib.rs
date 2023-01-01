use std::fmt::Debug;

#[derive(Debug, PartialEq)]
enum Token {
    LeftParen,
    RightParen,
    Number(f64),
    Symbol(String),
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
}
