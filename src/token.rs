use std::{fmt, num::ParseIntError};

use logos::{Lexer, Logos};

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub enum LexicalError {
    InvalidInteger,
    InvalidIdentifier,
    #[default]
    InvalidToken,
}

impl From<ParseIntError> for LexicalError {
    fn from(_: ParseIntError) -> Self {
        Self::InvalidInteger
    }
}

fn produce_identifier_error(_: &mut Lexer<Token>) -> Result<(), LexicalError> {
    Err(LexicalError::InvalidIdentifier)
}

#[derive(Logos, PartialEq, Debug, Clone)]
#[logos(error = LexicalError)]
#[logos(skip r"\s+")]
// Skip comments
#[logos(skip "//[^\r\n|\n|\r]*")]
#[logos(skip r"/\*[^\*/]*\*/")]
pub enum Token {
    // Keyword
    #[token("var")]
    Var,
    #[token("const")]
    Const,
    #[token("procedure")]
    Procedure,
    #[token("begin")]
    Begin,
    #[token("end")]
    End,
    #[token("odd")]
    Odd,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("call")]
    Call,
    #[token("while")]
    While,
    #[token("do")]
    Do,
    #[token("read")]
    Read,
    #[token("write")]
    Write,
    // Delimiter
    #[token(";")]
    Semicolon,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    // Operator
    #[token(":=")]
    ColonEqual,
    #[token("=")]
    Equal,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("(")]
    LeftParenthese,
    #[token(")")]
    RightParenthese,
    #[token("#")]
    Number,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessEqualThan,
    #[token(">=")]
    GreaterEqualThan,
    // Identifier
    #[regex(r"\d+\w*[a-zA-Z_]\w*", produce_identifier_error)]
    InvalidIdentifier,
    #[regex(r"[a-zA-Z_]\w*", |lex| lex.slice().to_owned())]
    Identifier(String),
    // Literal
    #[regex(r"\d+", |lex| lex.slice().parse())]
    Integer(isize),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let mut lex = Token::lexer("do while read call procedure");

        assert_eq!(lex.next(), Some(Ok(Token::Do)));
        assert_eq!(lex.next(), Some(Ok(Token::While)));
        assert_eq!(lex.next(), Some(Ok(Token::Read)));
        assert_eq!(lex.next(), Some(Ok(Token::Call)));
        assert_eq!(lex.next(), Some(Ok(Token::Procedure)));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_identifiers() {
        let mut lex = Token::lexer("_isekai frills var_0 _ 1c1xx");

        assert_eq!(
            lex.next(),
            Some(Ok(Token::Identifier("_isekai".to_owned())))
        );
        assert_eq!(lex.next(), Some(Ok(Token::Identifier("frills".to_owned()))));
        assert_eq!(lex.next(), Some(Ok(Token::Identifier("var_0".to_owned()))));
        assert_eq!(lex.next(), Some(Ok(Token::Identifier("_".to_owned()))));
        assert_eq!(lex.next(), Some(Err(LexicalError::InvalidIdentifier)));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_integer() {
        let mut lex = Token::lexer("123 51 520 326");

        assert_eq!(lex.next(), Some(Ok(Token::Integer(123))));
        assert_eq!(lex.next(), Some(Ok(Token::Integer(51))));
        assert_eq!(lex.next(), Some(Ok(Token::Integer(520))));
        assert_eq!(lex.next(), Some(Ok(Token::Integer(326))));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_symbol() {
        let mut lex = Token::lexer(r". ; <= > := =");

        assert_eq!(lex.next(), Some(Ok(Token::Dot)));
        assert_eq!(lex.next(), Some(Ok(Token::Semicolon)));
        assert_eq!(lex.next(), Some(Ok(Token::LessEqualThan)));
        assert_eq!(lex.next(), Some(Ok(Token::GreaterThan)));
        assert_eq!(lex.next(), Some(Ok(Token::ColonEqual)));
        assert_eq!(lex.next(), Some(Ok(Token::Equal)));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_comment() {
        let mut lex = Token::lexer("// discarded \n/*hello\nworld*/ abc = 1;");

        assert_eq!(lex.next(), Some(Ok(Token::Identifier("abc".to_owned()))));
        assert_eq!(lex.next(), Some(Ok(Token::Equal)));
        assert_eq!(lex.next(), Some(Ok(Token::Integer(1))));
        assert_eq!(lex.next(), Some(Ok(Token::Semicolon)));
        assert_eq!(lex.next(), None);
    }
}
