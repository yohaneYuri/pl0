use std::num::ParseIntError;

use logos::Logos;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexingError {
    InvalidInteger,
    #[default]
    Other,
}

impl From<ParseIntError> for LexingError {
    fn from(_: ParseIntError) -> Self {
        Self::InvalidInteger
    }
}

#[derive(Logos, PartialEq, Debug)]
#[logos(error = LexingError)]
#[logos(skip r"\s+")]
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
    #[token("==")]
    DoubleEqual,
    #[token("<=")]
    LessEqualThan,
    #[token(">=")]
    GreaterEqualThan,
    // Identifier
    #[regex(r"\d+\w*[a-zA-Z_]\w*")]
    InvalidIdentifier,
    #[regex(r"[a-zA-Z_]\w*", |lex| lex.slice().to_owned())]
    Identifier(String),
    // Literal
    #[regex(r"\d+", |lex| lex.slice().parse())]
    Integer(isize),
    #[regex(r#"\"[^\"]*\""#, |lex| lex.slice().trim_matches('"').to_owned())]
    String(String),
    // Comment
    #[regex("//[^\r\n|\n|\r]*")]
    SingleLineComment,
    #[regex(r"/\*[^\*/]*\*/")]
    MultiLineComment,
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
        assert_eq!(lex.next(), Some(Ok(Token::InvalidIdentifier)));
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
    fn test_string() {
        let mut lex = Token::lexer(r#" "c" "c++" "python" "java" "#);

        assert_eq!(lex.next(), Some(Ok(Token::String("c".to_owned()))));
        assert_eq!(lex.next(), Some(Ok(Token::String("c++".to_owned()))));
        assert_eq!(lex.next(), Some(Ok(Token::String("python".to_owned()))));
        assert_eq!(lex.next(), Some(Ok(Token::String("java".to_owned()))));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_symbol() {
        let mut lex = Token::lexer(r". ; <= > := == =");

        assert_eq!(lex.next(), Some(Ok(Token::Dot)));
        assert_eq!(lex.next(), Some(Ok(Token::Semicolon)));
        assert_eq!(lex.next(), Some(Ok(Token::LessEqualThan)));
        assert_eq!(lex.next(), Some(Ok(Token::GreaterThan)));
        assert_eq!(lex.next(), Some(Ok(Token::ColonEqual)));
        assert_eq!(lex.next(), Some(Ok(Token::DoubleEqual)));
        assert_eq!(lex.next(), Some(Ok(Token::Equal)));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_comment() {
        let mut lex = Token::lexer("// discarded \n/*hello\nworld*/ abc = 1;");

        assert_eq!(lex.next(), Some(Ok(Token::SingleLineComment)));
        assert_eq!(lex.next(), Some(Ok(Token::MultiLineComment)));
        assert_eq!(lex.next(), Some(Ok(Token::Identifier("abc".to_owned()))));
        assert_eq!(lex.next(), Some(Ok(Token::Equal)));
        assert_eq!(lex.next(), Some(Ok(Token::Integer(1))));
        assert_eq!(lex.next(), Some(Ok(Token::Semicolon)));
        assert_eq!(lex.next(), None);
    }
}
