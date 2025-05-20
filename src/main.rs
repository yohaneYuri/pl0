use logos::Logos;
use pl0::token::Token;

fn main() {
    println!("Hello, world!");

    let mut lex = Token::lexer(include_str!("../pl0_src/case_0.pl0"));
    while let Some(token) = lex.next() {
        match token {
            Ok(token) => println!("{:?}", token),
            Err(err) => println!("{:?}, {}", err, lex.slice()),
        }
    }
}
