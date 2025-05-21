use grammar::ProgramParser;
use pl0::lexer::Lexer;

lalrpop_util::lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    grammar
);

fn main() {
    let source_code = std::fs::read_to_string("pl0_src/case_0.pl0").unwrap();
    let lexer = Lexer::new(&source_code);
    let parser = ProgramParser::new();
    let ast = parser.parse(lexer).unwrap();

    println!("{:?}", ast);
}
