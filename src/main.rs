use grammar::ProgramParser;
use pl0::{lexer::Lexer, symbol_table::SymbolTableBuilder};

lalrpop_util::lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    grammar
);

fn main() {
    let mut symbols = SymbolTableBuilder::new();
    let source_code = std::fs::read_to_string("pl0_src/scope.pl0").unwrap();
    let lexer = Lexer::new(&source_code);
    let parser = ProgramParser::new();
    let ast = parser.parse(&mut symbols, lexer).unwrap();
    let symbols = symbols.build();

    println!("{:?}", ast);
    println!("{:?}", symbols);
}
