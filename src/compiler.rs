use lalrpop_util::ErrorRecovery;

use crate::{
    grammar::ProgramParser,
    lexer::Lexer,
    semantic::AstTraverser,
    symbol_table::SymbolTableBuilder,
    tac::Tac,
    token::{LexicalError, Token},
};

pub struct Compiler;

impl Compiler {
    pub fn compile(
        source: &str,
    ) -> Result<Vec<Tac>, Vec<ErrorRecovery<usize, Token, LexicalError>>> {
        let mut symbols = SymbolTableBuilder::new();
        let mut errors = Vec::new();

        let source_code = std::fs::read_to_string(source).unwrap();
        let lexer = Lexer::new(&source_code);
        let parser = ProgramParser::new();
        let ast = parser.parse(&mut symbols, &mut errors, lexer).unwrap();
        if !errors.is_empty() {
            return Err(errors);
        }
        let symbols = symbols.build();
        let tacs = AstTraverser::traverse(&ast, &symbols).unwrap();

        Ok(tacs)
    }
}
