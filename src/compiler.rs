use lalrpop_util::{ErrorRecovery, ParseError};

use crate::{
    grammar::ProgramParser,
    lexer::Lexer,
    semantic::{AstTraverser, SemanticError},
    symbol_table::SymbolTableBuilder,
    tac::Tac,
    token::{LexicalError, Token},
};

pub struct Compiler;

impl Compiler {
    pub fn compile(source: &str) -> Result<Vec<Tac>, CompileError> {
        let mut symbols = SymbolTableBuilder::new();
        let mut syntax_errors = Vec::new();

        let source_code = std::fs::read_to_string(source).unwrap();
        let lexer = Lexer::new(&source_code);
        let parser = ProgramParser::new();
        let parse_result = parser.parse(&mut symbols, &mut syntax_errors, lexer);
        if let Err(err) = parse_result {
            return Err(CompileError::Lexical(err));
        }
        if !syntax_errors.is_empty() {
            return Err(CompileError::Syntax(syntax_errors));
        }

        let ast = parse_result.unwrap();
        let symbols = symbols.build();
        AstTraverser::traverse(&ast, &symbols).map_err(|err| CompileError::Semantic(err))
    }
}

pub enum CompileError {
    Lexical(ParseError<usize, Token, LexicalError>),
    Syntax(Vec<ErrorRecovery<usize, Token, LexicalError>>),
    Semantic(Vec<SemanticError>),
}
