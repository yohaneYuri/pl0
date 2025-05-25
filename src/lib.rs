pub mod ast;
pub mod compiler;
pub mod lexer;
pub mod ll;
pub mod semantic;
pub mod symbol_table;
pub mod tac;
pub mod token;

lalrpop_util::lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    grammar
);
