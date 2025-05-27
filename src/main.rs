use std::env;

use pl0::compiler::{CompileError, Compiler};

// TODO clone everywhere, bad performance, use reference instead

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = args.get(1);
    let use_ll = args.get(2).map_or(false, |string| string.parse().unwrap());

    if !use_ll {
        if let Some(file) = file {
            let compile_result = Compiler::compile(file);
            match compile_result {
                Ok((tacs, symbols)) => {
                    println!("==== TACs ====");
                    tacs.iter().for_each(|tac| println!("{tac}"));
                    println!("\n==== Symbols ====");
                    symbols.iter().for_each(|symbol| println!("{:?}", symbol));
                }
                Err(errs) => {
                    println!("==== Compile Errors ====");
                    match errs {
                        CompileError::Lexical(err) => println!("{:?}", err),
                        CompileError::Syntax(errs) => {
                            errs.iter().for_each(|err| println!("{:?}", err))
                        }
                        CompileError::Semantic(errs) => {
                            errs.iter().for_each(|err| println!("{:?}", err))
                        }
                    }
                }
            }

            return;
        }
    }

    use pl0::lexer::Lexer;
    use pl0::ll::Ll1Parser;
    use pl0::semantic::AstTraverser;

    if let Some(file) = file {
        let source_code = std::fs::read_to_string(file).unwrap();
        let lexer = Lexer::new(&source_code);

        let parser = Ll1Parser::new(lexer);
        let parse_result = parser.parse();
        if parse_result.is_err() {
            println!("==== Compile failed ====");
            println!("{:?}", parse_result);
            return;
        }

        let (ast, symbols) = parse_result.unwrap();
        AstTraverser::traverse(&ast, &symbols)
            .unwrap()
            .iter()
            .for_each(|tac| println!("{tac}"));
    }
}
