use std::env;

use pl0::compiler::Compiler;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = args.iter().nth(1);

    if let Some(file) = file {
        let compile_result = Compiler::compile(file);
        match compile_result {
            Ok(tacs) => tacs.iter().for_each(|tac| println!("{tac}")),
            Err(errs) => errs.iter().for_each(|err| println!("{:?}", err)),
        }
    }
}
