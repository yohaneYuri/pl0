use std::collections::HashMap;

#[derive(Debug)]
pub enum Kind {
    Const(isize),
    Var(isize),
    Procedure,
}

pub type SymbolTable = HashMap<String, (Kind, usize)>;

pub struct SymbolTableBuilder {
    current_scope: usize,
    symbol_table: SymbolTable,
}

impl SymbolTableBuilder {
    pub fn new() -> Self {
        Self {
            current_scope: 0,
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn add(&mut self, name: String, kind: Kind) {
        self.symbol_table.insert(name, (kind, self.current_scope));
    }

    pub fn build(self) -> SymbolTable {
        self.symbol_table
    }

    pub fn enter(&mut self) {
        self.current_scope += 1;
    }

    pub fn leave(&mut self) {
        self.current_scope -= 1;
    }
}
