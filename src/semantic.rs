use crate::{
    ast::*,
    symbol_table::{Kind, SymbolTable},
    tac::*,
};

use std::time::{SystemTime, UNIX_EPOCH};

pub struct AstTraverser;

impl AstTraverser {
    pub fn traverse(
        node: &dyn Visit,
        symbol_table: &SymbolTable,
    ) -> Result<Vec<Tac>, SemanticError> {
        Ok(node.visit(symbol_table)?.tacs)
    }
}

pub trait Visit {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult;
}

pub struct VisitReturn {
    tacs: Vec<Tac>,
    attributes: Attributes,
}

pub type VisitResult = Result<VisitReturn, SemanticError>;

#[derive(Debug)]
pub enum SemanticError {
    AssignToConst,
}

impl Visit for Program {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        let mut tacs = vec![Tac::SystemStart];
        let mut visit_result = self.subprogram.visit(symbol_table)?;
        tacs.append(&mut visit_result.tacs);
        tacs.push(Tac::SystemEnd);

        Ok(VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        })
    }
}

impl Visit for Subprogram {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        let mut tacs = vec![];
        if let Some(declaration) = &self.const_declaration {
            let mut result = declaration.visit(symbol_table)?;
            tacs.append(&mut result.tacs);
        }
        if let Some(declaration) = &self.var_declaration {
            let mut result = declaration.visit(symbol_table)?;
            tacs.append(&mut result.tacs);
        }
        if let Some(declaration) = &self.procedure_definition {
            let mut result = declaration.visit(symbol_table)?;
            tacs.append(&mut result.tacs);
        }

        let mut statement_result = self.statement.visit(symbol_table)?;
        tacs.append(&mut statement_result.tacs);

        Ok(VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        })
    }
}

impl Visit for ConstDeclaration {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        let mut tacs = vec![];
        for definition in self.definitions.iter() {
            let mut result = definition.visit(symbol_table)?;
            tacs.append(&mut result.tacs);
        }

        Ok(VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        })
    }
}

impl Visit for ConstDefinition {
    fn visit(&self, _: &SymbolTable) -> VisitResult {
        let define_tac = Tac::Const(self.identifier.clone());
        let assign_tac = Tac::Assign {
            destination: LeftValue::ExistingIdentifier(self.identifier.clone()),
            kind: AssignKind::Const,
            source: RightValue::Literal(self.integer),
        };

        Ok(VisitReturn {
            tacs: vec![define_tac, assign_tac],
            attributes: Attributes::Empty,
        })
    }
}

impl Visit for VarDeclaration {
    fn visit(&self, _: &SymbolTable) -> VisitResult {
        let tacs = self
            .identifiers
            .iter()
            .map(|identifier| Tac::Var(identifier.clone()))
            .collect();

        Ok(VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        })
    }
}

impl Visit for ProcedureDefinition {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        let result = self.header.visit(symbol_table)?;
        let mut tacs = result.tacs;
        tacs.append(&mut self.subprogram.visit(symbol_table)?.tacs);
        tacs.push(Tac::Return);

        for definition in self.definitions.iter() {
            tacs.append(&mut definition.visit(symbol_table)?.tacs);
        }

        Ok(VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        })
    }
}

impl Visit for ProcedureHeader {
    fn visit(&self, _: &SymbolTable) -> VisitResult {
        Ok(VisitReturn {
            tacs: vec![Tac::Procedure(self.identifier.clone())],
            attributes: Attributes::Empty,
        })
    }
}

impl Visit for Statement {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        match self {
            Statement::Assign(statement) => statement.visit(symbol_table),
            Statement::Condition(statement) => statement.visit(symbol_table),
            Statement::Loop(statement) => statement.visit(symbol_table),
            Statement::Call(statement) => statement.visit(symbol_table),
            Statement::Read(statement) => statement.visit(symbol_table),
            Statement::Write(statement) => statement.visit(symbol_table),
            Statement::Compound(statement) => statement.visit(symbol_table),
            Statement::Null => Ok(VisitReturn {
                tacs: vec![],
                attributes: Attributes::Empty,
            }),
        }
    }
}

impl Visit for AssignStatement {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        // Check out symbol's kind, it should be variable, not const
        if let Some((Kind::Const, _)) = symbol_table.get(&self.identifier) {
            return Err(SemanticError::AssignToConst);
        }

        let mut visit_result = self.expression.visit(symbol_table)?;
        visit_result.tacs.push(Tac::Assign {
            destination: LeftValue::ExistingIdentifier(self.identifier.clone()),
            kind: AssignKind::Var,
            source: match visit_result.attributes {
                Attributes::Expression(value) => value,
                _ => unreachable!(),
            },
        });

        Ok(VisitReturn {
            tacs: visit_result.tacs,
            attributes: Attributes::Empty,
        })
    }
}

impl Visit for CompoundStatement {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        let mut tacs = vec![];
        for statement in self.statements.iter() {
            tacs.append(&mut statement.visit(symbol_table)?.tacs);
        }

        Ok(VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        })
    }
}

impl Visit for Condition {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        match self {
            Condition::Binary(left, operator, right) => {
                let left_result = left.visit(symbol_table)?;
                let mut right_result = right.visit(symbol_table)?;
                let mut tacs = left_result.tacs;

                let symbol = timestamped_name("condition");
                let attributes = Attributes::Expression(RightValue::Temporary(symbol.clone()));
                let tac = Tac::CompareOperation {
                    destination: LeftValue::Temporary(symbol),
                    left: match left_result.attributes {
                        Attributes::Expression(value) => value,
                        _ => unreachable!(),
                    },
                    operator: (*operator).into(),
                    right: match right_result.attributes {
                        Attributes::Expression(value) => value,
                        _ => unreachable!(),
                    },
                };

                tacs.append(&mut right_result.tacs);
                tacs.push(tac);

                Ok(VisitReturn { tacs, attributes })
            }
            Condition::Odd(expression) => expression.visit(symbol_table),
        }
    }
}

impl Visit for Expression {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        let mut item_result = self.item.visit(symbol_table)?;
        if Some(Sign::Negative) == self.sign {
            // A = -B => A = 0 - B
            item_result = generate_arithmetic_return(
                "uminus",
                // Fake a return here
                VisitReturn {
                    tacs: vec![],
                    attributes: Attributes::Expression(RightValue::Literal(0)),
                },
                ArithmeticOperator::Sub,
                item_result,
            );
        }

        let mut result = item_result;
        for (operator, item) in self.pairs.iter() {
            result = generate_arithmetic_return(
                "expression",
                result,
                (*operator).into(),
                item.visit(symbol_table)?,
            );
        }

        Ok(result)
    }
}

// No need for visiting Sign

impl Visit for Item {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        let mut result = self.factor.visit(symbol_table)?;
        for (operator, factor) in self.pairs.iter() {
            result = generate_arithmetic_return(
                "item",
                result,
                (*operator).into(),
                factor.visit(symbol_table)?,
            );
        }

        Ok(result)
    }
}

impl Visit for Factor {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        match self {
            Factor::Identifier(name) => Ok(VisitReturn {
                tacs: vec![],
                attributes: Attributes::Expression(RightValue::ExistingIdentifier(name.clone())),
            }),
            Factor::Integer(value) => Ok(VisitReturn {
                tacs: vec![],
                attributes: Attributes::Expression(RightValue::Literal(*value)),
            }),
            Factor::Expression(expression) => expression.visit(symbol_table),
        }
    }
}

// No need for visiting AddSubOperator

// No need for visiting MulDivOperator

// No need for visiting RelationalOperator

impl Visit for ConditionStatement {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        let label = timestamped_name("next");
        let condition_result = self.condition.visit(symbol_table)?;
        let mut tacs = condition_result.tacs;
        let jump_tac = Tac::Jump {
            target: label.clone(),
            expression: match condition_result.attributes {
                Attributes::Expression(value) => value,
                _ => unreachable!(),
            },
        };
        let mut statement_result = self.statement.visit(symbol_table)?;
        let label_tac = Tac::Label(label);

        tacs.push(jump_tac);
        tacs.append(&mut statement_result.tacs);
        tacs.push(label_tac);

        Ok(VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        })
    }
}

impl Visit for CallStatement {
    fn visit(&self, _: &SymbolTable) -> VisitResult {
        Ok(VisitReturn {
            tacs: vec![Tac::Call(self.procedure_identifier.clone())],
            attributes: Attributes::Empty,
        })
    }
}

impl Visit for LoopStatement {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        let loop_label = timestamped_name("loop");
        let end_label = timestamped_name("loop_end");
        let condition_result = self.condition.visit(symbol_table)?;
        let check_loop_condition_tac = Tac::Jump {
            target: end_label.clone(),
            expression: match condition_result.attributes {
                Attributes::Expression(value) => value,
                _ => unreachable!(),
            },
        };
        let mut statement_tacs = self.statement.visit(symbol_table)?.tacs;
        let next_loop_tac = Tac::Jump {
            target: loop_label.clone(),
            expression: RightValue::Literal(1),
        };

        let mut tacs = condition_result.tacs;
        tacs.push(Tac::Label(loop_label.clone()));
        tacs.push(check_loop_condition_tac);
        tacs.append(&mut statement_tacs);
        tacs.push(next_loop_tac);
        tacs.push(Tac::Label(end_label));

        Ok(VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        })
    }
}

impl Visit for ReadStatement {
    fn visit(&self, _: &SymbolTable) -> VisitResult {
        let tacs = self
            .identifiers
            .iter()
            .map(|identifier| Tac::Read(identifier.clone()))
            .collect();

        Ok(VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        })
    }
}

impl Visit for WriteStatement {
    fn visit(&self, symbol_table: &SymbolTable) -> VisitResult {
        let mut tacs = vec![];
        for expression in self.expressions.iter() {
            let mut result = expression.visit(symbol_table)?;
            tacs.append(&mut result.tacs);
            tacs.push(Tac::Write(match result.attributes {
                Attributes::Expression(value) => value,
                _ => unreachable!(),
            }));
        }

        Ok(VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        })
    }
}

fn timestamped_name(prefix: &str) -> String {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    format!("{prefix}{timestamp}")
}

fn generate_arithmetic_return(
    temporary_var_prefix: &str,
    left_result: VisitReturn,
    operator: ArithmeticOperator,
    right_result: VisitReturn,
) -> VisitReturn {
    let symbol = timestamped_name(temporary_var_prefix);
    let attributes = Attributes::Expression(RightValue::Temporary(symbol.clone()));
    let mut tacs = left_result.tacs;
    let mut right_tacs = right_result.tacs;

    let tac = Tac::ArithmeticOperation {
        destination: LeftValue::Temporary(symbol),
        left: match left_result.attributes {
            Attributes::Expression(value) => value,
            _ => unreachable!(),
        },
        operator,
        right: match right_result.attributes {
            Attributes::Expression(value) => value,
            _ => unreachable!(),
        },
    };

    tacs.append(&mut right_tacs);
    tacs.push(tac);

    VisitReturn { tacs, attributes }
}
