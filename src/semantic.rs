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
    ) -> Result<Vec<Tac>, Vec<SemanticError>> {
        let mut errors = Vec::new();
        let result = node.visit(symbol_table, &mut errors);

        if errors.is_empty() {
            Ok(result.tacs)
        } else {
            Err(errors)
        }
    }
}

pub trait Visit {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn;
}

pub struct VisitReturn {
    tacs: Vec<Tac>,
    attributes: Attributes,
}

#[derive(Debug)]
pub enum SemanticError {
    AssignToConst,
}

impl Visit for Program {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        let mut tacs = vec![Tac::SystemStart];
        let mut visit_result = self.subprogram.visit(symbol_table, semantic_errors);
        tacs.append(&mut visit_result.tacs);
        tacs.push(Tac::SystemEnd);

        VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        }
    }
}

impl Visit for Subprogram {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        let mut tacs = Vec::new();
        if let Some(declaration) = &self.const_declaration {
            let mut result = declaration.visit(symbol_table, semantic_errors);
            tacs.append(&mut result.tacs);
        }
        if let Some(declaration) = &self.var_declaration {
            let mut result = declaration.visit(symbol_table, semantic_errors);
            tacs.append(&mut result.tacs);
        }
        if let Some(declaration) = &self.procedure_definition {
            let mut result = declaration.visit(symbol_table, semantic_errors);
            tacs.append(&mut result.tacs);
        }

        let mut statement_result = self.statement.visit(symbol_table, semantic_errors);
        tacs.append(&mut statement_result.tacs);

        VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        }
    }
}

impl Visit for ConstDeclaration {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        let mut tacs = Vec::new();
        for definition in self.definitions.iter() {
            let mut result = definition.visit(symbol_table, semantic_errors);
            tacs.append(&mut result.tacs);
        }

        VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        }
    }
}

impl Visit for ConstDefinition {
    fn visit(&self, _: &SymbolTable, _: &mut Vec<SemanticError>) -> VisitReturn {
        let define_tac = Tac::Const(self.identifier.clone());
        let assign_tac = Tac::Assign {
            destination: LeftValue::ExistingIdentifier(self.identifier.clone()),
            kind: AssignKind::Const,
            source: RightValue::Literal(self.integer),
        };

        VisitReturn {
            tacs: vec![define_tac, assign_tac],
            attributes: Attributes::Empty,
        }
    }
}

impl Visit for VarDeclaration {
    fn visit(&self, _: &SymbolTable, _: &mut Vec<SemanticError>) -> VisitReturn {
        let tacs = self
            .identifiers
            .iter()
            .map(|identifier| Tac::Var(identifier.clone()))
            .collect();

        VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        }
    }
}

impl Visit for ProcedureDefinition {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        let result = self.header.visit(symbol_table, semantic_errors);
        let mut tacs = result.tacs;
        tacs.append(&mut self.subprogram.visit(symbol_table, semantic_errors).tacs);
        tacs.push(Tac::Return);

        for definition in self.definitions.iter() {
            tacs.append(&mut definition.visit(symbol_table, semantic_errors).tacs);
        }

        VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        }
    }
}

impl Visit for ProcedureHeader {
    fn visit(&self, _: &SymbolTable, _: &mut Vec<SemanticError>) -> VisitReturn {
        VisitReturn {
            tacs: vec![Tac::Procedure(self.identifier.clone())],
            attributes: Attributes::Empty,
        }
    }
}

impl Visit for Statement {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        match self {
            Statement::Assign(statement) => statement.visit(symbol_table, semantic_errors),
            Statement::Condition(statement) => statement.visit(symbol_table, semantic_errors),
            Statement::Loop(statement) => statement.visit(symbol_table, semantic_errors),
            Statement::Call(statement) => statement.visit(symbol_table, semantic_errors),
            Statement::Read(statement) => statement.visit(symbol_table, semantic_errors),
            Statement::Write(statement) => statement.visit(symbol_table, semantic_errors),
            Statement::Compound(statement) => statement.visit(symbol_table, semantic_errors),
            Statement::Null | Statement::Error => VisitReturn {
                tacs: Vec::new(),
                attributes: Attributes::Empty,
            },
        }
    }
}

impl Visit for AssignStatement {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        // Check out symbol's kind, it should be variable, not const
        if let Some((Kind::Const, _)) = symbol_table.get(&self.identifier) {
            semantic_errors.push(SemanticError::AssignToConst);
        }

        let mut visit_result = self.expression.visit(symbol_table, semantic_errors);
        visit_result.tacs.push(Tac::Assign {
            destination: LeftValue::ExistingIdentifier(self.identifier.clone()),
            kind: AssignKind::Var,
            source: match visit_result.attributes {
                Attributes::Expression(value) => value,
                _ => unreachable!(),
            },
        });

        VisitReturn {
            tacs: visit_result.tacs,
            attributes: Attributes::Empty,
        }
    }
}

impl Visit for CompoundStatement {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        let mut tacs = Vec::new();
        for statement in self.statements.iter() {
            tacs.append(&mut statement.visit(symbol_table, semantic_errors).tacs);
        }

        VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        }
    }
}

impl Visit for Condition {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        match self {
            Condition::Binary(left, operator, right) => {
                let left_result = left.visit(symbol_table, semantic_errors);
                let mut right_result = right.visit(symbol_table, semantic_errors);
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

                VisitReturn { tacs, attributes }
            }
            Condition::Odd(expression) => expression.visit(symbol_table, semantic_errors),
        }
    }
}

impl Visit for Expression {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        match self {
            Self::Valid { item, sign, pairs } => {
                let mut item_result = item.visit(symbol_table, semantic_errors);
                if Some(Sign::Negative) == *sign {
                    // A = -B => A = 0 - B
                    item_result = generate_arithmetic_return(
                        "uminus",
                        // Fake a return here
                        VisitReturn {
                            tacs: Vec::new(),
                            attributes: Attributes::Expression(RightValue::Literal(0)),
                        },
                        ArithmeticOperator::Sub,
                        item_result,
                    );
                }

                let mut result = item_result;
                for (operator, item) in pairs.iter() {
                    result = generate_arithmetic_return(
                        "expression",
                        result,
                        (*operator).into(),
                        item.visit(symbol_table, semantic_errors),
                    );
                }

                result
            }
            Self::Invalid => VisitReturn {
                tacs: Vec::new(),
                attributes: Attributes::Empty,
            },
        }
    }
}

// No need for visiting Sign

impl Visit for Item {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        let mut result = self.factor.visit(symbol_table, semantic_errors);
        for (operator, factor) in self.pairs.iter() {
            result = generate_arithmetic_return(
                "item",
                result,
                (*operator).into(),
                factor.visit(symbol_table, semantic_errors),
            );
        }

        result
    }
}

impl Visit for Factor {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        match self {
            Factor::Identifier(name) => VisitReturn {
                tacs: Vec::new(),
                attributes: Attributes::Expression(RightValue::ExistingIdentifier(name.clone())),
            },
            Factor::Integer(value) => VisitReturn {
                tacs: Vec::new(),
                attributes: Attributes::Expression(RightValue::Literal(*value)),
            },
            Factor::Expression(expression) => expression.visit(symbol_table, semantic_errors),
        }
    }
}

// No need for visiting AddSubOperator

// No need for visiting MulDivOperator

// No need for visiting RelationalOperator

impl Visit for ConditionStatement {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        let label = timestamped_name("next");
        let condition_result = self.condition.visit(symbol_table, semantic_errors);
        let mut tacs = condition_result.tacs;
        let jump_tac = Tac::Jump {
            target: label.clone(),
            expression: match condition_result.attributes {
                Attributes::Expression(value) => value,
                _ => unreachable!(),
            },
        };
        let mut statement_result = self.statement.visit(symbol_table, semantic_errors);
        let label_tac = Tac::Label(label);

        tacs.push(jump_tac);
        tacs.append(&mut statement_result.tacs);
        tacs.push(label_tac);

        VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        }
    }
}

impl Visit for CallStatement {
    fn visit(&self, _: &SymbolTable, _: &mut Vec<SemanticError>) -> VisitReturn {
        VisitReturn {
            tacs: vec![Tac::Call(self.procedure_identifier.clone())],
            attributes: Attributes::Empty,
        }
    }
}

impl Visit for LoopStatement {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        let loop_label = timestamped_name("loop");
        let end_label = timestamped_name("loop_end");
        let condition_result = self.condition.visit(symbol_table, semantic_errors);
        let check_loop_condition_tac = Tac::Jump {
            target: end_label.clone(),
            expression: match condition_result.attributes {
                Attributes::Expression(value) => value,
                _ => unreachable!(),
            },
        };
        let mut statement_tacs = self.statement.visit(symbol_table, semantic_errors).tacs;
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

        VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        }
    }
}

impl Visit for ReadStatement {
    fn visit(&self, _: &SymbolTable, _: &mut Vec<SemanticError>) -> VisitReturn {
        let tacs = self
            .identifiers
            .iter()
            .map(|identifier| Tac::Read(identifier.clone()))
            .collect();

        VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        }
    }
}

impl Visit for WriteStatement {
    fn visit(
        &self,
        symbol_table: &SymbolTable,
        semantic_errors: &mut Vec<SemanticError>,
    ) -> VisitReturn {
        let mut tacs = Vec::new();
        for expression in self.expressions.iter() {
            let mut result = expression.visit(symbol_table, semantic_errors);
            tacs.append(&mut result.tacs);
            tacs.push(Tac::Write(match result.attributes {
                Attributes::Expression(value) => value,
                _ => unreachable!(),
            }));
        }

        VisitReturn {
            tacs,
            attributes: Attributes::Empty,
        }
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
