#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub subprogram: Box<Subprogram>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Subprogram {
    pub const_declaration: Option<ConstDeclaration>,
    pub var_declaration: Option<VarDeclaration>,
    pub procedure_definition: Option<Box<ProcedureDefinition>>,
    pub statement: Box<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstDeclaration {
    pub definitions: Vec<ConstDefinition>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstDefinition {
    pub identifier: String,
    pub integer: isize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDeclaration {
    pub identifiers: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProcedureDefinition {
    pub header: ProcedureHeader,
    pub subprogram: Box<Subprogram>,
    pub definitions: Vec<Box<ProcedureDefinition>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProcedureHeader {
    pub identifier: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Assign(AssignStatement),
    Condition(ConditionStatement),
    Loop(LoopStatement),
    Call(CallStatement),
    Read(ReadStatement),
    Write(WriteStatement),
    Compound(CompoundStatement),
    Null,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AssignStatement {
    pub identifier: String,
    pub expression: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompoundStatement {
    pub statements: Vec<Box<Statement>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Condition {
    Binary(Box<Expression>, RelationalOperator, Box<Expression>),
    Odd(Box<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expression {
    pub sign: Option<Sign>,
    pub item: Item,
    pub pairs: Vec<(AddSubOperator, Item)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Sign {
    Positive,
    Negative,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    pub factor: Factor,
    pub pairs: Vec<(MulDivOperator, Factor)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Factor {
    Identifier(String),
    Integer(isize),
    Expression(Box<Expression>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AddSubOperator {
    Add,
    Sub,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MulDivOperator {
    Mul,
    Div,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RelationalOperator {
    Equal,
    NotEqual,
    LessThan,
    LessEqualThan,
    GreaterThan,
    GreaterEqualThan,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConditionStatement {
    pub condition: Condition,
    pub statement: Box<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallStatement {
    pub procedure_identifier: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoopStatement {
    pub condition: Condition,
    pub statement: Box<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReadStatement {
    pub identifiers: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WriteStatement {
    pub expressions: Vec<Box<Expression>>,
}
