pub enum Tac {
    SystemStart,
    SystemEnd,
    Const(String),
    Var(String),
    Procedure(String),
    ArithmeticOperation {
        destination: LeftValue,
        left: RightValue,
        operator: ArithmeticOperator,
        right: RightValue,
    },
    CompareOperation {
        destination: LeftValue,
        left: RightValue,
        operator: CompareOperator,
        right: RightValue,
    },
    Assign {
        destination: LeftValue,
        kind: AssignKind,
        source: RightValue,
    },
    Jump {
        target: usize,
        left: RightValue,
        right: RightValue,
    },
    Read(String),
    Write(RightValue),
    CallWithReture {
        procedure: String,
        return_value_place: LeftValue,
    },
    Call(String),
    ReturnValue(RightValue),
    Return,
}

pub struct TemporaryVar(pub isize);

pub enum LeftValue {
    Temporary(TemporaryVar),
    Var(String),
}

pub enum RightValue {
    Temporary(TemporaryVar),
    Const(String),
    Var(String),
}

pub enum ArithmeticOperator {
    Add,
    Sub,
    Mul,
    Div,
}

pub enum CompareOperator {
    LessThan,
    LessEqualThan,
    GreaterThan,
    GreaterEqualThan,
    NotEqual,
    Equal,
}

pub enum AssignKind {
    Const,
    Var,
}
