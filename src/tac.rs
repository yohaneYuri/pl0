use std::fmt;

use crate::ast::{AddSubOperator, MulDivOperator, RelationalOperator};

#[derive(Debug)]
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
    // Jump if expression is true
    Jump {
        target: String,
        expression: RightValue,
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
    Label(String),
}

impl fmt::Display for Tac {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tac = match self {
            Self::SystemStart => "syss, _, _, _".to_owned(),
            Self::SystemEnd => "syse, _, _, _".to_owned(),
            Self::Const(name) => format!("const, {name}, _, _"),
            Self::Var(name) => format!("var, {name}, _, _"),
            Self::Procedure(name) => format!("procedure, {name}, _, _"),
            Self::ArithmeticOperation {
                destination,
                left,
                operator,
                right,
            } => format!("{operator}, {left}, {right}, {destination}"),
            Self::CompareOperation {
                destination,
                left,
                operator,
                right,
            } => format!("{operator}, {left}, {right}, {destination}"),
            Self::Assign {
                destination,
                kind,
                source,
            } => format!(
                "{}, {source}, _, {destination}",
                match kind {
                    AssignKind::Const => "=",
                    AssignKind::Var => ":=",
                }
            ),
            Self::Jump { target, expression } => format!("jump, {expression}, _, {target}"),
            Self::Read(name) => format!("read, {name}, _, _"),
            Self::Write(value) => format!("write, {value}, _, _"),
            Self::CallWithReture {
                procedure,
                return_value_place,
            } => format!("call, {procedure}, {return_value_place}, _"),
            Self::Call(name) => format!("call, {name}, _, _"),
            Self::ReturnValue(value) => format!("ret, {value}, _ _"),
            Self::Return => "ret, _, _, _".to_owned(),
            Self::Label(name) => format!("label, _, _, {name}"),
        };

        write!(f, "({})", tac)
    }
}

#[derive(Debug)]
pub enum LeftValue {
    Temporary(String),
    ExistingIdentifier(String),
}

impl fmt::Display for LeftValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Temporary(name) | Self::ExistingIdentifier(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug)]
pub enum RightValue {
    Temporary(String),
    ExistingIdentifier(String),
    Literal(isize),
}

impl fmt::Display for RightValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Temporary(name) | Self::ExistingIdentifier(name) => write!(f, "{}", name),
            Self::Literal(int) => write!(f, "{}", int),
        }
    }
}

#[derive(Debug)]
pub enum ArithmeticOperator {
    Add,
    Sub,
    Mul,
    Div,
}

impl From<MulDivOperator> for ArithmeticOperator {
    fn from(value: MulDivOperator) -> Self {
        match value {
            MulDivOperator::Mul => Self::Mul,
            MulDivOperator::Div => Self::Div,
        }
    }
}

impl From<AddSubOperator> for ArithmeticOperator {
    fn from(value: AddSubOperator) -> Self {
        match value {
            AddSubOperator::Sub => Self::Sub,
            AddSubOperator::Add => Self::Add,
        }
    }
}

impl fmt::Display for ArithmeticOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Add => "+",
                Self::Sub => "-",
                Self::Mul => "*",
                Self::Div => "/",
            }
        )
    }
}

#[derive(Debug)]
pub enum CompareOperator {
    LessThan,
    LessEqualThan,
    GreaterThan,
    GreaterEqualThan,
    NotEqual,
    Equal,
}

impl fmt::Display for CompareOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::LessThan => "<",
                Self::LessEqualThan => "<=",
                Self::GreaterThan => ">",
                Self::GreaterEqualThan => ">=",
                Self::NotEqual => "#",
                Self::Equal => "=",
            }
        )
    }
}

impl From<RelationalOperator> for CompareOperator {
    fn from(value: RelationalOperator) -> Self {
        match value {
            RelationalOperator::Equal => Self::Equal,
            RelationalOperator::NotEqual => Self::NotEqual,
            RelationalOperator::LessThan => Self::LessThan,
            RelationalOperator::LessEqualThan => Self::LessEqualThan,
            RelationalOperator::GreaterThan => Self::GreaterThan,
            RelationalOperator::GreaterEqualThan => Self::GreaterEqualThan,
        }
    }
}

#[derive(Debug)]
pub enum AssignKind {
    Const,
    Var,
}

pub enum Attributes {
    Expression(RightValue),
    Empty,
}
