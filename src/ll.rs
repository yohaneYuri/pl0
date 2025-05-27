use crate::{
    ast::*,
    lexer::Lexer,
    symbol_table::{SymbolTable, SymbolTableBuilder},
    token::{LexicalError, Token},
};

pub struct Ll1Parser<'input> {
    lexer: Lexer<'input>,
    syntax_errors: Vec<SyntaxError>,
    symbols: SymbolTableBuilder,
    lookahead: Option<Result<Token, LexicalError>>,
}

#[derive(Debug)]
pub enum ParseError {
    ShouldPanic(PanicError),
    Syntax(Vec<SyntaxError>),
}

#[derive(Debug)]
pub struct SyntaxError;

#[derive(Debug)]
pub enum UnexpectedBehaviorWhileParsing {
    UnexpectedNormalToken,
    Panic(PanicError),
}

#[derive(Debug)]
pub enum PanicError {
    UnexpectedEof,
    Lexical(LexicalError),
}

impl From<PanicError> for UnexpectedBehaviorWhileParsing {
    fn from(value: PanicError) -> Self {
        UnexpectedBehaviorWhileParsing::Panic(value)
    }
}

impl<'input> Ll1Parser<'input> {
    pub fn new(lexer: Lexer<'input>) -> Self {
        let errors = Vec::new();
        let symbols = SymbolTableBuilder::new();
        let mut lexer = lexer;
        let first_token = lexer.next().map(|result| result.map(|(_, token, _)| token));
        println!("[debug] first token is {:?}", first_token);

        Self {
            lexer,
            syntax_errors: errors,
            symbols,
            lookahead: first_token,
        }
    }

    pub fn parse(mut self) -> Result<(Program, SymbolTable), ParseError> {
        let parse_result = self.parse_program().map(|ast| (ast, self.symbols.build()));

        if parse_result.is_ok() && !self.syntax_errors.is_empty() {
            Err(ParseError::Syntax(self.syntax_errors))
        } else {
            parse_result.map_err(|err| match err {
                UnexpectedBehaviorWhileParsing::Panic(err) => ParseError::ShouldPanic(err),
                _ => unreachable!(),
            })
        }
    }

    #[inline]
    fn advance(&mut self) {
        self.lookahead = self
            .lexer
            .next()
            .map(|result| result.map(|(_, token, _)| token));
        println!("[debug] next token is {:?}", self.lookahead);
    }

    #[inline]
    fn match_token(&self, token: Token) -> Result<bool, PanicError> {
        match &self.lookahead {
            None => Err(PanicError::UnexpectedEof),
            Some(Err(err)) => Err(PanicError::Lexical(*err)),
            Some(Ok(current)) => Ok(*current == token),
        }
    }

    #[inline]
    fn match_identifier(&self) -> Result<bool, PanicError> {
        match &self.lookahead {
            Some(Ok(Token::Identifier(_))) => Ok(true),
            None => Err(PanicError::UnexpectedEof),
            Some(Err(err)) => Err(PanicError::Lexical(*err)),
            _ => Ok(false),
        }
    }

    #[inline]
    fn match_integer(&self) -> Result<bool, PanicError> {
        match &self.lookahead {
            Some(Ok(Token::Integer(_))) => Ok(true),
            None => Err(PanicError::UnexpectedEof),
            Some(Err(err)) => Err(PanicError::Lexical(*err)),
            _ => Ok(false),
        }
    }

    #[inline]
    fn consume(&mut self, token: Token) -> Result<(), UnexpectedBehaviorWhileParsing> {
        println!("[debug] consuming {token}");
        match self.lookahead.take() {
            None => Err(UnexpectedBehaviorWhileParsing::Panic(
                PanicError::UnexpectedEof,
            )),
            Some(lookahead) => {
                let lookahead = lookahead.map_err(|err| {
                    UnexpectedBehaviorWhileParsing::Panic(PanicError::Lexical(err))
                })?;

                if lookahead != token {
                    // Do not take it, give it back
                    self.lookahead = Some(Ok(lookahead));
                    self.record(SyntaxError);
                    self.synchronize();
                    // Jump to synchronization point
                    return Err(UnexpectedBehaviorWhileParsing::UnexpectedNormalToken);
                }

                self.advance();
                Ok(())
            }
        }
    }

    #[inline]
    fn record(&mut self, err: SyntaxError) {
        self.syntax_errors.push(err);
    }

    #[inline]
    fn synchronize(&mut self) {
        println!("[debug] recovering from error");
        while let Some(maybe_token) = &self.lookahead {
            if maybe_token.is_err() {
                self.advance();
                continue;
            }

            let token = maybe_token.as_ref().unwrap();
            match token {
                Token::Var
                | Token::Const
                | Token::Procedure
                | Token::Begin
                | Token::Read
                | Token::Write
                | Token::While
                | Token::Call
                | Token::If => break,
                Token::Semicolon => {
                    self.advance();
                    break;
                }
                _ => {}
            }

            self.advance();
        }
        println!(
            "[debug] synchronization: next token is {:?}",
            self.lookahead
        );
    }

    fn parse_identifier(&mut self) -> Result<String, UnexpectedBehaviorWhileParsing> {
        match self.lookahead.take() {
            Some(Ok(Token::Identifier(name))) => {
                println!("[debug] consume identifier {name}");
                self.advance();
                Ok(name)
            }
            None => Err(UnexpectedBehaviorWhileParsing::Panic(
                PanicError::UnexpectedEof,
            )),
            Some(result) => match result {
                Err(err) => Err(UnexpectedBehaviorWhileParsing::Panic(PanicError::Lexical(
                    err,
                ))),
                _ => Err(UnexpectedBehaviorWhileParsing::Panic(
                    PanicError::UnexpectedEof,
                )),
            },
        }
    }

    fn parse_integer(&mut self) -> Result<isize, UnexpectedBehaviorWhileParsing> {
        match self.lookahead.take() {
            Some(Ok(Token::Integer(int))) => {
                println!("[debug] consume integer {int}");
                self.advance();
                Ok(int)
            }
            None => Err(UnexpectedBehaviorWhileParsing::Panic(
                PanicError::UnexpectedEof,
            )),
            Some(result) => match result {
                Err(err) => Err(UnexpectedBehaviorWhileParsing::Panic(PanicError::Lexical(
                    err,
                ))),
                _ => Err(UnexpectedBehaviorWhileParsing::Panic(
                    PanicError::UnexpectedEof,
                )),
            },
        }
    }

    fn parse_program(&mut self) -> Result<Program, UnexpectedBehaviorWhileParsing> {
        let subprogram = self.parse_subprogram()?;
        self.consume(Token::Dot)?;

        Ok(Program { subprogram })
    }

    fn parse_subprogram(&mut self) -> Result<Box<Subprogram>, UnexpectedBehaviorWhileParsing> {
        let const_declaration = if self.match_token(Token::Const)? {
            Some(self.parse_const_declaration()?)
        } else {
            None
        };

        let var_declaration = if self.match_token(Token::Var)? {
            Some(self.parse_var_declaration()?)
        } else {
            None
        };

        let procedure_definition = if self.match_token(Token::Procedure)? {
            Some(self.parse_procedure_definition()?)
        } else {
            None
        };

        let statement = self.parse_statement()?;

        Ok(Box::new(Subprogram {
            const_declaration,
            var_declaration,
            procedure_definition,
            statement,
        }))
    }

    fn parse_const_declaration(
        &mut self,
    ) -> Result<ConstDeclaration, UnexpectedBehaviorWhileParsing> {
        self.consume(Token::Const)?;
        let mut definitions = vec![self.parse_const_definition()?];
        loop {
            if !self.match_token(Token::Comma)? {
                break;
            }
            definitions.push(self.parse_const_definition()?);
        }
        self.consume(Token::Semicolon)?;

        Ok(ConstDeclaration { definitions })
    }

    fn parse_const_definition(
        &mut self,
    ) -> Result<ConstDefinition, UnexpectedBehaviorWhileParsing> {
        let identifier = self.parse_identifier()?;
        self.consume(Token::Equal)?;
        let integer = self.parse_integer()?;

        Ok(ConstDefinition {
            identifier,
            integer,
        })
    }

    fn parse_var_declaration(&mut self) -> Result<VarDeclaration, UnexpectedBehaviorWhileParsing> {
        self.consume(Token::Var)?;
        let mut identifiers = vec![self.parse_identifier()?];
        loop {
            if !self.match_token(Token::Comma)? {
                break;
            }

            self.consume(Token::Comma).unwrap();
            identifiers.push(self.parse_identifier()?);
        }
        self.consume(Token::Semicolon)?;

        Ok(VarDeclaration { identifiers })
    }

    fn parse_procedure_definition(
        &mut self,
    ) -> Result<Box<ProcedureDefinition>, UnexpectedBehaviorWhileParsing> {
        let header = self.parse_procedure_header()?;
        let subprogram = self.parse_subprogram()?;
        let mut definitions = Vec::new();
        self.consume(Token::Semicolon)?;
        loop {
            if !self.match_token(Token::Procedure)? {
                break;
            }

            definitions.push(self.parse_procedure_definition()?);
            self.consume(Token::Semicolon)?;
        }

        Ok(Box::new(ProcedureDefinition {
            header,
            subprogram,
            definitions,
        }))
    }

    fn parse_procedure_header(
        &mut self,
    ) -> Result<ProcedureHeader, UnexpectedBehaviorWhileParsing> {
        self.consume(Token::Procedure)?;
        let identifier = self.parse_identifier()?;
        self.consume(Token::Semicolon)?;

        Ok(ProcedureHeader { identifier })
    }

    fn parse_statement(&mut self) -> Result<Box<Statement>, UnexpectedBehaviorWhileParsing> {
        fn try_parse(
            parser: &mut Ll1Parser,
        ) -> Result<Box<Statement>, UnexpectedBehaviorWhileParsing> {
            if parser.match_identifier()? {
                return Ok(Box::new(Statement::Assign(
                    parser.parse_assign_statement()?,
                )));
            }

            if parser.match_token(Token::If)? {
                return Ok(Box::new(Statement::Condition(
                    parser.parse_condition_statement()?,
                )));
            }

            if parser.match_token(Token::While)? {
                return Ok(Box::new(Statement::Loop(parser.parse_loop_statement()?)));
            }

            if parser.match_token(Token::Call)? {
                return Ok(Box::new(Statement::Call(parser.parse_call_statement()?)));
            }

            if parser.match_token(Token::Read)? {
                return Ok(Box::new(Statement::Read(parser.parse_read_statement()?)));
            }

            if parser.match_token(Token::Write)? {
                return Ok(Box::new(Statement::Write(parser.parse_write_statement()?)));
            }

            if parser.match_token(Token::Begin)? {
                return Ok(Box::new(Statement::Compound(
                    parser.parse_compound_statement()?,
                )));
            }

            Ok(Box::new(Statement::Null))
        }

        let result = try_parse(self);
        if result.is_err() {
            Ok(Box::new(Statement::Error))
        } else {
            result
        }
    }

    fn parse_assign_statement(
        &mut self,
    ) -> Result<AssignStatement, UnexpectedBehaviorWhileParsing> {
        let identifier = self.parse_identifier()?;
        self.consume(Token::ColonEqual)?;
        let expression = self.parse_expression()?;

        Ok(AssignStatement {
            identifier,
            expression,
        })
    }

    fn parse_compound_statement(
        &mut self,
    ) -> Result<CompoundStatement, UnexpectedBehaviorWhileParsing> {
        self.consume(Token::Begin)?;
        let mut statements = vec![self.parse_statement()?];
        loop {
            if !self.match_token(Token::Semicolon)? && self.match_token(Token::End)? {
                break;
            }

            // If a semicolon is forgotten at non-last statement, do a recovery
            match self.consume(Token::Semicolon) {
                Ok(_) | Err(UnexpectedBehaviorWhileParsing::UnexpectedNormalToken) => {}
                Err(err) => return Err(err),
            }

            statements.push(self.parse_statement()?);
        }
        self.consume(Token::End)?;

        Ok(CompoundStatement { statements })
    }

    // No need for parsing null statement

    fn parse_condition(&mut self) -> Result<Condition, UnexpectedBehaviorWhileParsing> {
        Ok(if self.match_token(Token::Odd)? {
            self.consume(Token::Odd).unwrap();
            let expression = self.parse_expression()?;

            Condition::Odd(expression)
        } else {
            let left = self.parse_expression()?;
            let operator = self.parse_relational_operator()?;
            let right = self.parse_expression()?;

            Condition::Binary(left, operator, right)
        })
    }

    fn parse_expression(&mut self) -> Result<Box<Expression>, UnexpectedBehaviorWhileParsing> {
        fn try_parse(
            parser: &mut Ll1Parser,
        ) -> Result<Box<Expression>, UnexpectedBehaviorWhileParsing> {
            let sign = if parser.match_token(Token::Plus)? {
                parser.advance();
                Some(Sign::Positive)
            } else if parser.match_token(Token::Minus)? {
                parser.advance();
                Some(Sign::Negative)
            } else {
                None
            };

            let item = parser.parse_item()?;
            let mut pairs = Vec::new();
            loop {
                let operator = if parser.match_token(Token::Plus)? {
                    parser.advance();
                    AddSubOperator::Add
                } else if parser.match_token(Token::Minus)? {
                    parser.advance();
                    AddSubOperator::Sub
                } else {
                    break;
                };

                pairs.push((operator, parser.parse_item()?));
            }

            Ok(Box::new(Expression::Valid { sign, item, pairs }))
        }

        let result = try_parse(self);
        if result.is_err() {
            Ok(Box::new(Expression::Invalid))
        } else {
            result
        }
    }

    fn parse_item(&mut self) -> Result<Item, UnexpectedBehaviorWhileParsing> {
        let factor = self.parse_factor()?;
        let mut pairs = Vec::new();
        loop {
            let operator = if self.match_token(Token::Star)? {
                self.advance();
                MulDivOperator::Mul
            } else if self.match_token(Token::Slash)? {
                self.advance();
                MulDivOperator::Div
            } else {
                break;
            };

            pairs.push((operator, self.parse_factor()?));
        }

        Ok(Item { factor, pairs })
    }

    fn parse_factor(&mut self) -> Result<Factor, UnexpectedBehaviorWhileParsing> {
        if self.match_identifier()? {
            return Ok(Factor::Identifier(self.parse_identifier()?));
        }

        if self.match_integer()? {
            return Ok(Factor::Integer(self.parse_integer()?));
        }

        self.consume(Token::LeftParenthese)?;
        let expression = self.parse_expression()?;
        self.consume(Token::RightParenthese)?;

        Ok(Factor::Expression(expression))
    }

    // No need for parsing arithmetic opeators

    #[inline]
    fn parse_relational_operator(
        &mut self,
    ) -> Result<RelationalOperator, UnexpectedBehaviorWhileParsing> {
        if self.match_token(Token::Equal)? {
            self.advance();
            return Ok(RelationalOperator::Equal);
        }

        if self.match_token(Token::Number)? {
            self.advance();
            return Ok(RelationalOperator::NotEqual);
        }

        if self.match_token(Token::LessThan)? {
            self.advance();
            return Ok(RelationalOperator::LessThan);
        }

        if self.match_token(Token::LessEqualThan)? {
            self.advance();
            return Ok(RelationalOperator::LessEqualThan);
        }

        if self.match_token(Token::GreaterThan)? {
            self.advance();
            return Ok(RelationalOperator::GreaterThan);
        }

        if self.match_token(Token::GreaterEqualThan)? {
            self.advance();
            return Ok(RelationalOperator::GreaterEqualThan);
        }

        Err(UnexpectedBehaviorWhileParsing::UnexpectedNormalToken)
    }

    fn parse_condition_statement(
        &mut self,
    ) -> Result<ConditionStatement, UnexpectedBehaviorWhileParsing> {
        self.consume(Token::If)?;
        let condition = self.parse_condition()?;
        self.consume(Token::Then)?;
        let statement = self.parse_statement()?;

        Ok(ConditionStatement {
            condition,
            statement,
        })
    }

    fn parse_call_statement(&mut self) -> Result<CallStatement, UnexpectedBehaviorWhileParsing> {
        self.consume(Token::Call)?;
        let procedure_identifier = self.parse_identifier()?;

        Ok(CallStatement {
            procedure_identifier,
        })
    }

    fn parse_loop_statement(&mut self) -> Result<LoopStatement, UnexpectedBehaviorWhileParsing> {
        self.consume(Token::While)?;
        let condition = self.parse_condition()?;
        self.consume(Token::Do)?;
        let statement = self.parse_statement()?;

        Ok(LoopStatement {
            condition,
            statement,
        })
    }

    fn parse_read_statement(&mut self) -> Result<ReadStatement, UnexpectedBehaviorWhileParsing> {
        self.consume(Token::Read)?;
        self.consume(Token::LeftParenthese)?;
        let mut identifiers = vec![self.parse_identifier()?];
        loop {
            if !self.match_token(Token::Comma)? {
                break;
            }

            self.consume(Token::Comma).unwrap();
            identifiers.push(self.parse_identifier()?);
        }
        self.consume(Token::RightParenthese)?;

        Ok(ReadStatement { identifiers })
    }

    fn parse_write_statement(&mut self) -> Result<WriteStatement, UnexpectedBehaviorWhileParsing> {
        self.consume(Token::Write)?;
        self.consume(Token::LeftParenthese)?;
        let mut expressions = vec![self.parse_expression()?];
        loop {
            if !self.match_token(Token::Comma)? {
                break;
            }

            self.consume(Token::Comma).unwrap();
            expressions.push(self.parse_expression()?);
        }
        self.consume(Token::RightParenthese)?;

        Ok(WriteStatement { expressions })
    }
}
