pub mod error;
use error::*;

pub mod ast;
use ast::*;

pub mod validate;

use crate::lexer::{Keyword, Token, TokenKind};

pub struct Parser {
	tokens: Vec<Token>,
	pos: usize,
	tree: AST,
}

impl Parser {
	pub fn new(tokens: Vec<Token>) -> Self {
		Self {
			tokens,
			pos: 0,
			tree: AST::default(),
		}
	}

	fn bump(&mut self) {
		self.pos += 1;
	}

	fn skip_trivia(&mut self) {
		while self.pos < self.tokens.len() && self.tokens[self.pos].kind.is_trivia() {
			self.bump();
		}
	}

	fn first(&mut self) -> Result<&Token, Error<ParserError>> {
		self.skip_trivia();
		if self.pos >= self.tokens.len() {
			Err(Error::new(
				ParserError::UnexpectedEof,
				convert_position(self.pos, &self.tokens),
				0,
			))
		} else {
			Ok(&self.tokens[self.pos])
		}
	}

	fn expect(&mut self, kind: TokenKind) -> Result<(), Error<ParserError>> {
		if self.first()?.kind == kind {
			self.bump();
			Ok(())
		} else {
			Err(Error::new(
				ParserError::Expected {
					expected: kind,
					received: self.first()?.kind.clone(),
				},
				convert_position(self.pos, &self.tokens),
				self.first()?.length,
			))
		}
	}

	fn location(&mut self) -> usize {
		self.skip_trivia();
		convert_position(self.pos, &self.tokens)
	}

	fn expect_ident(&mut self) -> Result<String, Error<ParserError>> {
		self.skip_trivia();
		let value: String;
		if let TokenKind::Identifier(val) = &self.first()?.kind {
			value = val.clone();
		} else {
			return Err(Error::new(
				ParserError::ExpectedIdentifier {
					received: self.first()?.kind.clone(),
				},
				convert_position(self.pos, &self.tokens),
				self.first()?.length,
			));
		}
		self.bump();
		Ok(value)
	}
}

impl Parser {
	/// Handles parsing the top level of a file.
	pub fn parse_file(mut self) -> Result<AST, Error<ParserError>> {
		while self.pos < self.tokens.len() {
			match self.first()?.kind {
				TokenKind::Identifier(_) => self.parse_function()?,
				TokenKind::Keyword(Keyword::Struct) => self.parse_struct()?,
				_ => {
					return Err(Error::new(
						ParserError::Unexpected {
							received: self.first()?.kind.clone(),
						},
						convert_position(self.pos, &self.tokens),
						self.first()?.length,
					))
				}
			}
			self.skip_trivia();
		}

		Ok(self.tree)
	}

	/// Handles parsing of expressions that begin with identifiers.
	fn parse_identifier_expression(&mut self) -> Result<Expression, Error<ParserError>> {
		let start = self.location();
		let identifier = self.expect_ident()?;
		let mut kind: ExpressionKind = ExpressionKind::Identifier(identifier.clone());

		if self.expect(TokenKind::OpenParenthesis).is_ok() {
			let mut parameters = vec![];
			while self.expect(TokenKind::CloseParenthesis).is_err() {
				let expr = self.parse_expression()?;
				parameters.push(expr);
				if self.expect(TokenKind::Comma).is_err() {
					self.expect(TokenKind::CloseParenthesis)?;
					break;
				}
			}
			kind = ExpressionKind::FunctionCall { identifier, parameters }
		}

		let mut expression = Expression::new(kind, start, self.location());

		if self.expect(TokenKind::Dot).is_ok() {
			expression = Expression::new(
				ExpressionKind::MemberAccess {
					parent: Box::new(expression),
					child: Box::new(self.parse_identifier_expression()?),
				},
				start,
				self.location(),
			);
		}

		Ok(expression)
	}

	/// Handles parsing an expression.
	fn parse_expression(&mut self) -> Result<Expression, Error<ParserError>> {
		let start = self.location();
		let token = self.first()?;
		let mut expression: Expression;
		match &token.kind {
			TokenKind::IntegerLiteral(val) => {
				expression = Expression::new_len(ExpressionKind::IntegerLiteral(*val), start, token.length);
				self.bump();
			}
			TokenKind::FloatLiteral(val) => {
				expression = Expression::new_len(ExpressionKind::FloatLiteral(*val), start, token.length);
				self.bump();
			}
			TokenKind::BooleanLiteral(val) => {
				expression = Expression::new_len(ExpressionKind::BooleanLiteral(*val), start, token.length);
				self.bump();
			}
			TokenKind::StringLiteral(val) => {
				expression = Expression::new_len(ExpressionKind::StringLiteral(val.clone()), start, token.length);
				self.bump();
			}
			TokenKind::Identifier(val) => {
				expression = Expression::new_len(ExpressionKind::Identifier(val.clone()), start, token.length);
			}
			TokenKind::OpenParenthesis => {
				self.bump();
				expression = self.parse_expression()?;
				self.expect(TokenKind::CloseParenthesis)?;
			}
			TokenKind::Bang => {
				self.bump();
				expression = Expression::new(
					ExpressionKind::UnaryOp {
						operator: Operator::new_len(OperatorKind::Not, start, 1),
						operand: Box::new(self.parse_expression()?),
					},
					start,
					self.location(),
				);
			}
			kind => {
				return Err(Error::new(
					ParserError::Unexpected { received: kind.clone() },
					self.location(),
					self.first()?.length,
				))
			}
		}

		if let ExpressionKind::Identifier(_) = expression.kind {
			expression = self.parse_identifier_expression()?;
		}

		self.skip_trivia();
		let operator_start = self.location();
		let operator_kind = match (&self.tokens[self.pos].kind, &self.tokens[self.pos + 1].kind) {
			(TokenKind::Plus, _) => OperatorKind::Add,
			(TokenKind::Hyphen, _) => OperatorKind::Sub,
			(TokenKind::Asterisk, _) => OperatorKind::Mul,
			(TokenKind::Slash, _) => OperatorKind::Div,
			(TokenKind::And, TokenKind::And) => OperatorKind::And,
			(TokenKind::Pipe, TokenKind::Pipe) => OperatorKind::Or,
			(TokenKind::Equals, TokenKind::Equals) => OperatorKind::Equ,
			(TokenKind::LessThan, TokenKind::Equals) => OperatorKind::Lte,
			(TokenKind::LessThan, _) => OperatorKind::Lt,
			(TokenKind::GreaterThan, TokenKind::Equals) => OperatorKind::Gte,
			(TokenKind::GreaterThan, _) => OperatorKind::Gt,
			_ => return Ok(expression),
		};
		self.bump();
		if matches!(
			operator_kind,
			OperatorKind::And | OperatorKind::Or | OperatorKind::Equ | OperatorKind::Lte | OperatorKind::Gte
		) {
			self.bump();
		}
		expression = Expression::new(
			ExpressionKind::BinaryOp {
				left_operand: Box::new(expression),
				operator: Operator::new(operator_kind, operator_start, self.location()),
				right_operand: Box::new(self.parse_expression()?),
			},
			start,
			self.location(),
		);

		Ok(expression)
	}

	/// Handles parsing of a complete statement.
	fn parse_statement(&mut self) -> Result<Statement, Error<ParserError>> {
		let start = self.location();
		let statement = match &self.first()?.kind {
			TokenKind::Identifier(_) => {
				let identifier = Identifier::new_len(self.expect_ident()?, start, self.first()?.length);
				let after_first = self.location();
				if self.expect(TokenKind::Equals).is_ok() {
					let statement = Statement::new(
						StatementKind::Assignment {
							identifier,
							expression: self.parse_expression()?,
						},
						start,
						self.location(),
					);
					self.expect(TokenKind::Semicolon)?;
					statement
				} else if let Ok(name) = self.expect_ident() {
					let len = name.len();
					let name = Identifier::new_len(name, after_first, len);
					self.expect(TokenKind::Equals)?;
					let expression = self.parse_expression()?;
					self.expect(TokenKind::Semicolon)?;
					Statement::new(
						StatementKind::VariableDeclaration {
							identifier: name,
							datatype: identifier,
							expression,
						},
						start,
						self.location(),
					)
				} else if self.expect(TokenKind::Dot).is_ok() {
					let statement = Statement::new(
						StatementKind::Expression(self.parse_expression()?),
						start,
						self.location(),
					);
					self.expect(TokenKind::Semicolon)?;
					statement
				} else {
					return Err(Error::new(
						ParserError::Unexpected {
							received: self.first()?.kind.clone(),
						},
						self.location(),
						self.first()?.length,
					));
				}
			}
			TokenKind::Keyword(Keyword::If) => {
				self.bump();
				let condition = self.parse_expression()?;
				let statements = self.parse_block()?;

				let else_statement: Option<Box<Statement>>;
				if self.expect(TokenKind::Keyword(Keyword::Else)).is_err() {
					else_statement = None;
				} else if self.first()?.kind == TokenKind::Keyword(Keyword::If) {
					else_statement = Some(Box::new(self.parse_statement()?));
				} else {
					else_statement = Some(Box::new(Statement::new(
						StatementKind::Block {
							statements: self.parse_block()?,
						},
						start,
						self.location(),
					)));
				}

				Statement::new(
					StatementKind::If {
						condition,
						statements,
						else_statement,
					},
					start,
					self.location(),
				)
			}
			TokenKind::Keyword(Keyword::Return) => {
				self.bump();
				let expression = self.parse_expression()?;
				self.expect(TokenKind::Semicolon)?;
				Statement::new(StatementKind::Return { expression }, start, self.location())
			}
			_ => {
				let statement = Statement::new(
					StatementKind::Expression(self.parse_expression()?),
					start,
					self.location(),
				);
				self.expect(TokenKind::Semicolon)?;
				statement
			}
		};

		Ok(statement)
	}

	/// Handles parsing a block of statements.
	fn parse_block(&mut self) -> Result<Vec<Statement>, Error<ParserError>> {
		let mut statements: Vec<Statement> = vec![];

		self.expect(TokenKind::OpenBrace)?;

		while self.expect(TokenKind::CloseBrace).is_err() {
			statements.push(self.parse_statement()?);
		}

		Ok(statements)
	}

	/// Handles parsing the signature and contents of a function.
	fn parse_function(&mut self) -> Result<(), Error<ParserError>> {
		// Datatype
		let return_type = Identifier::new_identifier(self.location(), self.expect_ident()?);
		// Function keyword
		self.expect(TokenKind::Keyword(Keyword::Function))?;
		// Identifier
		let identifier = Identifier::new_identifier(self.location(), self.expect_ident()?);

		// Parameters
		let mut parameters = vec![];
		self.expect(TokenKind::OpenParenthesis)?;
		while self.expect(TokenKind::CloseParenthesis).is_err() {
			let datatype = Identifier::new_identifier(self.location(), self.expect_ident()?);
			let identifier = Identifier::new_identifier(self.location(), self.expect_ident()?);
			parameters.push((identifier, datatype));
			if self.expect(TokenKind::Comma).is_err() {
				self.expect(TokenKind::CloseParenthesis)?;
				break;
			}
		}

		// Body
		let statements = self.parse_block()?;

		self.tree.functions.push(Function {
			return_type,
			identifier,
			parameters,
			statements,
		});

		Ok(())
	}

	fn parse_struct(&mut self) -> Result<(), Error<ParserError>> {
		self.expect(TokenKind::Keyword(Keyword::Struct))?;
		let identifier = Identifier::new_identifier(self.location(), self.expect_ident()?);
		self.expect(TokenKind::OpenBrace)?;

		let mut fields = vec![];
		while self.expect(TokenKind::CloseBrace).is_err() {
			let datatype = Identifier::new_identifier(self.location(), self.expect_ident()?);
			let identifier = Identifier::new_identifier(self.location(), self.expect_ident()?);
			fields.push((identifier, datatype));
			if self.expect(TokenKind::Comma).is_err() {
				self.expect(TokenKind::CloseBrace)?;
				break;
			}
		}

		self.tree.structs.push(Struct { identifier, fields });

		Ok(())
	}
}
