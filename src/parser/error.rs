use crate::lexer::{Token, TokenKind};

use std::error::Error as StdError;
use thiserror::Error as ThisError;

#[derive(ThisError, Debug)]
pub enum ParserError {
	#[error("expected {}, got {}", expected.noun(), received.noun())]
	Expected { expected: TokenKind, received: TokenKind },
	#[error("expected identifier, got {}", received.noun())]
	ExpectedIdentifier { received: TokenKind },
	#[error("unexpected {}", received.noun())]
	Unexpected { received: TokenKind },
	#[error("unexpected EOF")]
	UnexpectedEof,
}

#[derive(ThisError, Debug)]
pub enum ValidationError {
	#[error("undeclared identifier {0}")]
	UndeclaredIdentifier(String),
	#[error("cannot redeclare identifier {0}")]
	DuplicateIdentifier(String),
	#[error("wrong datatype {received}, expected {expected}")]
	WrongDatatype { received: String, expected: String },
	#[error("cannot perform operation {operator} with {left} and {right}")]
	ImpossibleOperation {
		operator: super::ast::OperatorKind,
		left: String,
		right: String,
	},
	#[error("recursive reference to {violating} found within {within}")]
	RecursiveStructFields { within: String, violating: String },
}

#[derive(Debug)]
pub struct Error<T>
where
	T: StdError + std::fmt::Display,
{
	pub kind: T,
	pub location: usize,
	pub length: usize,
}

pub fn convert_position(pos: usize, tokens: &[Token]) -> usize {
	let mut count = 0;
	for token in &tokens[0..pos] {
		count += token.length;
	}
	count
}

impl<T> Error<T>
where
	T: StdError,
{
	pub fn new(kind: T, location: usize, length: usize) -> Self {
		Self { kind, location, length }
	}

	pub fn span(kind: T, start: usize, end: usize) -> Self {
		Self::new(kind, start, end - start)
	}
}

pub fn display_error<T>(error: Error<T>, code: &str, filename: &str) -> String
where
	T: StdError,
{
	let mut output = "\x1b[31;1merror: \x1b[39m".to_owned();
	output += &format!("{}", error.kind);

	let start = code[..error.location].rfind('\n').unwrap_or(0);
	let line = code[..error.location].chars().filter(|c| *c == '\n').count();
	let content: &str = code.split('\n').nth(line).unwrap();

	output += &format!(
		"\n  \x1b[34m-->\x1b[0m {}:{}:{}",
		filename,
		line + 1,
		error.location - start
	);

	if line > 1 {
		output += &format!("\n\x1b[34m{} | \x1b[39m", line - 1);
		output += code.split('\n').nth(line - 2).unwrap();
	}

	if line > 0 {
		output += &format!("\n\x1b[34m{} | \x1b[39m", line);
		output += code.split('\n').nth(line - 1).unwrap();
	}

	output += &format!("\n\x1b[34m{} | \x1b[39m", line + 1);
	let (error_start, error_end) = if error.location - start > 0 {
		(error.location - start - 1, error.location - start + error.length - 1)
	} else {
		(0, error.location - start + error.length)
	};
	output += &content[..error_start];
	output += "\x1b[4;31m";
	output += &content[error_start..error_end];
	output += "\x1b[24m\x1b[39m";
	output += &content[error_end..];

	output
}
