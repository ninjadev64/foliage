mod reader;
use std::str::FromStr;

use reader::SourceCodeReader;

mod token_stream;
use token_stream::TokenStream;

#[derive(Debug)]
pub struct Token {
	pub kind: TokenKind,
	pub length: usize,
}

impl Token {
	fn new(kind: TokenKind, length: usize) -> Self {
		Self { kind, length }
	}
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "kind: {:?} length: {}", self.kind, self.length)?;
		Ok(())
	}
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Keyword {
	Function,
	Let,
	If,
	Else,
	Return,
	Struct,
}

impl std::fmt::Display for Keyword {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}",
			match self {
				Self::Function => "func",
				Self::Let => "let",
				Self::If => "if",
				Self::Else => "else",
				Self::Return => "return",
				Self::Struct => "struct",
			}
		)?;
		Ok(())
	}
}

impl std::str::FromStr for Keyword {
	type Err = ();
	fn from_str(s: &str) -> Result<Self, Self::Err> {
		Ok(match s {
			"func" => Self::Function,
			"let" => Self::Let,
			"if" => Self::If,
			"else" => Self::Else,
			"return" => Self::Return,
			"struct" => Self::Struct,
			_ => return Err(()),
		})
	}
}

#[derive(PartialEq, Clone, Debug)]
pub enum Invalid {
	Unknown(char),
	UnterminatedStringLiteral,
	UnterminatedBlockComment,
}

#[derive(PartialEq, Clone, Debug)]
pub enum TokenKind {
	Identifier(String),
	Keyword(Keyword),
	IntegerLiteral(i32),
	FloatLiteral(f64),
	BooleanLiteral(bool),
	StringLiteral(String),
	Semicolon,
	Colon,
	Comma,
	Dot,
	OpenParenthesis,
	CloseParenthesis,
	OpenBrace,
	CloseBrace,
	Equals,
	LessThan,
	GreaterThan,
	And,
	Pipe,
	Bang,
	Plus,
	Hyphen,
	Asterisk,
	Slash,
	LineComment,
	BlockComment,
	Whitespace,
	Invalid(Invalid),
}

impl ToString for TokenKind {
	fn to_string(&self) -> String {
		match self {
			Self::Identifier(val) => val.clone(),
			Self::Keyword(val) => format!("{}", val),
			Self::IntegerLiteral(val) => format!("{}", val),
			Self::FloatLiteral(val) => format!("{}", val),
			Self::BooleanLiteral(val) => format!("{}", val),
			Self::StringLiteral(val) => format!("\"{}\"", val),
			Self::Semicolon => ";".to_owned(),
			Self::Colon => ":".to_owned(),
			Self::Comma => ",".to_owned(),
			Self::Dot => ".".to_owned(),
			Self::OpenParenthesis => "(".to_owned(),
			Self::CloseParenthesis => ")".to_owned(),
			Self::OpenBrace => "{".to_owned(),
			Self::CloseBrace => "}".to_owned(),
			Self::Equals => "=".to_owned(),
			Self::LessThan => "<".to_owned(),
			Self::GreaterThan => ">".to_owned(),
			Self::And => "&".to_owned(),
			Self::Pipe => "|".to_owned(),
			Self::Bang => "!".to_owned(),
			Self::Plus => "+".to_owned(),
			Self::Hyphen => "-".to_owned(),
			Self::Asterisk => "*".to_owned(),
			Self::Slash => "/".to_owned(),
			Self::LineComment => "line comment".to_owned(),
			Self::BlockComment => "block comment".to_owned(),
			Self::Whitespace => "whitespace".to_owned(),
			Self::Invalid(invalid) => match invalid {
				Invalid::Unknown(char) => format!("unknown character {}", char),
				Invalid::UnterminatedStringLiteral => "unterminated string literal".to_owned(),
				Invalid::UnterminatedBlockComment => "unterminated block comment".to_owned(),
			},
		}
	}
}

impl TokenKind {
	pub fn noun(&self) -> String {
		match self {
			Self::Identifier(val) => format!("identifier \"{}\"", val),
			Self::Keyword(val) => format!("keyword \"{}\"", val),
			Self::IntegerLiteral(val) => format!("integer literal \"{}\"", val),
			Self::FloatLiteral(val) => format!("float literal \"{}\"", val),
			Self::BooleanLiteral(val) => format!("boolean literal \"{}\"", val),
			Self::StringLiteral(val) => format!("string literal \"{}\"", val),
			_ => {
				let s = self.to_string();
				if s.len() == 1 {
					format!("\"{}\"", s)
				} else {
					s
				}
			}
		}
	}

	pub fn is_trivia(&self) -> bool {
		matches!(self, Self::Whitespace | Self::LineComment | Self::BlockComment)
	}
}

pub struct Lexer {
	reader: SourceCodeReader,
	tokens: TokenStream,
}

impl Lexer {
	pub fn new(code: String) -> Self {
		Self {
			reader: SourceCodeReader::new(code),
			tokens: TokenStream::default(),
		}
	}

	pub fn lex_file(mut self) -> Vec<Token> {
		while self.reader.available() {
			match self.reader.read() {
				c if c.is_alphabetic() => self.eat_ident_or_keyword(),
				'"' => self.eat_string_literal(),
				c if c.is_ascii_digit() => self.eat_number_literal(),
				';' => self.eat_single_character(TokenKind::Semicolon),
				':' => self.eat_single_character(TokenKind::Colon),
				',' => self.eat_single_character(TokenKind::Comma),
				'.' => self.eat_single_character(TokenKind::Dot),
				'(' => self.eat_single_character(TokenKind::OpenParenthesis),
				')' => self.eat_single_character(TokenKind::CloseParenthesis),
				'{' => self.eat_single_character(TokenKind::OpenBrace),
				'}' => self.eat_single_character(TokenKind::CloseBrace),
				'=' => self.eat_single_character(TokenKind::Equals),
				'<' => self.eat_single_character(TokenKind::LessThan),
				'>' => self.eat_single_character(TokenKind::GreaterThan),
				'&' => self.eat_single_character(TokenKind::And),
				'|' => self.eat_single_character(TokenKind::Bang),
				'!' => self.eat_single_character(TokenKind::Bang),
				'+' => self.eat_single_character(TokenKind::Plus),
				'-' => self.eat_single_character(TokenKind::Hyphen),
				'*' => self.eat_single_character(TokenKind::Asterisk),
				'/' => {
					if self.reader.second() == '/' {
						self.eat_line_comment();
					} else if self.reader.second() == '*' {
						self.eat_block_comment();
					} else {
						self.eat_single_character(TokenKind::Slash)
					}
				}
				c if c.is_whitespace() => self.eat_whitespace(),
				c => {
					self.tokens.push(Token::new(TokenKind::Invalid(Invalid::Unknown(c)), 1));
					self.reader.advance(1);
				}
			}
		}

		self.tokens.finish()
	}

	fn eat_single_character(&mut self, kind: TokenKind) {
		self.tokens.push(Token::new(kind, 1));
		self.reader.advance(1);
	}

	fn eat_whitespace(&mut self) {
		let mut length = 0;
		while self.reader.available() && self.reader.read().is_whitespace() {
			length += 1;
			self.reader.advance(1);
		}
		self.tokens.push(Token::new(TokenKind::Whitespace, length))
	}

	fn eat_ident_or_keyword(&mut self) {
		let mut chars = String::new();
		while self.reader.available() && self.reader.read().is_alphanumeric() {
			chars.push(self.reader.read());
			self.reader.advance(1);
		}
		if let Ok(keyword) = Keyword::from_str(&chars) {
			self.tokens.push(Token::new(TokenKind::Keyword(keyword), chars.len()))
		} else if chars == "true" || chars == "false" {
			self.tokens.push(Token::new(
				TokenKind::BooleanLiteral(bool::from_str(&chars).unwrap()),
				chars.len(),
			))
		} else {
			self.tokens
				.push(Token::new(TokenKind::Identifier(chars.clone()), chars.len()))
		}
	}

	fn eat_string_literal(&mut self) {
		let mut chars = String::new();

		let mut escaped = false;
		let mut terminated = false;

		while self.reader.available() {
			self.reader.advance(1);
			match self.reader.read() {
				'\\' => escaped = !escaped,
				'"' if !escaped => {
					self.reader.advance(1);
					terminated = true;
					break;
				}
				_ => escaped = false,
			}
			chars.push(self.reader.read());
		}

		if terminated {
			self.tokens
				.push(Token::new(TokenKind::StringLiteral(chars.clone()), chars.len() + 2))
		} else {
			self.tokens.push(Token::new(
				TokenKind::Invalid(Invalid::UnterminatedStringLiteral),
				chars.len() + 1,
			))
		}
	}

	fn eat_number_literal(&mut self) {
		let mut chars = String::new();
		let mut is_float = false;

		while self.reader.available() && self.reader.read().is_ascii_digit() {
			chars.push(self.reader.read());
			self.reader.advance(1);
			if self.reader.read() == '.' && !is_float {
				is_float = true;
				chars.push(self.reader.read());
				self.reader.advance(1);
			}
		}

		if is_float {
			self.tokens.push(Token::new(
				TokenKind::FloatLiteral(f64::from_str(&chars).unwrap()),
				chars.len(),
			))
		} else {
			self.tokens.push(Token::new(
				TokenKind::IntegerLiteral(i32::from_str(&chars).unwrap()),
				chars.len(),
			))
		}
	}

	fn eat_line_comment(&mut self) {
		self.reader.advance(2);
		let mut length = 2;
		while self.reader.available() {
			if self.reader.read() == '\n' {
				break;
			}
			length += 1;
			self.reader.advance(1);
		}
		self.tokens.push(Token::new(TokenKind::LineComment, length));
	}

	fn eat_block_comment(&mut self) {
		self.reader.advance(2);
		let mut length = 2;
		let mut asterisk = false;
		let mut terminated = false;
		while self.reader.available() {
			if self.reader.read() == '*' {
				asterisk = true;
			} else if asterisk && self.reader.read() == '/' {
				length += 1;
				self.reader.advance(1);
				terminated = true;
				break;
			} else {
				asterisk = false;
			}
			length += 1;
			self.reader.advance(1);
		}

		if terminated {
			self.tokens.push(Token::new(TokenKind::BlockComment, length));
		} else {
			self.tokens.push(Token::new(
				TokenKind::Invalid(Invalid::UnterminatedBlockComment),
				length,
			));
		}
	}
}
