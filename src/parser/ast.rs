#[derive(Debug)]
pub struct WithSpan<T> {
	pub kind: T,
	pub start: usize,
	pub end: usize,
}

impl<T> WithSpan<T> {
	pub fn new(kind: T, start: usize, end: usize) -> Self {
		Self { kind, start, end }
	}

	pub fn new_len(kind: T, start: usize, length: usize) -> Self {
		Self {
			kind,
			start,
			end: start + length,
		}
	}
}

impl<T> Clone for WithSpan<T>
where
	T: Clone,
{
	fn clone(&self) -> Self {
		Self {
			kind: self.kind.clone(),
			start: self.start,
			end: self.end,
		}
	}
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum OperatorKind {
	Add,
	Sub,
	Mul,
	Div,
	Not,
	And,
	Or,
	Equ,
	Lte,
	Lt,
	Gte,
	Gt,
}
impl std::fmt::Display for OperatorKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}",
			match self {
				Self::Add => "+",
				Self::Sub => "-",
				Self::Mul => "*",
				Self::Div => "/",
				Self::Not => "!",
				Self::And => "&&",
				Self::Or => "||",
				Self::Equ => "==",
				Self::Lte => "<=",
				Self::Lt => "<",
				Self::Gte => ">=",
				Self::Gt => ">",
			}
		)
	}
}
pub type Operator = WithSpan<OperatorKind>;

#[derive(Clone, Debug)]
pub enum ExpressionKind {
	IntegerLiteral(i32),
	FloatLiteral(f64),
	BooleanLiteral(bool),
	StringLiteral(String),
	Identifier(String),
	UnaryOp {
		operator: Operator,
		operand: Box<Expression>,
	},
	BinaryOp {
		left_operand: Box<Expression>,
		operator: Operator,
		right_operand: Box<Expression>,
	},
	FunctionCall {
		identifier: String,
		parameters: Vec<Expression>,
	},
	MemberAccess {
		parent: Box<Expression>,
		child: Box<Expression>,
	},
}
pub type Expression = WithSpan<ExpressionKind>;

#[derive(Debug)]
pub enum StatementKind {
	Expression(Expression),
	VariableDeclaration {
		identifier: Identifier,
		datatype: Identifier,
		expression: Expression,
	},
	Assignment {
		identifier: Identifier,
		expression: Expression,
	},
	Block {
		statements: Vec<Statement>,
	},
	If {
		condition: Expression,
		statements: Vec<Statement>,
		else_statement: Option<Box<Statement>>,
	},
	Return {
		expression: Expression,
	},
}
pub type Statement = WithSpan<StatementKind>;

pub type Identifier = WithSpan<String>;
impl Identifier {
	pub fn new_identifier(start: usize, val: String) -> Self {
		Self {
			start,
			end: start + val.len(),
			kind: val,
		}
	}
}

#[derive(Debug)]
pub struct Function {
	pub return_type: Identifier,
	pub identifier: Identifier,
	pub parameters: Vec<(Identifier, Identifier)>,
	pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct Struct {
	pub identifier: Identifier,
	pub fields: Vec<(Identifier, Identifier)>,
}

#[derive(Default, Debug)]
#[allow(clippy::upper_case_acronyms)]
pub struct AST {
	pub functions: Vec<Function>,
	pub structs: Vec<Struct>,
}
