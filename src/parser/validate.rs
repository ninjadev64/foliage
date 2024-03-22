use super::ast::*;
use super::error::*;

const BUILTINS: [&str; 4] = ["int", "float", "string", "bool"];

pub fn validate_expression(
	expression: &Expression,
	tree: &AST,
	variables: &[(String, String)],
) -> Result<String, Error<ValidationError>> {
	let datatype;
	match &expression.kind {
		ExpressionKind::FloatLiteral(_) => datatype = "float".to_owned(),
		ExpressionKind::IntegerLiteral(_) => datatype = "int".to_owned(),
		ExpressionKind::BooleanLiteral(_) => datatype = "bool".to_owned(),
		ExpressionKind::StringLiteral(_) => datatype = "string".to_owned(),
		ExpressionKind::Identifier(val) => {
			if let Some((_, t)) = variables.iter().find(|v| &v.0 == val) {
				datatype = t.to_owned();
			} else {
				return Err(Error::span(
					ValidationError::UndeclaredIdentifier(val.clone()),
					expression.start,
					expression.end,
				));
			}
		}
		ExpressionKind::UnaryOp { operator: _, operand } => {
			datatype = validate_expression(operand, tree, variables)?;
			if datatype != "bool" {
				return Err(Error::span(
					ValidationError::WrongDatatype {
						received: datatype.to_owned(),
						expected: "bool".to_owned(),
					},
					expression.start,
					expression.end,
				));
			}
		}
		ExpressionKind::BinaryOp {
			left_operand,
			operator,
			right_operand,
		} => {
			let left = validate_expression(left_operand, tree, variables)?;
			let right = validate_expression(right_operand, tree, variables)?;
			if (left != right && !matches!((left.as_str(), right.as_str()), ("int", "float") | ("float", "int")))
				|| (left == "string" && !matches!(operator.kind, OperatorKind::Add | OperatorKind::Equ))
				|| (left == "bool"
					&& !matches!(operator.kind, OperatorKind::Equ | OperatorKind::And | OperatorKind::Or))
			{
				return Err(Error::span(
					ValidationError::ImpossibleOperation {
						operator: operator.kind.clone(),
						left: left.to_owned(),
						right: right.to_owned(),
					},
					expression.start,
					expression.end,
				));
			}
			if [left.as_str(), right.as_str()].contains(&"float") {
				datatype = "float".to_owned();
			} else {
				datatype = left;
			}
		}
		ExpressionKind::FunctionCall { identifier, parameters } => {
			if let Some(function) = tree.functions.iter().find(|v| &v.identifier.kind == identifier) {
				datatype = function.return_type.kind.clone();
				for (expression, expected) in parameters.iter().zip(&function.parameters) {
					let received = validate_expression(expression, tree, variables)?;
					if received != expected.1.kind {
						return Err(Error::span(
							ValidationError::WrongDatatype {
								received,
								expected: expected.1.kind.clone(),
							},
							expression.start,
							expression.end,
						));
					}
				}
			} else {
				return Err(Error::span(
					ValidationError::UndeclaredIdentifier(identifier.clone()),
					expression.start,
					expression.end,
				));
			}
		}
		ExpressionKind::MemberAccess { .. } => datatype = "string".to_owned(),
	}

	Ok(datatype)
}

pub fn validate_block(
	statements: &Vec<Statement>,
	tree: &AST,
	mut variables: Vec<(String, String)>,
	return_type: &str,
) -> Result<(), Error<ValidationError>> {
	for statement in statements {
		match &statement.kind {
			StatementKind::Expression(expr) => {
				validate_expression(expr, tree, &variables)?;
			}
			StatementKind::VariableDeclaration {
				identifier,
				datatype,
				expression,
			} => {
				if variables.iter().any(|v| v.0 == identifier.kind) {
					return Err(Error::span(
						ValidationError::DuplicateIdentifier(identifier.kind.clone()),
						identifier.start,
						identifier.end,
					));
				}
				let received = validate_expression(expression, tree, &variables)?.to_owned();
				if received != datatype.kind {
					return Err(Error::span(
						ValidationError::WrongDatatype {
							received,
							expected: datatype.kind.clone(),
						},
						datatype.start,
						datatype.end,
					));
				}
				variables.push((identifier.kind.clone(), datatype.kind.clone()));
			}
			StatementKind::Assignment { identifier, expression } => {
				let first = variables.iter().find(|v| v.0 == identifier.kind);
				if first.is_none() {
					return Err(Error::span(
						ValidationError::UndeclaredIdentifier(identifier.kind.clone()),
						identifier.start,
						identifier.end,
					));
				}
				let expected = &first.unwrap().1;
				let received = validate_expression(expression, tree, &variables)?;
				if &received != expected {
					return Err(Error::span(
						ValidationError::WrongDatatype {
							received: received.to_owned(),
							expected: expected.to_owned(),
						},
						expression.start,
						expression.end,
					));
				}
			}
			StatementKind::Block { statements } => validate_block(statements, tree, variables.clone(), return_type)?,
			StatementKind::If {
				condition,
				statements,
				else_statement,
			} => {
				validate_expression(condition, tree, &variables)?;
				validate_block(statements, tree, variables.clone(), return_type)?;
				if let Some(else_statement) = else_statement {
					if let StatementKind::Block { statements } = &else_statement.kind {
						validate_block(statements, tree, variables.clone(), return_type)?;
					}
				}
			}
			StatementKind::Return { expression } => {
				let received = validate_expression(expression, tree, &variables)?;
				if received != return_type {
					return Err(Error::span(
						ValidationError::WrongDatatype {
							received: received.to_owned(),
							expected: return_type.to_owned(),
						},
						expression.start,
						expression.end,
					));
				};
			}
		};
	}

	Ok(())
}

fn validate_struct(tree: &AST, r#struct: &Struct, found: &mut Vec<String>) -> Result<(), Error<ValidationError>> {
	found.push(r#struct.identifier.kind.clone());
	for (_, datatype) in &r#struct.fields {
		if !BUILTINS.contains(&&datatype.kind[..]) && !tree.structs.iter().any(|v| v.identifier.kind == datatype.kind) {
			return Err(Error::span(
				ValidationError::UndeclaredIdentifier(datatype.kind.clone()),
				datatype.start,
				datatype.end,
			));
		}
		if found.contains(&datatype.kind) {
			return Err(Error::span(
				ValidationError::RecursiveStructFields {
					within: datatype.kind.clone(),
					violating: r#struct.identifier.kind.clone(),
				},
				datatype.start,
				datatype.end,
			));
		}
		if let Some(s) = tree.structs.iter().find(|v| v.identifier.kind == datatype.kind) {
			if let Err(error) = validate_struct(tree, s, found) {
				if let ValidationError::RecursiveStructFields { within, violating } = error.kind {
					return Err(Error::new(
						ValidationError::RecursiveStructFields { within, violating },
						error.location,
						error.length,
					));
				}
			}
		}
	}

	Ok(())
}

pub fn validate_ast(tree: AST) -> Result<(), Error<ValidationError>> {
	for function in &tree.functions {
		let mut variables = vec![];
		for (identifier, datatype) in &function.parameters {
			if !BUILTINS.contains(&&datatype.kind[..])
				&& !tree.structs.iter().any(|v| v.identifier.kind == datatype.kind)
			{
				return Err(Error::span(
					ValidationError::UndeclaredIdentifier(datatype.kind.clone()),
					datatype.start,
					datatype.end,
				));
			}
			variables.push((identifier.kind.clone(), datatype.kind.clone()));
		}
		validate_block(&function.statements, &tree, variables, &function.return_type.kind)?;
	}

	for r#struct in &tree.structs {
		validate_struct(&tree, r#struct, &mut vec![])?;
	}

	Ok(())
}
