pub mod lexer;
pub mod parser;

use std::fs::read;

fn main() {
	let contents = match read_file("main.leaf") {
		Ok(contents) => contents,
		Err(error) => {
			eprintln!("Failed to read contents of file: {}", error);
			return;
		}
	};

	let lexer = lexer::Lexer::new(contents.clone());
	let tokens = lexer.lex_file();
	let ast = match parser::Parser::new(tokens).parse_file() {
		Ok(ast) => ast,
		Err(error) => return eprintln!("{}", parser::error::display_error(error, &contents, "main.leaf")),
	};
	if let Err(error) = parser::validate::validate_ast(ast) {
		eprintln!("{}", parser::error::display_error(error, &contents, "main.leaf"));
	}
}

fn read_file(path: &str) -> Result<String, Box<dyn std::error::Error>> {
	let bytes = read(path)?;
	Ok(String::from_utf8(bytes)?)
}
