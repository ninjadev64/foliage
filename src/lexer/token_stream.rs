use super::Token;

/// A write-only stream of tokens that can be consumed only once.
#[derive(Default)]
pub struct TokenStream {
	tokens: Vec<Token>,
}

impl TokenStream {
	pub fn push(&mut self, token: Token) {
		self.tokens.push(token)
	}

	pub fn finish(self) -> Vec<Token> {
		self.tokens
	}
}
