/// A helper struct for consecutively reading characters from a string of source code.
pub struct SourceCodeReader {
	pub pos: usize,
	contents: String,
}

impl SourceCodeReader {
	pub fn new(contents: String) -> Self {
		Self { pos: 0, contents }
	}

	pub fn read(&self) -> char {
		self.contents.chars().nth(self.pos).unwrap()
	}

	pub fn second(&self) -> char {
		self.contents.chars().nth(self.pos + 1).unwrap()
	}

	pub fn advance(&mut self, places: usize) {
		self.pos += places
	}

	/// Return whether there are any non-whitespace characters available.
	pub fn available(&self) -> bool {
		self.pos < self.contents.len() && !self.contents[self.pos..].trim().is_empty()
	}
}
