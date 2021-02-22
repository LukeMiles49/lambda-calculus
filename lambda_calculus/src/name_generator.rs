use super::Name;

pub struct NameGenerator {
	current: Name,
}

impl NameGenerator {
	pub fn new() -> Self {
		Self {
			current: "".into(),
		}
	}
	
	fn increment(&mut self) {
		match self.current.pop() {
			None => self.current.push('a'),
			Some('z') => { self.increment(); self.current.push('a'); },
			Some(c) => self.current.push((c as u8 + 1) as char),
		}
	}
}

impl Iterator for NameGenerator {
	type Item = Name;
	
	fn next(&mut self) -> Option<Name> {
		self.increment();
		Some(self.current.clone())
	}
}
