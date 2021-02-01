#![feature(box_syntax)]
#![feature(box_patterns)]

use std::rc::Rc;
use std::iter::Peekable;
use std::str::Chars;
use std::fmt::{self, Display};
use im::hashset::HashSet;

pub type Name = String;

#[derive(Debug, Clone)]
pub enum LambdaExpr {
	Var(Name),
	Apply(Rc<LambdaExpr>, Rc<LambdaExpr>),
	Lambda(Name, Rc<LambdaExpr>),
}

impl LambdaExpr {
	pub fn free(self: &LambdaExpr) -> HashSet<Name> {
		match &*self {
			LambdaExpr::Var(name) => HashSet::unit(name.clone()),
			LambdaExpr::Apply(lhs, rhs) => HashSet::union(lhs.free(), rhs.free()),
			LambdaExpr::Lambda(arg, body) => body.free().without(arg),
		}
	}
	
	pub fn sub(self: Rc<LambdaExpr>, var: &Name, expr: Rc<LambdaExpr>) -> Rc<LambdaExpr> {
		match &*self {
			LambdaExpr::Var(name) =>
				if name == var { expr }
				else { self },
			LambdaExpr::Apply(lhs, rhs) => Rc::new(LambdaExpr::Apply(
				lhs.clone().sub(var, expr.clone()),
				rhs.clone().sub(var, expr.clone()),
			)),
			LambdaExpr::Lambda(arg, body) => {
				let reserved = expr.free().update(var.clone());
				if reserved.contains(arg) {
					let new_arg = NameGenerator::new().find(|n| !reserved.contains(n)).unwrap();
					Rc::new(LambdaExpr::Lambda(new_arg.clone(),
						body.clone()
							.sub(&arg, Rc::new(LambdaExpr::Var(new_arg.clone())))
							.sub(&var, expr)))
				}
				else { Rc::new(LambdaExpr::Lambda(arg.clone(), body.clone().sub(var, expr.clone()))) }
			}
		}
	}
	
	pub fn parse(input: &mut Peekable<Chars>) -> Result<Rc<LambdaExpr>, String> {
		fn read_name(input: &mut Peekable<Chars>) -> Name {
			let mut name = String::new();
			while let Some(c) = input.peek() {
				if !c.is_ascii_alphanumeric() { break; }
				name.push(input.next().unwrap())
			}
			name
		}
		
		enum Tok {
			Par,
			Args(Stack<Name>),
			Expr(Rc<LambdaExpr>),
		}
		
		enum Stack<T> {
			Nil,
			Item(Box<(T, Self)>),
		}
		
		let mut stack = Stack::Nil;
		
		fn push_expr(stack: Stack<Tok>, expr: Rc<LambdaExpr>) -> Stack<Tok> {
			if let Stack::Item(box (Tok::Expr(prev), rest)) = stack {
				Stack::Item(box (Tok::Expr(Rc::new(LambdaExpr::Apply(prev.clone(), expr))), rest))
			}
			else { Stack::Item(box (Tok::Expr(expr), stack)) }
		}
		
		loop {
			match input.peek() {
				Some(c) if c.is_whitespace() => { input.next(); },
				Some('(') => { input.next(); stack = Stack::Item(box (Tok::Par, stack)); },
				Some(')') => {
					input.next();
					loop {
						if let Stack::Item(box (Tok::Expr(expr), Stack::Item(box (next, rest)))) = stack {
							stack = rest;
							match next {
								Tok::Par => { stack = push_expr(stack, expr); break; },
								Tok::Args(mut args) => {
									let mut expr = expr;
									while let Stack::Item(box (arg, rest)) = args {
										args = rest;
										expr = Rc::new(LambdaExpr::Lambda(arg, expr));
									}
									stack = push_expr(stack, expr);
								},
								_ => panic!("Invalid stack state"),
							}
						} else { return Err("Invalid parenthesis".into()) }
					}
				},
				Some('\\') => {
					input.next();
					let mut args = Stack::Nil;
					while let Some(c) = input.peek() {
						match c {
							c if c.is_whitespace() => { input.next(); },
							c if c.is_ascii_alphabetic() => { args = Stack::Item(box (read_name(input), args)); },
							'.' => { input.next(); break; }
							_ => return Err(format!("Unexpected character '{}' in input", c)),
						}
					}
					stack = Stack::Item(box (Tok::Args(args), stack));
				},
				Some(c) if c.is_ascii_alphabetic() => {
					stack = push_expr(stack, Rc::new(LambdaExpr::Var(read_name(input))));
				},
				None => {
					loop {
						if let Stack::Item(box (Tok::Expr(expr), rest)) = stack {
							match rest {
								Stack::Nil => return Ok(expr),
								Stack::Item(box (next, rest)) => {
									stack = rest;
									match next {
										Tok::Par => { return Err("Invalid parenthesis".into()) },
										Tok::Args(mut args) => {
											let mut expr = expr;
											while let Stack::Item(box (arg, rest)) = args {
												args = rest;
												expr = Rc::new(LambdaExpr::Lambda(arg, expr));
											}
											stack = push_expr(stack, expr);
										},
										_ => panic!("Invalid stack state"),
									}
								},
							}
						}
						else { return Err("Unexpected end of input".into()) }
					}
				},
				Some(c) => return Err(format!("Unexpected character '{}' in input", c)),
			}
		}
	}
}

impl Display for LambdaExpr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			LambdaExpr::Var(name) => write!(f, "{}", name),
			LambdaExpr::Apply(lhs, rhs) => write!(f, "({} {})", lhs, rhs),
			LambdaExpr::Lambda(name, body) => write!(f, "(\\{}.{})", name, body),
		}
	}
}

pub trait ReductionStrategy {
	fn reduce_step(expr: Rc<LambdaExpr>) -> Option<Rc<LambdaExpr>>;
	
	fn reduce_full(mut expr: Rc<LambdaExpr>) -> Rc<LambdaExpr> {
		loop {
			if let Some(new_expr) = Self::reduce_step(expr.clone()) { expr = new_expr; }
			else { break expr; }
		}
	}
}

pub enum LazyReduction { }

impl ReductionStrategy for LazyReduction {
	fn reduce_step(expr: Rc<LambdaExpr>) -> Option<Rc<LambdaExpr>> {
		if let LambdaExpr::Apply(lhs, rhs) = (*expr).clone() {
			if let LambdaExpr::Lambda(arg, body) = (*lhs).clone() { Some(body.sub(&arg, rhs)) }
			else { Some(Rc::new(LambdaExpr::Apply(Self::reduce_step(lhs)?, rhs))) }
		} else { None }
	}
}

pub struct NameGenerator {
	current: String,
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
	type Item = String;
	
	fn next(&mut self) -> Option<String> {
		self.increment();
		Some(self.current.clone())
	}
}
