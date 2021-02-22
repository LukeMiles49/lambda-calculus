use super::{NameGenerator};

use std::{
	iter::Peekable,
	str::Chars,
	fmt::{self, Display},
	rc::Rc,
};
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
					let reserved = reserved.union(body.free());
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
	
	pub fn to_cl(self: Rc<LambdaExpr>) -> Rc<LambdaExpr> {
		match &*self {
			LambdaExpr::Var(name) => Rc::new(LambdaExpr::Var(name.clone())),
			LambdaExpr::Apply(lhs, rhs) => Rc::new(LambdaExpr::Apply(lhs.clone().to_cl(), rhs.clone().to_cl())),
			LambdaExpr::Lambda(arg, body) => {
				match &*body.clone().to_cl() {
					LambdaExpr::Var(name) if name == arg =>
						Rc::new(LambdaExpr::Apply(
							Rc::new(LambdaExpr::Apply(
								Rc::new(LambdaExpr::Var("S".into())),
								Rc::new(LambdaExpr::Var("K".into())))),
							Rc::new(LambdaExpr::Var("K".into())))),
					body if !body.free().contains(arg) =>
						Rc::new(LambdaExpr::Apply(
							Rc::new(LambdaExpr::Var("K".into())),
							Rc::new(body.clone()))),
					LambdaExpr::Apply(lhs, rhs) =>
						Rc::new(LambdaExpr::Apply(
							Rc::new(LambdaExpr::Apply(
								Rc::new(LambdaExpr::Var("S".into())),
								Rc::new(LambdaExpr::Lambda(arg.clone(), lhs.clone())).to_cl())),
							Rc::new(LambdaExpr::Lambda(arg.clone(), rhs.clone())).to_cl())),
					_ => panic!("Invalid CL formula returned by to_cl"),
				}
			},
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
