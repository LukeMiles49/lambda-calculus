#![feature(box_syntax)]
#![feature(box_patterns)]

use std::rc::Rc;

pub trait ReductionStrategy {
	fn reduce_step(expr: Rc<LambdaExpr>) -> Option<Rc<LambdaExpr>>;
	
	fn reduce_full(mut expr: Rc<LambdaExpr>) -> Rc<LambdaExpr> {
		loop {
			if let Some(new_expr) = Self::reduce_step(expr.clone()) { expr = new_expr; }
			else { break expr; }
		}
	}
}

mod name_generator;
use name_generator::NameGenerator;

mod lambda_expr;
pub use lambda_expr::{LambdaExpr, Name};

mod lazy_reduction;
pub use lazy_reduction::LazyReduction;
