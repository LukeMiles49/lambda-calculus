use super::{ReductionStrategy, LambdaExpr};

use std::rc::Rc;

pub enum LazyReduction { }

impl ReductionStrategy for LazyReduction {
	fn reduce_step(expr: Rc<LambdaExpr>) -> Option<Rc<LambdaExpr>> {
		if let LambdaExpr::Apply(lhs, rhs) = (*expr).clone() {
			if let LambdaExpr::Lambda(arg, body) = (*lhs).clone() { Some(body.sub(&arg, rhs)) }
			else { Some(Rc::new(LambdaExpr::Apply(Self::reduce_step(lhs)?, rhs))) }
		} else { None }
	}
}
