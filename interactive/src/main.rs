use std::io::stdin;
use lambda_calculus::*;

fn main() {
	let stdin = stdin();
	
	loop {
		let mut input = String::new();
		
		println!();
		println!("Enter lambda calculus expression:");
		stdin.read_line(&mut input).unwrap();
		println!();
		
		match LambdaExpr::parse(&mut input.chars().peekable()) {
			Ok(mut expr) => {
				println!("{}", expr);
				while let Some(new_expr) = LazyReduction::reduce_step(expr.clone()) {
					stdin.read_line(&mut input).unwrap();
					expr = new_expr;
					println!("{}", expr);
				}
			},
			Err(msg) => println!("{}", msg),
		}
	}
}
