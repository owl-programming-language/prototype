mod untyped { //λ
	enum Term { //constructors
		Var(String),// let x = 1; x
		Abstraction(String, Box<Term>), // let f = (x) => x; f
		Application(Box<Term>, Box<Term>), // let f = (x) => x; f(1)
	}

	fn substitute(term: Term, name: String, value: Term) -> Term {
		match term {
			Term::Var(n) => {
				if n == name {
					value
				} else {
					Term::Var(n)
				}
			},
			Term::Abstraction(n, body) => {
				Term::Abstraction(n, Box::new(substitute(*body, name, value)))
			},
			Term::Application(func, arg) => {
				Term::Application(
					Box::new(substitute(*func, name.clone(), value)),
					Box::new(substitute(*arg, name.clone(), value)),
				)
			},
		}
	}

	fn eval(term: Term) -> Term {
		match term {
			Term::Var(name) => Term::Var(name),
			Term::Abstraction(name, body) => Term::Abstraction(name, body),
			Term::Application(func, arg) => {
				match *func {
					Term::Abstraction(name, body) => {
						eval(substitute(*body, name, *arg))
					},
					_ => Term::Application(func, arg),
				}
			},
		}
	}
}

mod simple_typed { //λ→
	pub trait T {
		fn formation() {}
		fn introduction() {}
		fn elimination() {}
		fn computation() {}
		fn uniqueness() {}
	}

	struct Bool {}

	impl T for Bool {
		fn formation() {}
		fn introduction() {}
		fn elimination() {}
		fn computation() {}
		fn uniqueness() {}
	}

	struct Int {}

	impl T for Int {
		fn formation() {}
		fn introduction() {}
		fn elimination() {}
		fn computation() {}
		fn uniqueness() {}
	}

	enum PrimitiveType {
		Bool,
		Int,
	}

	impl T for PrimitiveType {
		fn formation() {}
		fn introduction() {}
		fn elimination() {}
		fn computation() {}
		fn uniqueness() {}
	}

	enum Type {
		Primitive(PrimitiveType),
		Arrow(Box<Type>, Box<Type>),
	}

	impl T for Type {
		fn formation() {}
		fn introduction() {}
		fn elimination() {}
		fn computation() {}
		fn uniqueness() {}
	}

	enum Term {
		Var(String, Type),
		Abstraction(String, Type, Box<Term>),
		Application(Box<Term>, Box<Term>),
	}

	#[derive(Clone)]
	enum AST {
		Number(i32),
		String(String),
		Let(String, Box<AST>),
		Var(String),
		Abstraction(String, Box<AST>),
		Application(Box<AST>, Box<AST>),
		If(Box<AST>, Box<AST>, Box<AST>),
	}

	fn test() {
		let ast: Vec<AST> = vec![
			AST::Let(String::from("x"), Box::new(AST::Number(1))),
			AST::Abstraction(String::from("x"), Box::new(AST::Var(String::from("x")))),
			AST::Application(
				Box::new(AST::Abstraction(String::from("x"), Box::new(AST::Var(String::from("x"))))),
				Box::new(AST::Var(String::from("x"))),
			),
		];

	}

}

mod second_order_typed { //λ2
	enum PrimitiveType {
		Bool,
		Int,
	}

	enum T2 {
		Primitive(PrimitiveType),
		Arrow(Box<T2>, Box<T2>),
		ForAll(Box<T2>),
	}

	enum Term {
		Var(String, T2),
		Abstraction(String, T2, Box<Term>),
		Application(Box<Term>, Box<Term>),
		TypeAbstraction(String, Box<Term>),
		TypeApplication(Box<Term>, T2),
	}
}

mod types_dependent_on_types { //λω

}

mod types_dependent_on_terms { //λP

}