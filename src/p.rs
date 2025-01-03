use std::collections::{BTreeMap, HashMap, VecDeque};

type Symbol = String;
type F = fn(Exp, Exp) -> Exp;

/// A Scheme Number is implemented as an enum to handle both integers and floats.
#[derive(Debug, Clone, PartialEq)]
pub enum Number {
  Int(i64),
  Float(f64),
}

/// A Scheme Atom is either a Symbol or a Number.
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
  Symbol(Symbol),
  Number(Number),
  Function(F),
}

#[derive(Debug, PartialEq)]
pub enum Token {
  Atom(Atom),
  OpenParen,
  CloseParen,
  Lambda,
  Arrow,
}

/// A Scheme List is a vector of Scheme expressions.
type List = Vec<Exp>;

/// A Scheme expression (Exp) is either an Atom or a List.
#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
  Atom(Atom),
  List(List),
}

pub type Type<'a> = String;

/// A Scheme environment is a mapping of {variable: value}.
pub type Env<'a> = HashMap<Symbol, (Type<'a>, Exp)>;

pub fn init<'a>() -> Env<'a> {
  let mut env = HashMap::new();
  env.insert(
    "+".to_string(),
    (
      "int -> int -> int".to_string(),
      Exp::Atom(Atom::Function(|a, b| {
        match (a, b) {
          (Exp::Atom(Atom::Number(Number::Int(a))), Exp::Atom(Atom::Number(Number::Int(b)))) => {
            Exp::Atom(Atom::Number(Number::Int(a + b)))
          },
          _ => panic!("Invalid arguments"),
        }
      })),
    ),
  );
  env.insert(
    "-".to_string(),
    (
      "int -> int -> int".to_string(),
      Exp::Atom(Atom::Function(|a, b| {
        match (a, b) {
          (Exp::Atom(Atom::Number(Number::Int(a))), Exp::Atom(Atom::Number(Number::Int(b)))) => {
            Exp::Atom(Atom::Number(Number::Int(a - b)))
          },
          _ => panic!("Invalid arguments"),
        }
      })),
    ),
  );
  env.insert(
    "*".to_string(),
    (
      "int -> int -> int".to_string(),
      Exp::Atom(Atom::Function(|a, b| {
        println!("a: {:?}, b: {:?}", a, b);
        match (a, b) {
          (Exp::Atom(Atom::Number(Number::Int(a))), Exp::Atom(Atom::Number(Number::Int(b)))) => {
            Exp::Atom(Atom::Number(Number::Int(a * b)))
          },
          _ => panic!("Invalid arguments"),
        }
      })),
    ),
  );
  env.insert(
    "/".to_string(),
    (
      "int -> int -> int".to_string(),
      Exp::Atom(Atom::Function(|a, b| {
        match (a, b) {
          (Exp::Atom(Atom::Number(Number::Int(a))), Exp::Atom(Atom::Number(Number::Int(b)))) => {
            Exp::Atom(Atom::Number(Number::Int(a / b)))
          },
          _ => panic!("Invalid arguments"),
        }
      })),
    ),
  );
  env.insert(
    "begin".to_string(),
    (
      "int -> int -> bool".to_string(),
      Exp::Atom(Atom::Function(|a, b| {
        println!("a: {:?}, b: {:?}", a, b);
        match (a, b) {
          (_, last) => {
            println!("last: {:?}", last);
            last
          }
        }
      })),
    ),
  );
  env.insert("pi".to_string(), ("float".to_string(), Exp::Atom(Atom::Number(Number::Int(3)))));
  env
}

pub fn eval_variable_reference(s: &Exp, env: &Env) -> Exp {
  println!("eval_variable_reference: {:?}", s);
  println!("env: {:?}", env);
  let s = match s {
    Exp::Atom(Atom::Symbol(s)) => s,
    _ => panic!("Invalid variable reference"),
  };
  match env.get(s) {
    Some((t, v)) => v.clone(),
    None => panic!("Symbol not found: {}", s),
  }
}

// pub fn eval_number(n: Number) -> Exp {
//   match n {
//     Number::Int(i) => i,
//     Number::Float(f) => f,
//   }
// }

// pub fn eval_constant_literal() {}

pub fn eval_conditional(exp: &Exp, env: &mut Env ) -> Exp {
  let [cond, then, els] = match exp {
    Exp::List(l) => {
      let [_, cond, then, els] = &l[..] else {
        panic!("Invalid conditional expression");
      };
      [cond, then, els]
    },
    _ => panic!("Invalid conditional expression"),
  };

  let cond_result = eval(&cond, env);
  match cond_result {
    Exp::Atom(Atom::Number(Number::Int(0))) => eval(&els, env),
    Exp::Atom(Atom::Number(Number::Int(_))) => eval(&then, env),
    _ => panic!("Invalid conditional expression"),
  }
}

pub fn eval_definition(exp: &Exp, env: &mut Env) -> Exp {
  let [var, val] = match exp {
    Exp::List(l) => {
      let [_, var, val] = &l[..] else {
        panic!("Invalid definition expression");
      };
      [var, val]
    },
    _ => panic!("Invalid definition expression"),
  };

  let var_name = match var {
    Exp::Atom(Atom::Symbol(s)) => s,
    _ => panic!("Invalid definition expression"),
  };

  env.insert(var_name.clone(), ("any".to_string(), val.clone()));
  Exp::Atom(Atom::Symbol(var_name.clone()))
}

pub fn eval_application(exp: &Exp, env: &mut Env) -> Exp {
  println!("apply fn: {:?}", exp);
  let (fn_name, args) = match exp {
    Exp::List(l) => {
      println!("apply fn match: {:?}", l);
      let (fn_name, args) = l.split_first().expect("Invalid application expression1");
      (fn_name, args)
    },
    _ => panic!("Invalid application expression2"),
  };


  let proc = eval(&fn_name.clone(), env);
  println!("proc: {:?}", proc);
  let result = match proc {
    Exp::Atom(Atom::Function(f)) => {
      let mut args_evaluated = Vec::new();
      for arg in args {
        args_evaluated.push(eval(&arg, env));
      }
      f(args_evaluated[0].clone(), args_evaluated[1].clone())
    },
    _ => panic!("Invalid application expression3"),
  };
  result
}

pub fn eval(exp: &Exp, env: &mut Env) -> Exp {
  match exp {
    Exp::Atom(Atom::Symbol(s)) => eval_variable_reference(exp, env),
    Exp::Atom(Atom::Number(n)) => Exp::Atom(Atom::Number(n.clone())),
    Exp::Atom(Atom::Function(f)) => Exp::Atom(Atom::Function(f.clone())),
    Exp::List(l) => {
      if l.is_empty() {
        panic!("Empty list");
      }

      let ll = l.clone();

      let first = ll.first().unwrap();
      match first {
        Exp::Atom(Atom::Symbol(s)) => {
          match s.as_str() {
            "if" => eval_conditional(&Exp::List(ll), env),
            "define" => eval_definition(&Exp::List(ll), env),
            _ => eval_application(&Exp::List(ll), env),
          }
        },
        _ => panic!("Invalid expression"),
      }
    },
  }
}

pub fn tokenize(content: &str) -> Vec<String> {
  content
    .replace("(", " ( ")
    .replace(")", " ) ")
    .split_whitespace()
    .map(|s| s.to_string())
    .collect()
}

pub fn parse_atom(token: &str) -> Atom {
  if token.chars().all(char::is_numeric) {
    match token.parse::<i64>() {
      Ok(i) => Atom::Number(Number::Int(i)),
      Err(_) => match token.parse::<f64>() {
        Ok(f) => Atom::Number(Number::Float(f)),
        Err(_) => Atom::Symbol(token.to_string()),
      },
    }
  } else {
    Atom::Symbol(token.to_string())
  }
}

pub fn read_from_tokens(token_str_list: &mut VecDeque<String>) -> Exp {
  if token_str_list.is_empty() {
    panic!("Unexpected EOF");
  }

  match token_str_list.pop_front() {
    Some(token_str) => match token_str.as_str() {
      "(" => {
        let mut list: List = Vec::new();
        while let Some(token_str) = token_str_list.front() {
          if token_str != ")" {
            match read_from_tokens(token_str_list) {
              Exp::List(l) => list.push(Exp::List(l)),
              Exp::Atom(a) => list.push(Exp::Atom(a)),
            };
          } else {
            break;
          }
        }

        token_str_list.pop_front();
        return Exp::List(list);
      }
      ")" => {
        panic!("Unexpected )")
      },
      _ => {
        return Exp::Atom(parse_atom(&token_str))
      },
    },
    None => panic!("Unexpected EOF"),
  }
}

pub fn parse(program: &str) -> Exp {
  read_from_tokens(&mut VecDeque::from(tokenize(program)))
}
