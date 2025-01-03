use std::collections::{BTreeMap, HashMap, VecDeque};

type Symbol = String;

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
      Exp::Atom(Atom::Symbol("+".to_string())),
    ),
  );
  env.insert(
    "-".to_string(),
    (
      "int -> int -> int".to_string(),
      Exp::Atom(Atom::Symbol("-".to_string())),
    ),
  );
  env.insert(
    "*".to_string(),
    (
      "int -> int -> int".to_string(),
      Exp::Atom(Atom::Symbol("*".to_string())),
    ),
  );
  env.insert(
    "/".to_string(),
    (
      "int -> int -> int".to_string(),
      Exp::Atom(Atom::Symbol("/".to_string())),
    ),
  );
  env
}

pub fn eval_symbol(s: Symbol, env: Env) -> Exp {
  match env.get(&s) {
    Some((t, v)) => v.clone(),
    None => panic!("Symbol not found: {}", s),
  }
}

pub fn eval_number(n: Number, env: Env) -> Exp {
  Exp::Atom(Atom::Number(n))
}

pub fn eval_constant() {}

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
