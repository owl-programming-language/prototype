use std::collections::{ BTreeMap, HashMap };

#[derive(Debug, PartialEq)]
pub enum Token {
    Atom(String),
    OpenParen,
    CloseParen,
    Lambda,
    Arrow,
}

type Symbol = String;

/// A Scheme Number is implemented as an enum to handle both integers and floats.
#[derive(Debug, Clone, PartialEq)]
enum Number {
    Int(i64),
    Float(f64),
}

/// A Scheme Atom is either a Symbol or a Number.
#[derive(Debug, Clone, PartialEq)]
enum Atom {
    Symbol(Symbol),
    Number(Number),
}

/// A Scheme List is a vector of Scheme expressions.
type List = Vec<Exp>;

/// A Scheme expression (Exp) is either an Atom or a List.
#[derive(Debug, Clone, PartialEq)]
enum Exp {
    Atom(Atom),
    List(List),
}

/// A Scheme environment is a mapping of {variable: value}.
type Env = HashMap<Symbol, Exp>;

pub fn init() -> Env {
  let mut env=  HashMap::new();
  env.insert("+".to_string(), Exp::Atom(Atom::Symbol("+".to_string())));
  env.insert("-".to_string(), Exp::Atom(Atom::Symbol("-".to_string())));
  env.insert("*".to_string(), Exp::Atom(Atom::Symbol("*".to_string())));
  env.insert("/".to_string(), Exp::Atom(Atom::Symbol("/".to_string())));
  env
}

pub fn eval_symbol(s: Symbol, env: Env) -> Exp {
  match env.get(&s) {
    Some(v) => v.clone(),
    None => panic!("Symbol not found: {}", s),
  }
}

pub fn eval_number(n: Number, env: Env) -> Exp {
  Exp::Atom(Atom::Number(n))
}

pub fn eval_list(l: List, env: Env) -> Exp {
  let x0 = l.get(0).unwrap();
  match x0 {
      "if" => eval_conditional(l, env),
      "define" => eval_definition(l, env),
      _ => eval_application(l, env),
  }
}

pub fn eval_conditional(l: List, env: Env) -> Exp {
  let test = eval(l.get(1).unwrap(), env.clone());
  match test {
    Exp::Atom(Atom::Symbol(s)) => {
      match s.as_str() {
        "true" => eval(l.get(2).unwrap(), env.clone()),
        "false" => eval(l.get(3).unwrap(), env.clone()),
        _ => panic!("Expected boolean"),
      }
    }
    _ => panic!("Expected boolean"),
  }
}

pub fn eval_definition(l: List, env: Env) -> Exp {
  let var = l.get(1).unwrap();
  let val = l.get(2).unwrap();
  let mut env = env.clone();
  env.insert(var, val);
  Exp::Atom(Atom::Symbol("ok".to_string()))
}

pub fn eval_application(l: List, env: Env) -> Exp {
  let mut l = l;
  let op = l.remove(0);
  match op {
    Exp::Atom(Atom::Symbol(s)) => {
      match s.as_str() {
        "+" => {
          let mut sum = 0;
          for e in l {
            match eval(e, env.clone()) {
              Exp::Atom(Atom::Number(Number::Int(n))) => sum += n,
              _ => panic!("Expected number"),
            }
          }
          Exp::Atom(Atom::Number(Number::Int(sum)))
        }
        "-" => {
          let mut diff = match eval(l.remove(0), env.clone()) {
            Exp::Atom(Atom::Number(Number::Int(n))) => n,
            _ => panic!("Expected number"),
          };
          for e in l {
            match eval(e, env.clone()) {
              Exp::Atom(Atom::Number(Number::Int(n))) => diff -= n,
              _ => panic!("Expected number"),
            }
          }
          Exp::Atom(Atom::Number(Number::Int(diff)))
        }
        "*" => {
          let mut prod = 1;
          for e in l {
            match eval(e, env.clone()) {
              Exp::Atom(Atom::Number(Number::Int(n))) => prod *= n,
              _ => panic!("Expected number"),
            }
          }
          Exp::Atom(Atom::Number(Number::Int(prod)))
        }
        "/" => {
          let mut quot = match eval(l.remove(0), env.clone()) {
            Exp::Atom(Atom::Number(Number::Int(n))) => n,
            _ => panic!("Expected number"),
          };
          for e in l {
            match eval(e, env.clone()) {
              Exp::Atom(Atom::Number(Number::Int(n))) => quot /= n,
              _ => panic!("Expected number"),
            }
          }
          Exp::Atom(Atom::Number(Number::Int(quot)))
        }
        _ => panic!("Unknown operator: {}", s),
      }
    }
    _ => panic!("Expected operator"),
  }
}

pub fn eval(x: Exp, env: Env) -> Exp {
  match x {
    Exp::Atom(Atom::Symbol(s)) => eval_symbol(s, env),
    Exp::Atom(Atom::Number(n)) => eval_number(n, env),
    Exp::List(l) => eval_list(l, env),
  }
}

// pub fn eval_variable(env: & mut BTreeMap<String, String>, a: String, b: String) -> String {

// }

pub fn eval_constant() {

}

pub fn tokenize(content: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = content.chars().peekable();
    let mut map = BTreeMap::new();
    map.insert("add".to_string(), builtin::add);

    while let Some(&c) = chars.peek() {
        match c {
            'a'..='z' | 'A'..='Z' => {
                let mut identifier = String::new();
                while let Some(&c) = chars.peek() {
                    match c {
                        'a'..='z' | 'A'..='Z' => {
                            identifier.push(c);
                            chars.next();
                        }
                        _ => break,
                    }
                }
                tokens.push(Token::Atom(identifier));
            }
            '(' => {
                tokens.push(Token::OpenParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::CloseParen);
                chars.next();
            }
            '\\' => {
                tokens.push(Token::Lambda);
                chars.next();
            }
            '-' => {
                chars.next();
                if let Some('>') = chars.peek() {
                    tokens.push(Token::Arrow);
                    chars.next();
                } else {
                    panic!("Unexpected character: -");
                }
            }
            _ => {
                chars.next();
            }
        }
    }

    tokens
}
