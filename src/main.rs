use std::{env, result};
// pub mod owl;
pub mod p;

fn main() {
    let args: Vec<String> = env::args().collect();
    let [_, filename] = args.as_slice() else {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    };

    let content = std::fs::read_to_string(filename).unwrap();
    // dbg!(p::tokenize(&content));
    let exp = p::parse(&content);
    dbg!(exp.clone());
    let result = p::eval(&exp, &mut p::init());
    dbg!(result);
}