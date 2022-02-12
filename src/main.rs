mod expr;
mod gen;
mod lexer;
mod parser;
mod solve;
mod sub;
mod token;
mod types;

use gen::generate;
use solve::solve;
use std::io::{self, BufRead, Write};
use types::{apply, TypeEnv, TypeVarSet};

fn type_repl(line: &str) {
    let tokens = lexer::scan(line);
    let e = parser::parse_expr(&tokens)
        .clone()
        .unwrap()
        .unwrap()
        .0;
    println!("expr: {}", e);
    let (c, t) = generate(&e, &TypeVarSet::new(), TypeEnv::new()).unwrap();
    for constr in c.iter() {
        println!("const: {}", constr);
    }
    println!("gen type: {}", t);
    let s = solve(c).unwrap();
    println!("final: {}", apply(&t, &s));
}

fn main() {
    //type_repl("fn x -> match x with n -> if n then 3 else 4 end");
    print!("> ");
    io::stdout().flush().unwrap();
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        if line.as_ref().map(|line| line == "quit").unwrap() {
            break;
        }

        type_repl(&line.expect("stdin read failed"));
        print!("> ");
        io::stdout().flush().unwrap();
    }
}
