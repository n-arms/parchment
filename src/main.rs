mod parser;
mod expr;
mod token;
mod lexer;
mod types;
mod infer;

use std::io::{self, BufRead, Write};

fn type_repl(line: &str) {
    let tokens = lexer::scan(line);
    let e = parser::parse_expr(&tokens);
    if let Ok(ast) = e {
        if ast.1 != [] {
            return println!("leftover text after parse: {:?}", ast);
        }
        let tr = infer::infer_type(ast.0.clone());
        if let Ok(t) = tr {
            println!("{} :: {}", ast.0, t);
        } else {
            println!("{} :: {:?}", ast.0, tr);
        }
    } else {
        println!("{:?}", e);
    }
}

fn main() {
    //type_repl("fn x -> match x with n -> if n then 3 else 4 end");
    print!("> ");
    io::stdout().flush().unwrap();
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        if line.as_ref().map(|line| line == "quit").unwrap() {break;}

        type_repl(&line.expect("stdin read failed"));
        print!("> ");
        io::stdout().flush().unwrap();
    }
}
