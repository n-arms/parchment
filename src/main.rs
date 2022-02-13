mod expr;
mod gen;
mod lexer;
mod parser;
mod solve;
mod sub;
mod token;
mod types;

use gen::generate;
use im::HashSet;
use solve::solve;
use std::io::{self, BufRead, Write};
use std::panic::{catch_unwind, RefUnwindSafe};
use types::{Apply, Type, TypeVarSet};

fn read_input() -> String {
    let mut parens = 0isize;
    let mut braces = 0isize;
    let mut out = String::new();
    for line in io::stdin().lock().lines() {
        let l = line.unwrap();
        parens += l.chars().filter(|c| *c == '(').count() as isize;
        parens -= l.chars().filter(|c| *c == ')').count() as isize;
        braces += l.chars().filter(|c| *c == '{').count() as isize;
        braces -= l.chars().filter(|c| *c == '}').count() as isize;
        out.push_str(&l);

        if parens == 0 && braces == 0 {
            return out;
        }
    }
    unreachable!()
}

fn process(s: String) -> Type {
    let ast = parser::parse_expr(&lexer::scan(&s)).unwrap().unwrap().0;
    let tvs = TypeVarSet::new();
    let (a, c, t) = generate(&ast, &tvs, HashSet::new()).unwrap();
    assert!(a.is_empty());
    let s = solve(c.into_iter().collect(), &tvs).unwrap();
    t.apply(s)
}

fn repl<F: Fn(String) + RefUnwindSafe>(f: F) {
    loop {
        println!("====");
        io::stdout().flush().unwrap();
        let text = read_input();
        if text == "quit" {
            return;
        }
        #[allow(unused_must_use)]
        {
            catch_unwind(|| f(text));
        }
    }
}

fn main() {
    repl(|text| {
        println!(":: {}", process(text));
    });
}
