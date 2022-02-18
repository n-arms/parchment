mod expr;
mod gen;
mod lexer;
mod lift;
mod parser;
mod solve;
mod sub;
mod token;
mod types;
mod wasm;
mod codegen;

use gen::generate;
use im::HashSet;
use lift::lift;
use solve::solve;
use std::io::{self, BufRead, Write};
use std::panic::{catch_unwind, RefUnwindSafe};
use types::{Apply, Type, TypeVarSet, VarSet};
use wasm::WATFormatter;
use codegen::emit_program;
use std::fs::write;
use std::process::Command;

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

    let p = lift(&ast, &[], &VarSet::default(), &VarSet::default());
    println!("{:#?}", p);
    println!("evaluates to {}", reinterpret_as_f64(eval_wasm(&emit_program(p.unwrap()))));

    let tvs = TypeVarSet::default();
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

fn eval_wasm(w: &wasm::Wasm) -> i64 {
    let mut f = WATFormatter::default();
    w.format(&mut f);
    write("temp.wat", f.to_string()).unwrap();
    Command::new("wat2wasm")
        .arg("temp.wat")
        .output()
        .unwrap();
    let out = Command::new("node")
        .arg("runwasm.js")
        .output()
        .unwrap()
        .stdout;
    String::from_utf8_lossy(&out).parse().unwrap()
}

fn reinterpret_as_f64(bytes: i64) -> f64 {
    f64::from_ne_bytes(bytes.to_ne_bytes())
}

fn main() {
    repl(|text| {
        println!(":: {}", process(text));
    });
}
