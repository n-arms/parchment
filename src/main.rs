mod codegen;
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

use codegen::emit_program;
use gen::generate;
use im::HashSet;
use lift::lift;
use solve::solve;
use std::fs::write;
use std::io::{self, BufRead, Write};
use std::panic::{catch_unwind, RefUnwindSafe};
use std::process::Command;
use types::{Apply, Type, TypeVarSet, VarSet};
use wasm::WATFormatter;

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

fn process(s: String) {
    let ast = parser::parse_expr(&lexer::scan(&s)).unwrap().unwrap().0;

    let p = lift(&ast, &[], &VarSet::default(), &VarSet::default());

    let tvs = TypeVarSet::default();
    let (a, c, t) = generate(&ast, &tvs, HashSet::new()).unwrap();
    assert!(a.is_empty());
    let s = solve(c.into_iter().collect(), &tvs).unwrap();
    let t = t.apply(s);
    println!("::{}", t);
    let res = eval_wasm(&emit_program(p.unwrap()));
    let res_fmt = match t {
        Type::Arrow(_, _) => String::from("<function>"),
        Type::Constructor(types::Constructor::Boolean) => String::from(if res == 0 {"true"} else {"false"}),
        Type::Constructor(types::Constructor::Number) => reinterpret_as_f64(res).to_string(),
        Type::Constructor(types::Constructor::Unit) => String::from("()"),
        Type::Record(_) => String::from("<record>"),
        Type::Variable(_) => unreachable!()
    };
    println!(
        "= {}",
        res_fmt
    );
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
    Command::new("wat2wasm").arg("temp.wat").output().unwrap();
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

fn type_debug(text: String) {
    let ast = parser::parse_expr(&lexer::scan(&text)).unwrap().unwrap().0;
    let tvs = TypeVarSet::default();
    let (a, c, t) = generate(&ast, &tvs, HashSet::new()).unwrap();
    println!("assumptions:");
    for (var, t) in &a {
        println!("\t{} :: {}", var, t);
    }
    println!("\nconstraints:");
    for constr in &c {
        println!("\t{}", constr);
    }
    assert!(a.is_empty());
    let s = solve(c.clone().into_iter().collect(), &tvs).unwrap();
    println!("{}\n:: {}", ast, t.apply(s));

}

fn main() {
    repl(type_debug);
}
