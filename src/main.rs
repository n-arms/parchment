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
use lift::{lift, ConstructorEnv};
use parser::parse;
use solve::solve;
use std::fs::write;
use std::io::{self, BufRead};
use std::process::{Command, Output};
use types::{Apply, Constructor, Type, VarSet, Variant};
use wasm::WATFormatter;

fn read_ast(first: String, lines: &mut impl Iterator<Item = String>) -> String {
    let mut out = String::new();
    let mut parens = 0;
    let mut braces = 0;
    for line in vec![first].into_iter().chain(lines) {
        parens += line.chars().filter(|c| *c == '(').count() as isize;
        parens -= line.chars().filter(|c| *c == ')').count() as isize;
        braces += line.chars().filter(|c| *c == '{').count() as isize;
        braces -= line.chars().filter(|c| *c == '}').count() as isize;
        out.push_str(&line);
        if braces == 0 && parens == 0 {
            return out;
        }
    }
    unreachable!()
}

pub struct ReplState {
    type_debug: bool,
    eval: bool,
}

fn process_text(lines: String, state: &ReplState) {
    let ast = match parse(&lexer::scan(&lines)) {
        Ok(a) => a,
        Err(es) => {
            for e in es {
                println!("{:?}", e);
            }
            return;
        }
    };

    let tvs = VarSet::default();
    let (a, c, t) = match generate(&ast, &tvs, HashSet::new()) {
        Ok(s) => s,
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    };
    if state.type_debug {
        println!("constraints");
        for cons in &c {
            println!("\t{}", cons);
        }
        println!("\nbase type\n\t{}", t);
    }

    if !a.is_empty() {
        println!("non empty assumption set:");
        for (v, t) in a {
            println!("{} : {}", v, t);
        }
        return;
    }

    let s = match solve(c.iter().cloned().collect(), &tvs) {
        Ok(cs) => cs,
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    };

    if state.type_debug {
        println!("\nsubstitutions");
        for (tv, t) in &s {
            println!("\t{} => {}", tv, t);
        }
        println!();
    }

    let final_type = t.apply(s);
    println!(":: {}", final_type);

    if !state.eval {
        return;
    }

    let lifted = match lift(
        &ast,
        &[],
        &VarSet::default(),
        &VarSet::default(),
        &mut ConstructorEnv::default(),
    ) {
        Ok(l) => l,
        Err(()) => return,
    };

    let wasm = emit_program(lifted);
    let mut w = WATFormatter::default();
    wasm.format(&mut w);

    if let Err(e) = write("temp.wat", w.to_string()) {
        println!("{:?}", e);
        return;
    }

    let Output {
        status: convert_status,
        ..
    } = match Command::new("wat2wasm").arg("temp.wat").output() {
        Ok(o) => o,
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    };
    if !convert_status.success() {
        println!("wat2wasm failed, try running `wat2wasm temp.wat` on your own machine");
        return;
    }

    let Output {
        status: run_status,
        stdout: run_output,
        ..
    } = match Command::new("node").arg("runwasm.js").output() {
        Ok(o) => o,
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    };

    if !run_status.success() {
        println!("failed to run the generated .wasm file, try running `node runwasm.js` on your own machine");
        return;
    }

    let num = run_output
        .into_iter()
        .fold(0, |total, digit| total * 10 + (digit as u64 - 48));

    println!(
        "= {}",
        match final_type {
            Type::Arrow(_, _) => String::from("<fun>"),
            Type::Constructor(Constructor::Number) =>
                f64::from_ne_bytes(num.to_ne_bytes()).to_string(),
            Type::Constructor(Constructor::Boolean) =>
                String::from(if num == 0 { "false" } else { "true" }),
            Type::Constructor(Constructor::Unit) => String::from("()"),
            _ => {
                println!("I don't know how to display the type {}", final_type);
                return;
            }
        }
    );
}

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines().map(Result::unwrap);
    let mut state = ReplState {
        type_debug: false,
        eval: true,
    };
    loop {
        println!("====");
        let next = if let Some(l) = lines.next() {
            l
        } else {
            println!("thank you for using parchment.");
            return Ok(());
        };

        let stripped: String = next
            .chars()
            .filter(|c| !c.is_whitespace() && *c != ' ')
            .map(|c| c.to_ascii_lowercase())
            .collect();

        match &stripped[..] {
            "+typedebug" => state.type_debug = true,
            "-typedebug" => state.type_debug = false,
            "+eval" => state.eval = true,
            "-eval" => state.eval = false,
            "quit" => {
                println!("thank you for using parchment.");
                return Ok(());
            }
            _ => {
                process_text(read_ast(next, &mut lines), &state);
            }
        }
    }
}
