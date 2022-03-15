#![warn(clippy::all, clippy::pedantic, clippy::restriction)]
#![allow(
    clippy::missing_docs_in_private_items,
    clippy::implicit_return,
    clippy::shadow_reuse,
    clippy::print_stdout,
    clippy::wildcard_enum_match_arm,
    clippy::else_if_without_else,
    clippy::integer_arithmetic,
    clippy::cast_lossless,
    clippy::unreachable,
    clippy::cast_possible_wrap,
    clippy::as_conversions,
    clippy::use_debug,
    clippy::self_named_module_files
)]

use code_gen::{
    code_gen::emit_program,
    lift::lift,
    wasm::{WATFormatter, Wasm},
};
use expr::{
    expr::Expr,
    lexer::scan,
    parser::parse,
    types::{Apply, Type},
};
use std::fs::write;
use std::io::{self, BufRead};
use std::process::{Command, Output};
use type_checker::{generate, solve, solve::solve};

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
    lift_debug: bool,
    eval: bool,
}

fn parse_ast(lines: &str) -> Option<Expr<()>> {
    match parse(&scan(lines)) {
        Ok(a) => Some(a),
        Err(es) => {
            for e in es {
                println!("{:?}", e);
            }
            None
        }
    }
}

fn infer_types(ast: &Expr<()>, debug: bool) -> Option<Expr<Type>> {
    let st = generate::State::default();
    let (a, ast) = match st.generate(ast) {
        Ok(s) => s,
        Err(e) => {
            println!("{:?}", e);
            return None;
        }
    };

    if debug {
        println!("{}\nbase type\n\t{}", st, ast.get_type());
    }

    if !a.is_empty() {
        println!("non empty assumption set:");
        for assum in a {
            println!("{}", assum);
        }
        return None;
    }

    let (type_vars, constraints) = st.extract();

    let s = match solve(&constraints, solve::State::new(type_vars)) {
        Ok(cs) => cs,
        Err(e) => {
            println!("{:?}", e);
            return None;
        }
    };

    if debug {
        println!("\nsubstitutions");
        for (tv, t) in &s {
            println!("\t{} => {}", tv, t);
        }
        println!();
    }

    let final_expr = ast.apply(&s);
    println!(":: {}", final_expr.get_type());
    Some(final_expr)
}

fn generate_wasm(typed_ast: &Expr<Type>, debug: bool) -> Wasm {
    let lifted = lift(typed_ast, &[], &mut code_gen::lift::State::default());

    if debug {
        println!("{:#?}", lifted);
    }

    emit_program(lifted)
}

fn run_wasm(wasm: &Wasm, result_type: Type) -> Option<String> {
    let mut w = WATFormatter::default();
    wasm.format(&mut w);

    if let Err(e) = write("temp.wat", w.to_string()) {
        println!("{:?}", e);
        return None;
    }

    let Output {
        status: convert_status,
        ..
    } = match Command::new("wat2wasm").arg("temp.wat").output() {
        Ok(o) => o,
        Err(e) => {
            println!("{:?}", e);
            return None;
        }
    };
    if !convert_status.success() {
        println!("wat2wasm failed, try running `wat2wasm temp.wat` on your own machine");
        return None;
    }

    let Output {
        status: run_status,
        stdout: run_output,
        ..
    } = match Command::new("node").arg("runwasm.js").output() {
        Ok(o) => o,
        Err(e) => {
            println!("{:?}", e);
            return None;
        }
    };

    if !run_status.success() {
        println!("failed to run the generated .wasm file, try running `node runwasm.js` on your own machine");
        return None;
    }

    let num = run_output
        .into_iter()
        .fold(0, |total, digit| total * 10 + (digit as u64 - 48));

    Some(match result_type {
        Type::Arrow(_, _) => String::from("<fun>"),
        Type::Constant(c, _) if c.as_ref() == "Num" => {
            f64::from_ne_bytes(num.to_ne_bytes()).to_string()
        }
        Type::Constant(c, _) if c.as_ref() == "Bool" => {
            String::from(if num == 0 { "false" } else { "true" })
        }
        Type::Constant(c, _) if c.as_ref() == "Unit" => String::from("()"),
        _ => {
            println!("I don't know how to display the type {}", result_type);
            return None;
        }
    })
}

fn process_text(lines: &str, state: &ReplState) -> Option<()> {
    let ast = parse_ast(lines)?;

    let typed_ast = infer_types(&ast, state.type_debug)?;

    let wasm = generate_wasm(&typed_ast, state.lift_debug);

    if state.eval {
        println!("= {}", run_wasm(&wasm, typed_ast.get_type())?);
    }

    Some(())
}

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines().map(Result::unwrap);
    let mut state = ReplState {
        type_debug: false,
        lift_debug: false,
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
            "+liftdebug" => state.lift_debug = true,
            "-liftdebug" => state.lift_debug = false,
            "+eval" => state.eval = true,
            "-eval" => state.eval = false,
            "quit" => {
                println!("thank you for using parchment.");
                return Ok(());
            }
            _ => {
                process_text(&read_ast(next, &mut lines), &state);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use code_gen::lift;

    fn eval(text: &str) -> u64 {
        let untyped = parse(&scan(text)).unwrap();
        let state = generate::State::default();
        let (a, e1) = state.generate(&untyped).unwrap();
        assert!(a.is_empty());

        let (type_vars, constraints) = state.extract();

        let sub = solve(&constraints, solve::State::new(type_vars)).unwrap();

        let typed = e1.apply(&sub);
        let lifted = lift(&typed, &[], &mut lift::State::default());
        let wat = emit_program(lifted);
        let mut fmt = WATFormatter::default();
        wat.format(&mut fmt);

        write("./temp.wat", fmt.to_string()).unwrap();
        assert!(Command::new("wat2wasm")
            .arg("./temp.wat")
            .spawn()
            .unwrap()
            .wait()
            .unwrap()
            .success());
        let Output { status, stdout, .. } =
            Command::new("node").arg("runwasm.js").output().unwrap();
        assert!(status.success());

        stdout
            .into_iter()
            .fold(0, |total, digit| total * 10 + (digit as u64 - 48))
    }

    fn cast_f64(num: u64) -> f64 {
        f64::from_ne_bytes(num.to_ne_bytes())
    }

    fn cast_bool(num: u64) -> bool {
        num != 0
    }

    #[test]
    fn full_stack() {
        assert_eq!(
            6.,
            cast_f64(eval(
                r#"
                {
                    let sub1 = fn x -> x - 1;
                    let is_zero = fn x -> x <= 0.01;
                    let fact = fn x -> if is_zero x then 1 else x * (fact (sub1 x));
                    if (is_zero (sub1 42)) then 3 else (fact 3);
                }
            "#
            ))
        );

        assert_eq!(
            12.,
            cast_f64(eval(
                r#"
                {
                    let {a:(x, y), b:z} = {a: (3, 4), b: 5};
                    x + y + z;
                }
            "#
            ))
        );

        assert_eq!(
            false,
            cast_bool(eval(
                r#"
                {
                    let is_zero = fn x -> x <= 0.01;
                    let not = fn x -> if x then false else true;
                    let sub1 = fn x -> x - 1;
                    let is_even = fn x -> if is_zero x then true else not (is_even (sub1 x));
                    is_even 7;
                }
            "#
            ))
        );

        assert_eq!(
            0.,
            cast_f64(eval(
                r#"
                {
                    type maybe = Just a | Nothing;
                    Just 5;
                    Just true;
                    0;
                }
            "#
            ))
        );
    }
}
