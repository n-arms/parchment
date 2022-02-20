use super::lift::{self, locals, Expr, Program};
use super::wasm::{self, Instruction, Type, Value, Wasm};
use im::HashMap;

pub fn emit_program(p: Program) -> Wasm {
    let elems = p
        .defs
        .iter()
        .enumerate()
        .map(|(i, _)| i.to_string())
        .collect();

    let tables = vec![(p.defs.len(), String::from("funcref"))];

    let mut defs: Vec<_> = p.defs.into_iter().map(emit_function).collect();
    let mut main_locals: Vec<_> = locals(&p.main)
        .into_iter()
        .map(|name| (name, Type::I64))
        .collect();
    main_locals.extend([
        (String::from("g64"), Type::I64),
        (String::from("g32"), Type::I32),
    ]);
    defs.push(wasm::FunctionDef {
        args: Vec::new(),
        name: Some(String::from("main")),
        return_type: Type::I64,
        body: emit_expr(p.main),
        locals: main_locals,
    });

    Wasm {
        elem_offset: 0,
        elems,
        exports: vec![String::from("main")],
        globals: vec![(String::from("heap_top"), Type::I32, Value::I32(0))],
        memory: vec![(1, String::from("heap"))],
        types: HashMap::unit(
            String::from("all"),
            String::from("(func (param i64) (param i32) (result i64))"),
        ),
        tables,
        defs,
    }
}

pub fn emit_function(f: lift::FunctionDef) -> wasm::FunctionDef {
    let mut locals: Vec<_> = f.locals.into_iter().map(|name| (name, Type::I64)).collect();
    locals.extend([
        (String::from("g64"), Type::I64),
        (String::from("g32"), Type::I32),
    ]);
    wasm::FunctionDef {
        args: vec![(f.arg, Type::I64), (String::from("env"), Type::I32)],
        locals,
        name: None,
        return_type: Type::I64,
        body: emit_expr(f.body),
    }
}

pub fn emit_expr(e: Expr) -> Vec<Instruction> {
    match e {
        Expr::Application(e1, e2) => {
            let mut is = Vec::new();
            is.extend(emit_expr(*e2));
            is.extend(emit_expr(*e1));
            is.extend(call_closure());
            is
        }
        Expr::Variable(v) => vec![Instruction::GetLocal(v)],
        Expr::Number(n) => vec![
            Instruction::Const(Value::F64(n)),
            Instruction::Reinterpret(Type::I64, Type::F64),
        ],
        Expr::Boolean(b) => vec![Instruction::Const(Value::I64(if b { 1 } else { 0 }))],
        Expr::Closure(func, env) => {
            if env.is_empty() {
                make_function(func)
            } else {
                make_closure(func, env)
            }
        }
        Expr::EnvLookup(i) => vec![
            Instruction::GetLocal(String::from("env")),
            Instruction::Const(Value::I32(i as i32 * 8)),
            Instruction::Add(Type::I32),
            Instruction::Load(Type::I64),
        ],
        Expr::All(es) => es.into_iter().flat_map(emit_expr).collect(),
        Expr::Assign(v, e) => {
            let mut is = Vec::new();
            is.extend(emit_expr(*e));
            is.push(Instruction::SetLocal(v));
            is
        }
        Expr::Record(es) => make_record(es),
        Expr::RecordLookup(e, i) => {
            let mut is = Vec::new();
            is.extend(emit_expr(*e));
            is.extend([
                Instruction::Wrap(Type::I32, Type::I64),
                Instruction::Const(Value::I32(i as i32 * 8)),
                Instruction::Add(Type::I32),
                Instruction::Load(Type::I64),
            ]);
            is
        }
        Expr::If(p, c, a) => {
            let mut is = Vec::new();
            is.extend(emit_expr(*p));
            is.extend([
                Instruction::Wrap(Type::I32, Type::I64),
                Instruction::If(emit_expr(*c), emit_expr(*a), Type::I64),
            ]);
            is
        }
        Expr::Ignore(_) => todo!(),
    }
}

fn malloc(bytes: usize) -> Vec<Instruction> {
    vec![
        Instruction::GetGlobal(String::from("heap_top")),
        Instruction::GetGlobal(String::from("heap_top")),
        Instruction::Const(Value::I32(bytes as i32)),
        Instruction::Add(Type::I32),
        Instruction::SetGlobal(String::from("heap_top")),
    ]
}

/// evaluate all the expressions in e, then put their results on the heap, in order, and return a
/// pointer to the start
fn allocate_all(es: Vec<Expr>) -> Vec<Instruction> {
    let mut is = Vec::new();
    is.extend(malloc(es.len() * 8));
    for (i, e) in es.into_iter().enumerate() {
        is.extend(duplicate(Type::I32));
        is.extend([
            Instruction::Const(Value::I32(i as i32 * 8)),
            Instruction::Add(Type::I32),
        ]);
        is.extend(emit_expr(e));
        is.push(Instruction::Store(Type::I64));
    }
    is
}

fn make_closure(func: usize, env: Vec<Expr>) -> Vec<Instruction> {
    let mut is = Vec::new();
    is.extend(allocate_all(env));
    is.extend([
        Instruction::Extend(Type::I64, Type::I32),
        Instruction::Const(Value::I64((func as i64) << 32)),
        Instruction::Or(Type::I64),
    ]);
    is
}

fn make_function(func: usize) -> Vec<Instruction> {
    vec![Instruction::Const(Value::I64((func as i64) << 32))]
}

fn make_record(es: Vec<Expr>) -> Vec<Instruction> {
    let mut is = Vec::new();
    is.extend(allocate_all(es));
    is.push(Instruction::Extend(Type::I64, Type::I32));
    is
}

fn call_closure() -> Vec<Instruction> {
    let mut is = Vec::new();
    is.extend([
        Instruction::TeeLocal(String::from("g64")),
        Instruction::Const(Value::I64(0x_ff_ff_ff_ff)),
        Instruction::And(Type::I64),
        Instruction::Wrap(Type::I32, Type::I64),
        Instruction::GetLocal(String::from("g64")),
        Instruction::Const(Value::I64(32)),
        Instruction::RightShift(Type::I64),
        Instruction::Wrap(Type::I32, Type::I64),
        Instruction::CallIndirect(String::from("all")),
    ]);
    is
}

fn duplicate(t: Type) -> Vec<Instruction> {
    let reg = format!("g{}", t.bits());
    vec![
        Instruction::TeeLocal(reg.clone()),
        Instruction::GetLocal(reg),
    ]
}
