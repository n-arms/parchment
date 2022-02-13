use im::HashMap;
use std::fmt;

#[derive(Clone, Debug)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Clone, Debug)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Value {
    fn load_const(&self) -> String {
        let (t, v) = match self {
            Value::I32(i) => ("i32", i.to_string()),
            Value::I64(i) => ("i64", i.to_string()),
            Value::F32(i) => ("f32", i.to_string()),
            Value::F64(i) => ("f64", i.to_string()),
        };
        format!("{}.const {}", t, v)
    }
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Add(Type),
    Mul(Type),
    GetLocal(String),
    SetLocal(String),
    CallIndirect(String),
    Const(Value),
}

#[derive(Clone, Debug)]
pub struct FunctionDef {
    /// the list of arguments and their types
    args: Vec<(String, Type)>,
    /// the list of local variables and their types
    locals: Vec<(String, Type)>,
    /// the return type
    return_type: Type,
    /// the instructions
    body: Vec<Instruction>,
    /// the function name
    name: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Wasm {
    /// the list of function definitions
    defs: Vec<FunctionDef>,
    /// functions to be exported
    exports: Vec<String>,
    /// the user defined type aliases (to be used with call_indirect)
    types: HashMap<String, String>,
    /// the list of tables. Each table has a length and a contained type
    tables: Vec<(usize, String)>,
    /// the list of global variables, their types and their values
    globals: Vec<(String, Type, Value)>,
    /// the list of linear memory blocks. Each block has a length and a name
    memory: Vec<(usize, String)>,
    /// the offset of elements in the function table
    elem_offset: usize,
    /// the elements in the function table
    elems: Vec<String>,
}

impl Wasm {
    pub fn format(&self, w: &mut WATFormatter) {
        w.line("(module");
        w.inc();
        for (len, contains) in &self.tables {
            w.line(format!("(table {} {})", len, contains));
        }
        w.line(format!(
            "(elem (i32.const {}){})",
            self.elem_offset,
            self.elems.iter().fold(String::new(), |mut acc, x| {
                acc.push(' ');
                acc.push_str(&x.to_string());
                acc
            })
        ));
        for elem in &self.elems {}
        for (name, signature) in &self.types {
            w.line(format!("(type ${} {})", name, signature));
        }
        for (name, signature, value) in &self.globals {
            w.line(format!(
                "(global ${} (mut {}) ({}))",
                name,
                signature,
                value.load_const()
            ));
        }
        for (size, name) in &self.memory {
            w.line(format!("(memory ${} {})", size, name));
        }
        for def in &self.defs {
            def.format(w);
        }
        for export in &self.exports {
            w.line(format!("(export \"{}\" (func ${}))", export, export));
        }
        w.dec();
        w.line(")");
    }
}

impl FunctionDef {
    pub fn format(&self, w: &mut WATFormatter) {
        w.line(format!(
            "(func{} {}(result {}) {}",
            self.name
                .as_ref()
                .map(|n| String::from(" $") + n)
                .unwrap_or_default(),
            self.args
                .iter()
                .map(|(name, t)| format!("(param ${} {})", name, t))
                .fold(String::new(), |mut acc, x| {
                    acc.push_str(&x);
                    acc.push(' ');
                    acc
                }),
            self.return_type,
            self.locals
                .iter()
                .map(|(name, t)| format!("(local ${} {})", name, t))
                .fold(String::new(), |mut acc, x| {
                    acc.push_str(&x);
                    acc.push(' ');
                    acc
                })
        ));
        w.inc();
        for instr in &self.body {
            instr.format(w);
        }
        w.dec();
        w.line(")");
    }
}

impl Instruction {
    pub fn format(&self, w: &mut WATFormatter) {
        match self {
            Instruction::Add(t) => w.line(format!("{}.add", t)),
            Instruction::Mul(t) => w.line(format!("{}.mul", t)),
            Instruction::GetLocal(l) => w.line(format!("local.get ${}", l)),
            Instruction::SetLocal(l) => w.line(format!("local.set ${}", l)),
            Instruction::CallIndirect(t) => w.line(format!("call_indirect (type ${})", t)),
            Instruction::Const(r) => w.line(r.load_const()),
        }
    }
}

impl fmt::Display for WATFormatter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (indent, line) in &self.lines {
            for _ in 0..*indent {
                write!(f, "\t")?;
            }
            write!(f, "{}\n", line)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Default)]
pub struct WATFormatter {
    lines: Vec<(usize, String)>,
    indent: usize,
}

impl WATFormatter {
    pub fn line(&mut self, text: impl AsRef<str>) {
        self.lines.push((self.indent, text.as_ref().to_string()))
    }
    pub fn lines(&mut self, text: Vec<impl AsRef<str>>) {
        for line in text {
            self.line(line)
        }
    }
    pub fn inc(&mut self) {
        self.indent += 1;
    }
    pub fn dec(&mut self) {
        self.indent -= 1;
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::I32 => "i32",
                Type::I64 => "i64",
                Type::F32 => "f32",
                Type::F64 => "f64",
            }
        )
    }
}
