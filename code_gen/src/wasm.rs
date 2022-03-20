use im::HashMap;
use std::fmt;

#[derive(Copy, Clone, Debug)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
}

impl Type {
    pub fn bits(&self) -> usize {
        match self {
            Self::I32 | Self::F32 => 32,
            Self::I64 | Self::F64 => 64,
        }
    }
}

#[derive(Copy, Clone, Debug)]
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

/// a web assembly instruction
#[derive(Clone, Debug)]
pub enum Instruction {
    /// pop two values of type t off the stack, add them, push the result onto the stack
    Add(Type),
    /// pop two values of type t off the stack, subtract them, push the result onto the stack
    Sub(Type),
    /// pop two values of type t off the stack, multiply them, push the result onto the stack
    Mul(Type),
    /// pop two values of type t off the stack, left shift the first by the second, push the result onto the stack
    LeftShift(Type),
    /// pop two values of type t off the stack, right shift the first by the second, push the result onto the stack
    RightShift(Type),
    /// pop two values of type t off the stack, check if the first is less than the other, push the
    /// result onto the stack
    LessThan(Type),
    /// pop two values of type t off the stack, check if the first is less than or equal to the other, push the
    /// result onto the stack
    LessThanEqual(Type),
    /// pop two values of type t off the stack, check if the first is greater than the other, push the
    /// result onto the stack
    GreaterThan(Type),
    /// pop two values of type t off the stack, check if the first is greater than or equal to the other, push the
    /// result onto the stack
    GreaterThanEqual(Type),
    /// pop two values of type t off the stack, check if they are equal, push the result onto the
    /// stack
    Equal(Type),
    /// pop two values of type t off the stack, and them, push the result onto the stack
    Or(Type),
    /// pop two values of type t off the stack, or them, push the result onto the stack
    And(Type),
    /// push the value in register s onto the stack
    GetLocal(String),
    /// pop 1 value off the top of the stack and assign it to register s
    SetLocal(String),
    /// equivilant to [SetLocal(s), GetLocal(s)]
    TeeLocal(String),
    /// push the value in global s onto the stack
    GetGlobal(String),
    /// pop 1 value off the top of the stack and assign it to global s
    SetGlobal(String),
    /// pop an address: i32 off the stack, look it up in linear memory and push the value of type t onto the stack
    Load(Type),
    /// pop an address: i32 and a value: t off the stack and assign the value to the address
    Store(Type),
    /// pop a value of type t2 off the stack, reinterpret it as t1 and push it back onto the stack.
    /// This operation does not change the bits of the value, it just changes the type
    Reinterpret(Type, Type),
    /// pop a value of type t2 off the stack, shrink it to the appropriate size and push a value of
    /// type t1 onto the stack. This operation converts large values into small values.
    Wrap(Type, Type),
    /// pop a value of type t2 off the stack, grow it to the appropriate size and push a value of
    /// type t1 onto the stack. This operation converts small values into large values.
    Extend(Type, Type),
    /// pop a function pointer of type s off the top of the stack, then call it
    CallIndirect(String),
    /// load a constant value onto the stack
    Const(Value),
    /// pop a value off the stack, if it is true evaluate the first expression, if it is false
    /// evaluate the second. Both expressions should produce expressions of type t
    If(Vec<Instruction>, Vec<Instruction>, Type),
    Drop,
    Unreachable,
}

#[derive(Clone, Debug)]
pub struct FunctionDef {
    /// the list of arguments and their types
    pub args: Vec<(String, Type)>,
    /// the list of local variables and their types
    pub locals: Vec<(String, Type)>,
    /// the return type
    pub return_type: Type,
    /// the instructions
    pub body: Vec<Instruction>,
    /// the function name
    pub name: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Wasm {
    /// the list of function definitions
    pub defs: Vec<FunctionDef>,
    /// functions to be exported
    pub exports: Vec<String>,
    /// the user defined type aliases (to be used with call_indirect)
    pub types: HashMap<String, String>,
    /// the list of tables. Each table has a length and a contained type
    pub tables: Vec<(usize, String)>,
    /// the list of global variables, their types and their values
    pub globals: Vec<(String, Type, Value)>,
    /// the list of linear memory blocks. Each block has a length and a name
    pub memory: Vec<(usize, String)>,
    /// the offset of elements in the function table
    pub elem_offset: usize,
    /// the elements in the function table
    pub elems: Vec<String>,
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
            w.line(format!("(memory ${} {})", name, size));
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
            Instruction::Const(r) => w.line(r.load_const()),
            Instruction::Add(t) => w.line(format!("{}.add", t)),
            Instruction::Sub(t) => w.line(format!("{}.sub", t)),
            Instruction::Mul(t) => w.line(format!("{}.mul", t)),
            Instruction::LeftShift(t) => w.line(format!("{}.shl", t)),
            Instruction::RightShift(t) => w.line(format!("{}.shr_u", t)),
            Instruction::Or(t) => w.line(format!("{}.or", t)),
            Instruction::And(t) => w.line(format!("{}.and", t)),
            Instruction::GetLocal(l) => w.line(format!("local.get ${}", l)),
            Instruction::SetLocal(l) => w.line(format!("local.set ${}", l)),
            Instruction::TeeLocal(l) => w.line(format!("local.tee ${}", l)),
            Instruction::GetGlobal(g) => w.line(format!("global.get ${}", g)),
            Instruction::SetGlobal(g) => w.line(format!("global.set ${}", g)),
            Instruction::Load(t) => w.line(format!("{}.load", t)),
            Instruction::Store(t) => w.line(format!("{}.store", t)),
            Instruction::Reinterpret(new, old) => w.line(format!("{}.reinterpret_{}", new, old)),
            Instruction::Wrap(new, old) => w.line(format!("{}.wrap_{}", new, old)),
            Instruction::Extend(new, old) => w.line(format!("{}.extend_{}_u", new, old)),
            Instruction::CallIndirect(t) => w.line(format!("call_indirect (type ${})", t)),
            Instruction::If(i1, i2, res) => {
                w.line(format!("(if (result {})", res));
                w.inc();
                w.line("(then");
                w.inc();
                for i in i1 {
                    i.format(w);
                }
                w.dec();
                w.line(")");
                w.line("(else");
                w.inc();
                for i in i2 {
                    i.format(w);
                }
                w.dec();
                w.line(")");
                w.dec();
                w.line(")");
            }
            Instruction::LessThan(t) => w.line(format!("{}.lt", t)),
            Instruction::LessThanEqual(t) => w.line(format!("{}.le", t)),
            Instruction::GreaterThan(t) => w.line(format!("{}.gt", t)),
            Instruction::GreaterThanEqual(t) => w.line(format!("{}.ge", t)),
            Instruction::Equal(t) => w.line(format!("{}.eq", t)),
            Instruction::Drop => w.line(String::from("drop")),
            Instruction::Unreachable => w.line(String::from("unreachable")),
        }
    }
}

impl fmt::Display for WATFormatter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (indent, line) in &self.lines {
            for _ in 0..*indent {
                write!(f, "\t")?;
            }
            writeln!(f, "{}", line)?;
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
        self.lines.push((self.indent, text.as_ref().to_owned()));
    }
    pub fn lines(&mut self, text: Vec<impl AsRef<str>>) {
        for line in text {
            self.line(line);
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
