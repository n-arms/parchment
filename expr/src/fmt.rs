use super::expr::*;
use super::types::*;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

#[derive(Default, Debug)]
pub struct ExprFormatter {
    lines: Vec<(usize, String)>,
    indent: usize,
    current: String,
}

pub fn format(obj: &impl Pretty) -> String {
    let mut fmt = ExprFormatter::default();
    obj.format(&mut fmt);
    fmt.newline();

    return fmt.to_string();
}

impl Display for ExprFormatter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (indent, line) in &self.lines {
            write!(
                f,
                "{}",
                vec![' '; indent * 2].into_iter().collect::<String>()
            )?;
            writeln!(f, "{}", line)?;
        }
        Ok(())
    }
}

impl ExprFormatter {
    pub fn fmt_text(&mut self, text: &str) {
        self.current.push_str(text.as_ref());
    }

    pub fn fmt_line(&mut self, text: &str) {
        self.fmt_text(text);
        self.newline();
    }

    pub fn fmt_lines<'a>(&mut self, text: impl IntoIterator<Item = &'a str>) {
        for line in text {
            self.fmt_line(line);
        }
    }

    pub fn inc(&mut self, indent: usize) {
        self.indent += indent
    }

    pub fn dec(&mut self, indent: usize) {
        self.indent -= indent
    }

    pub fn newline(&mut self) {
        self.lines.push((self.indent, self.current.clone()));
        self.current = String::new();
    }
}

pub trait Pretty {
    fn format(&self, f: &mut ExprFormatter);
}

impl Pretty for &str {
    fn format(&self, f: &mut ExprFormatter) {
        f.fmt_text(self);
    }
}

pub fn pretty_expr<A>(expr: &Expr<A>, f: &mut ExprFormatter)
where
    A: Pretty + Clone + Debug + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    match expr {
        Expr::Function(pat, body, a) => {
            "fn ".format(f);
            pat.format(f);
            //a.format(f);
            " ->".format(f);
            f.newline();
            f.inc(1);
            body.format(f);
            f.newline();
            f.dec(1);
        }
        Expr::Application(left, right, a) => {
            "((".format(f);
            left.format(f);
            " ".format(f);
            right.format(f);
            ")".format(f);
            a.format(f);
            ")".format(f);
        }
        Expr::Number(n) => {
            n.to_string().as_str().format(f);
        }
        Expr::Boolean(b) => {
            b.to_string().as_str().format(f);
        }
        Expr::Operator(op, a) => {
            "(".format(f);
            let op_text = match op {
                Operator::Plus => "+",
                Operator::Minus => "-",
                Operator::Times => "*",
                Operator::And => "and",
                Operator::Or => "or",
                Operator::Not => "not",
                Operator::Equals => "==",
                Operator::LessThan => "<",
                Operator::LessThanEqual => "<=",
                Operator::GreaterThan => ">",
                Operator::GreaterThanEqual => ">=",
            };
            op_text.format(f);
            a.format(f);
            ")".format(f);
        }
        Expr::Variable(var, a) => {
            "(".format(f);
            var.as_str().format(f);
            a.format(f);
            ")".format(f);
        }
        Expr::Record(record) => {
            "{".format(f);
            f.newline();
            f.inc(1);
            for (key, val) in record {
                key.as_str().format(f);
                ": ".format(f);
                val.format(f);
                ",".format(f);
                f.newline();
            }
            f.dec(1);
            "}".format(f);
        }
        Expr::If(pred, cons, altr) => {
            "if ".format(f);
            pred.format(f);
            f.newline();
            f.inc(1);
            "then ".format(f);
            cons.format(f);
            f.newline();
            "else ".format(f);
            altr.format(f);
            f.newline();
            f.dec(1);
        }
        Expr::Match(e, es, a) => {
            "match ".format(f);
            e.format(f);
            " {".format(f);
            f.newline();
            f.inc(1);
            for (pat, res) in es {
                pat.format(f);
                " -> ".format(f);
                res.format(f);
                ",".format(f);
                f.newline();
            }
            f.dec(1);
            "}".format(f);
        }
        Expr::Block(block) => {
            "{".format(f);
            f.newline();
            f.inc(1);
            for stmt in block {
                stmt.format(f);
            }
            f.dec(1);
            "}".format(f);
        }
        Expr::Tuple(tuple) => {
            "(".format(f);
            f.newline();
            f.inc(1);
            for expr in tuple {
                expr.format(f);
                f.newline();
            }
            f.dec(1);
            ")".format(f);
        }
        Expr::Constructor(cons, a) => {
            "(".format(f);
            cons.as_str().format(f);
            a.format(f);
            ")".format(f);
        }
    }
}

impl<A> Pretty for Statement<A>
where
    A: Pretty + Clone + Debug + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    fn format(&self, f: &mut ExprFormatter) {
        match self {
            Statement::Let(pat, body, a) => {
                "let ".format(f);
                pat.format(f);
                a.format(f);
                " =".format(f);
                f.newline();
                f.inc(1);
                body.format(f);
                ";".format(f);
                f.newline();
                f.dec(1);
            }
            Statement::Raw(expr) => {
                expr.format(f);
                ";".format(f);
                f.newline();
            }
            Statement::TypeDef(tn, tvs, variants) => {
                "type ".format(f);
                tn.as_str().format(f);
                for tv in tvs {
                    " ".format(f);
                    tv.as_str().format(f);
                }
                f.newline();
                f.inc(1);
                let mut seperator = "= ";
                for Variant {
                    constructor,
                    fields,
                } in variants
                {
                    seperator.format(f);
                    seperator = "| ";
                    constructor.as_str().format(f);
                    for field in fields {
                        " ".format(f);
                        field.format(f);
                    }
                }
            }
        }
    }
}

impl<A> Pretty for Pattern<A>
where
    A: Pretty + Clone + Debug + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    fn format(&self, f: &mut ExprFormatter) {
        match self {
            Pattern::Variable(var, a) => {
                "(".format(f);
                var.as_str().format(f);
                a.format(f);
                ")".format(f);
            }
            Pattern::Record(_) => todo!(),
            Pattern::Tuple(_) => todo!(),
            Pattern::Construction(cons, pats, a) => {
                "(".format(f);
                cons.as_str().format(f);
                for pat in pats {
                    " ".format(f);
                    pat.format(f);
                }
                ")".format(f);
                a.format(f);
            }
        }
    }
}

impl<A> Pretty for Expr<A>
where
    A: Pretty + Clone + Debug + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    fn format(&self, f: &mut ExprFormatter) {
        pretty_expr(&self, f);
    }
}

impl Pretty for () {
    fn format(&self, f: &mut ExprFormatter) {}
}

impl<K> Pretty for Type<K>
where
    K: Clone + Debug + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    fn format(&self, f: &mut ExprFormatter) {
        " :: ".format(f);
        self.to_string().as_str().format(f);
    }
}
