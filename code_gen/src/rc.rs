use crate::lift::*;
use crate::variable::*;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Instruction {
    UpdateReferenceCount(Variable, i8),
    UpdateCaptureReferenceCounts(Variable, usize, i8),
    Binding(Binding),
}

pub fn reference_count_program(program: Program<Binding>) -> Program<Instruction> {
    let main = reference_count_block(program.main);
    let definitions = program
        .definitions
        .into_iter()
        .map(|(function, definition)| (function, reference_count_definition(definition)))
        .collect();
    Program { definitions, main }
}

fn reference_count_definition(
    definition: FunctionDefinition<Binding>,
) -> FunctionDefinition<Instruction> {
    let body = reference_count_block(definition.body);
    let arguments = definition.arguments;
    FunctionDefinition { arguments, body }
}

fn reference_count_block(block: Block<Binding>) -> Block<Instruction> {
    let mut to_free = Vec::new();
    let mut instructions = Vec::new();

    for binding in block.instructions {
        let (before, after, finally) = reference_count_binding(binding.clone());
        instructions.extend(before);
        instructions.push(Instruction::Binding(binding));
        instructions.extend(after);
        to_free.extend(finally);
    }

    instructions.push(Instruction::UpdateReferenceCount(block.result.clone(), 1));

    for variable in to_free {
        instructions.push(Instruction::UpdateReferenceCount(variable, -1));
    }

    Block {
        instructions,
        result: block.result,
    }
}

/// the first set of variables should be incremented right before the binding,
/// the second set of variables should be incremented right after the binding,
/// the third set of variables should be decremented at the end of the scope
fn reference_count_binding(
    binding: Binding,
) -> (Vec<Instruction>, Vec<Instruction>, Vec<Variable>) {
    todo!()
}
/*
    match binding.value {
        Expr::CallBuiltin(_, fields) | Expr::Tuple(_, fields) => (fields, vec![binding.variable]),
        Expr::Literal(_) => (Vec::new(), Vec::new()),
        Expr::TupleIndex(_, _) => (vec![binding.variable.clone()], vec![binding.variable]),
        Expr::Variable(_) => (vec![binding.variable.clone()], vec![binding.variable]),
        Expr::Closure(_, captures) => (captures, vec![binding.variable]),
        Expr::CallClosure(_, arguments) => (arguments, vec![binding.variable]),

        Expr::Switch(_, _) => todo!(),
        Expr::SpecializeClosure(_, _) => todo!(),
    }
}
*/
