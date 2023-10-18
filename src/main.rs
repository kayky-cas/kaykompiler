use std::{io::stdin, process::exit, rc::Rc, time::Instant};

use serde::Deserialize;

macro_rules! binary_op {
    ($term:ident, $lhs:ident $op:tt $rhs:ident) => {
        match ($lhs, $rhs) {
            (Ok(Val::Bool(l)), Ok(Val::Bool(r))) => Ok(Val::Bool(l $op r)),
            (Err(e), _) | (_, Err(e)) => Err(e),
            _ => Err(CompilerError::new(
                "Tipo invalido.".into()
            )),
        }
    }
}

macro_rules! binary_op_int {
    ($term:ident, $lhs:ident $op:tt $rhs:ident) => {
        match ($lhs, $rhs) {
            (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Bool(l $op r)),
            (Err(e), _) | (_, Err(e)) => Err(e),
            _ => Err(CompilerError::new(
                "Tipo invalido.".into()
            )),
        }
    }
}

#[derive(Debug, Deserialize)]
struct Program {
    expression: Term,
}

#[derive(Debug, Deserialize)]
struct CompilerError {
    message: String,
}

impl CompilerError {
    fn new(message: String) -> Self {
        Self { message }
    }
}

#[derive(Debug, Deserialize, Clone)]
struct Str {
    value: String,
}

#[derive(Debug, Deserialize, Clone)]
struct Int {
    value: i32,
}

#[derive(Debug, Deserialize, Clone)]
struct Bool {
    value: bool,
}

#[derive(Debug, Deserialize, Clone)]
struct Print {
    value: Box<Term>,
}

#[derive(Debug, Deserialize, Clone)]
struct Binary {
    lhs: Box<Term>,
    op: BinaryOp,
    rhs: Box<Term>,
}

#[derive(Debug, Deserialize, Clone)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
}

#[derive(Debug, Deserialize, Clone)]
struct Parameter {
    text: String,
}

#[derive(Debug, Deserialize, Clone)]
struct Let {
    name: Parameter,
    value: Box<Term>,
    next: Box<Term>,
}

#[derive(Debug, Deserialize, Clone)]
struct Function {
    parameters: Vec<Parameter>,
    value: Box<Term>,
}

#[derive(Debug, Deserialize, Clone)]
struct If {
    condition: Box<Term>,
    then: Box<Term>,
    otherwise: Box<Term>,
}

#[derive(Debug, Deserialize, Clone)]
struct Var {
    text: String,
}

#[derive(Debug, Deserialize, Clone)]
struct Call {
    callee: Box<Term>,
    arguments: Vec<Term>,
}

#[derive(Debug, Deserialize, Clone)]
#[serde(tag = "kind")]
enum Term {
    Int(Int),
    Str(Str),
    Bool(Bool),
    Print(Print),
    Binary(Binary),
    Let(Let),
    Function(Function),
    If(If),
    Var(Var),
    Call(Call),
}

#[derive(Debug, Clone)]
struct Func {
    parameters: Vec<Parameter>,
    value: Rc<Term>,
}

#[derive(Clone, Debug)]
enum Val {
    Function(Func),
    Bool(bool),
    Str(String),
    Int(i32),
    Void,
}

struct Runtime {
    env: Vec<(String, Val)>,
}

impl Runtime {
    fn new() -> Self {
        Self { env: vec![] }
    }

    fn run(&mut self, program: Program) -> Result<Val, CompilerError> {
        self.evaluate(&program.expression)
    }

    fn evaluate_print(&mut self, term: &Print) -> Result<Val, CompilerError> {
        let val = self.evaluate(&term.value);

        match val {
            Ok(Val::Str(val)) => println!("{val}"),
            Ok(Val::Int(val)) => println!("{val}"),
            Err(err) => return Err(err),
            _ => return Err(CompilerError::new("Tipo inválido para print".into())),
        };

        Ok(Val::Void)
    }

    fn evaluate_binary(&mut self, term: &Binary) -> Result<Val, CompilerError> {
        let lhs = self.evaluate(&term.lhs);
        let rhs = self.evaluate(&term.rhs);

        match term.op {
            BinaryOp::Add => match (lhs, rhs) {
                (Ok(Val::Str(l)), Ok(Val::Str(r))) => Ok(Val::Str(l + &r)),
                (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Int(l + r)),
                (Ok(Val::Str(l)), Ok(Val::Int(r))) => Ok(Val::Str(l + &r.to_string())),
                (Ok(Val::Int(l)), Ok(Val::Str(r))) => Ok(Val::Str(l.to_string() + &r)),
                (Err(e), _) | (_, Err(e)) => Err(e),
                _ => Err(CompilerError::new("Operador inválido para soma.".into())),
            },
            BinaryOp::Sub => match (lhs, rhs) {
                (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Int(l - r)),
                (Err(e), _) | (_, Err(e)) => Err(e),
                _ => Err(CompilerError::new(
                    "Operador inválido para subtração.".into(),
                )),
            },
            BinaryOp::Mul => match (lhs, rhs) {
                (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Int(l * r)),
                (Err(e), _) | (_, Err(e)) => Err(e),
                _ => Err(CompilerError::new(
                    "Operador inválido para subtração.".into(),
                )),
            },
            BinaryOp::Rem => match (lhs, rhs) {
                (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Int(l % r)),
                (Err(e), _) | (_, Err(e)) => Err(e),
                _ => Err(CompilerError::new("Operador inválido para resto.".into())),
            },
            BinaryOp::Div => match (lhs, rhs) {
                (Ok(Val::Int(l)), Ok(Val::Int(r))) => {
                    if r == 0 {
                        Err(CompilerError::new("Divisão por zero é inválida.".into()))
                    } else {
                        Ok(Val::Int(l / r))
                    }
                }
                (Err(e), _) | (_, Err(e)) => Err(e),
                _ => Err(CompilerError::new("Operador inválido para divisão.".into())),
            },

            BinaryOp::Eq => match (lhs, rhs) {
                (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Bool(l == r)),
                (Ok(Val::Str(l)), Ok(Val::Str(r))) => Ok(Val::Bool(l == r)),
                (Ok(Val::Bool(l)), Ok(Val::Bool(r))) => Ok(Val::Bool(l == r)),
                (Err(e), _) | (_, Err(e)) => Err(e),
                _ => Err(CompilerError::new("Igualdade inválida.".into())),
            },
            BinaryOp::Neq => match (lhs, rhs) {
                (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Bool(l != r)),
                (Ok(Val::Str(l)), Ok(Val::Str(r))) => Ok(Val::Bool(l != r)),
                (Ok(Val::Bool(l)), Ok(Val::Bool(r))) => Ok(Val::Bool(l != r)),
                (Err(e), _) | (_, Err(e)) => Err(e),
                _ => Err(CompilerError::new("Não igualdade inválida.".into())),
            },
            BinaryOp::Lt => binary_op_int!(term, lhs < rhs),
            BinaryOp::Gt => binary_op_int!(term, lhs > rhs),
            BinaryOp::Lte => binary_op_int!(term, lhs <= rhs),
            BinaryOp::Gte => binary_op_int!(term, lhs >= rhs),
            BinaryOp::And => binary_op!(term, lhs && rhs),
            BinaryOp::Or => binary_op!(term, lhs || rhs),
        }
    }

    fn evaluate_let(&mut self, term: &Let) -> Result<Val, CompilerError> {
        let value = self.evaluate(&term.value)?;
        self.env.push((term.name.text.clone(), value));
        self.evaluate(&term.next)
    }

    fn evaluate_function(&mut self, term: &Function) -> Result<Val, CompilerError> {
        Ok(Val::Function(Func {
            parameters: term.parameters.clone(),
            value: Rc::new(*term.value.clone()),
        }))
    }

    fn evaluate_if(&mut self, term: &If) -> Result<Val, CompilerError> {
        let condition = self.evaluate(&term.condition)?;

        match condition {
            Val::Bool(true) => self.evaluate(&term.then),
            Val::Bool(false) => self.evaluate(&term.otherwise),
            _ => Err(CompilerError::new("Condição inválida para if.".into())),
        }
    }

    fn evaluate_var(&mut self, term: &Var) -> Result<Val, CompilerError> {
        for (name, value) in self.env.iter().rev() {
            if name == &term.text {
                return Ok(value.clone());
            }
        }

        Err(CompilerError::new("Variável não encontrada.".into()))
    }

    fn evaluate_call(&mut self, term: &Call) -> Result<Val, CompilerError> {
        if let Val::Function(func) = self.evaluate(&term.callee)? {
            if term.arguments.len() != func.parameters.len() {
                return Err(CompilerError::new("Número de argumentos inválido.".into()));
            }

            let original_stack_size = self.env.len();

            for (parameter, argument) in func.parameters.iter().zip(term.arguments.iter()) {
                let argument = self.evaluate(argument)?;
                self.env.push((parameter.text.clone(), argument));
            }

            let result = self.evaluate(&func.value);
            self.env.truncate(original_stack_size);

            result
        } else {
            Err(CompilerError::new("Chamada inválida.".into()))
        }
    }

    fn evaluate(&mut self, term: &Term) -> Result<Val, CompilerError> {
        match term {
            Term::Int(term) => Ok(Val::Int(term.value)),
            Term::Str(term) => Ok(Val::Str(term.value.clone())),
            Term::Bool(term) => Ok(Val::Bool(term.value)),
            Term::Print(term) => self.evaluate_print(term),
            Term::Binary(term) => self.evaluate_binary(term),
            Term::Let(term) => self.evaluate_let(term),
            Term::Function(term) => self.evaluate_function(term),
            Term::If(term) => self.evaluate_if(term),
            Term::Var(term) => self.evaluate_var(term),
            Term::Call(term) => self.evaluate_call(term),
        }
    }
}

fn main() {
    let program: Program = serde_json::from_reader(stdin().lock()).unwrap();

    let mut runtime = Runtime::new();

    let instant = Instant::now();

    if let Err(e) = runtime.run(program) {
        eprintln!("ERROR: {}", e.message);
        exit(1);
    }

    println!("Elapsed: {:?}", instant.elapsed());
}
