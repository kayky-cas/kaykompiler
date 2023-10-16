use std::{io::stdin, time::Instant};

use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Program {
    // name: String,
    expression: Term,
    // location: Location,
}

#[derive(Debug, Deserialize, Clone)]
struct Location {
    start: usize,
    end: usize,
    filename: String,
}

#[derive(Debug, Deserialize)]
struct KaykompilerError {
    message: String,
    location: Location,
}

impl KaykompilerError {
    fn new(message: String, location: Location) -> Self {
        Self { message, location }
    }
}

#[derive(Debug, Deserialize, Clone)]
struct Str {
    value: String,
    // location: Location,
}

#[derive(Debug, Deserialize, Clone)]
struct Int {
    value: i32,
    // location: Location,
}

#[derive(Debug, Deserialize, Clone)]
struct Bool {
    value: bool,
}

#[derive(Debug, Deserialize, Clone)]
struct Print {
    value: Box<Term>,
    location: Location,
}

#[derive(Debug, Deserialize, Clone)]
struct Binary {
    lhs: Box<Term>,
    op: BinaryOp,
    rhs: Box<Term>,
    location: Location,
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
    location: Location,
}

#[derive(Debug, Deserialize, Clone)]
struct Var {
    text: String,
    location: Location,
}

#[derive(Debug, Deserialize, Clone)]
struct Call {
    callee: Box<Term>,
    arguments: Vec<Term>,
    location: Location,
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
    value: Box<Term>,
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

    fn run(&mut self, program: Program) -> Result<Val, KaykompilerError> {
        self.evaluate(program.expression)
    }

    fn evaluate(&mut self, term: Term) -> Result<Val, KaykompilerError> {
        match term {
            Term::Int(term) => Ok(Val::Int(term.value)),
            Term::Str(term) => Ok(Val::Str(term.value)),
            Term::Bool(term) => Ok(Val::Bool(term.value)),
            Term::Print(term) => {
                let val = self.evaluate(*term.value);

                match val {
                    Ok(Val::Str(val)) => println!("{val}"),
                    Ok(Val::Int(val)) => println!("{val}"),
                    Err(err) => return Err(err),
                    _ => {
                        return Err(KaykompilerError::new(
                            "Tipo inválido para print".into(),
                            term.location,
                        ))
                    }
                };

                Ok(Val::Void)
            }
            Term::Binary(term) => match term.op {
                BinaryOp::Add => {
                    let lhs = self.evaluate(*term.lhs);
                    let rhs = self.evaluate(*term.rhs);

                    match (lhs, rhs) {
                        (Ok(Val::Str(l)), Ok(Val::Str(r))) => Ok(Val::Str(l + &r)),
                        (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Int(l + r)),
                        (Ok(Val::Str(l)), Ok(Val::Int(r))) => Ok(Val::Str(l + &r.to_string())),
                        (Ok(Val::Int(l)), Ok(Val::Str(r))) => Ok(Val::Str(l.to_string() + &r)),
                        (Err(e), _) | (_, Err(e)) => Err(e),
                        _ => Err(KaykompilerError::new(
                            "Operador inválido para soma.".into(),
                            term.location,
                        )),
                    }
                }
                BinaryOp::Sub => {
                    let lhs = self.evaluate(*term.lhs);
                    let rhs = self.evaluate(*term.rhs);

                    match (lhs, rhs) {
                        (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Int(l - r)),
                        (Err(e), _) | (_, Err(e)) => Err(e),
                        _ => Err(KaykompilerError::new(
                            "Operador inválido para subtração.".into(),
                            term.location,
                        )),
                    }
                }
                BinaryOp::Mul => {
                    let lhs = self.evaluate(*term.lhs);
                    let rhs = self.evaluate(*term.rhs);

                    match (lhs, rhs) {
                        (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Int(l * r)),
                        (Err(e), _) | (_, Err(e)) => Err(e),
                        _ => Err(KaykompilerError::new(
                            "Operador inválido para subtração.".into(),
                            term.location,
                        )),
                    }
                }
                BinaryOp::Rem => {
                    let lhs = self.evaluate(*term.lhs);
                    let rhs = self.evaluate(*term.rhs);

                    match (lhs, rhs) {
                        (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Int(l % r)),
                        (Err(e), _) | (_, Err(e)) => Err(e),
                        _ => Err(KaykompilerError::new(
                            "Operador inválido para resto.".into(),
                            term.location,
                        )),
                    }
                }
                BinaryOp::Div => {
                    let lhs = self.evaluate(*term.lhs);
                    let rhs = self.evaluate(*term.rhs);

                    match (lhs, rhs) {
                        (Ok(Val::Int(l)), Ok(Val::Int(r))) => {
                            if r == 0 {
                                Err(KaykompilerError::new(
                                    "Divisão por zero é inválida.".into(),
                                    term.location,
                                ))
                            } else {
                                Ok(Val::Int(l / r))
                            }
                        }
                        (Err(e), _) | (_, Err(e)) => Err(e),
                        _ => Err(KaykompilerError::new(
                            "Operador inválido para divisão.".into(),
                            term.location,
                        )),
                    }
                }

                BinaryOp::Eq => {
                    let lhs = self.evaluate(*term.lhs);
                    let rhs = self.evaluate(*term.rhs);

                    match (lhs, rhs) {
                        (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Bool(l == r)),
                        (Ok(Val::Str(l)), Ok(Val::Str(r))) => Ok(Val::Bool(l == r)),
                        (Ok(Val::Bool(l)), Ok(Val::Bool(r))) => Ok(Val::Bool(l == r)),
                        (Err(e), _) | (_, Err(e)) => Err(e),
                        _ => Err(KaykompilerError::new(
                            "Igualdade inválida.".into(),
                            term.location,
                        )),
                    }
                }
                BinaryOp::Neq => {
                    let lhs = self.evaluate(*term.lhs);
                    let rhs = self.evaluate(*term.rhs);

                    match (lhs, rhs) {
                        (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Bool(l != r)),
                        (Ok(Val::Str(l)), Ok(Val::Str(r))) => Ok(Val::Bool(l != r)),
                        (Ok(Val::Bool(l)), Ok(Val::Bool(r))) => Ok(Val::Bool(l != r)),
                        (Err(e), _) | (_, Err(e)) => Err(e),
                        _ => Err(KaykompilerError::new(
                            "Não igualdade inválida.".into(),
                            term.location,
                        )),
                    }
                }
                BinaryOp::Lt => {
                    let lhs = self.evaluate(*term.lhs);
                    let rhs = self.evaluate(*term.rhs);

                    match (lhs, rhs) {
                        (Ok(Val::Int(l)), Ok(Val::Int(r))) => Ok(Val::Bool(l < r)),
                        (Err(e), _) | (_, Err(e)) => Err(e),
                        _ => Err(KaykompilerError::new(
                            "Tipo invalido.".into(),
                            term.location,
                        )),
                    }
                }
                BinaryOp::Gt => todo!(),
                BinaryOp::Lte => todo!(),
                BinaryOp::Gte => todo!(),
                BinaryOp::And => todo!(),
                BinaryOp::Or => {
                    let lhs = self.evaluate(*term.lhs);
                    let rhs = self.evaluate(*term.rhs);

                    match (lhs, rhs) {
                        (Ok(Val::Bool(l)), Ok(Val::Bool(r))) => Ok(Val::Bool(l || r)),
                        (Err(e), _) | (_, Err(e)) => Err(e),
                        _ => Err(KaykompilerError::new(
                            "Tipo invalido.".into(),
                            term.location,
                        )),
                    }
                }
            },
            Term::Let(term) => {
                let value = self.evaluate(*term.value)?;
                self.env.push((term.name.text, value));
                self.evaluate(*term.next)
            }
            Term::Function(term) => {
                let function = Func {
                    parameters: term.parameters,
                    value: term.value,
                };

                Ok(Val::Function(function))
            }
            Term::If(term) => {
                let condition = self.evaluate(*term.condition)?;

                match condition {
                    Val::Bool(true) => self.evaluate(*term.then),
                    Val::Bool(false) => self.evaluate(*term.otherwise),
                    _ => Err(KaykompilerError::new(
                        "Condição inválida para if.".into(),
                        term.location,
                    )),
                }
            }
            Term::Var(term) => {
                for (name, value) in self.env.iter().rev() {
                    if name == &term.text {
                        return Ok(value.clone());
                    }
                }

                Err(KaykompilerError::new(
                    "Variável não encontrada.".into(),
                    term.location,
                ))
            }
            Term::Call(term) => {
                let callee = self.evaluate(*term.callee)?;

                match callee {
                    Val::Function(func) => {
                        if term.arguments.len() != func.parameters.len() {
                            return Err(KaykompilerError::new(
                                "Número de argumentos inválido.".into(),
                                term.location,
                            ));
                        }

                        let original_stack_size = self.env.len();

                        for (parameter, argument) in
                            func.parameters.into_iter().zip(term.arguments.into_iter())
                        {
                            let arg = self.evaluate(argument)?;
                            self.env.push((parameter.text, arg));
                        }

                        let result = self.evaluate(*func.value);
                        self.env.truncate(original_stack_size);

                        result
                    }
                    _ => Err(KaykompilerError::new(
                        "Chamada inválida.".into(),
                        term.location,
                    )),
                }
            }
        }
    }
}

fn main() {
    let program: Program = serde_json::from_reader(stdin().lock()).unwrap();

    let mut runtime = Runtime::new();

    let instant = Instant::now();

    if let Err(e) = runtime.run(program) {
        eprintln!(
            "ERROR: {} {}:{}:{}",
            e.message, e.location.filename, e.location.start, e.location.end
        );
    }

    println!("Elapsed: {:?}", instant.elapsed());
}
