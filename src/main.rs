use std::{io::stdin, process::exit};

use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Program {
    // name: String,
    expression: Term,
    // location: Location,
}

#[derive(Debug, Deserialize)]
struct Location {
    start: usize,
    end: usize,
    filename: String,
}

struct KaykompilerError {
    message: String,
    location: Location,
}

impl KaykompilerError {
    fn new(message: String, location: Location) -> Self {
        Self { message, location }
    }
}

#[derive(Debug, Deserialize)]
struct Str {
    value: String,
    // location: Location,
}

#[derive(Debug, Deserialize)]
struct Int {
    value: i32,
    // location: Location,
}

#[derive(Debug, Deserialize)]
struct Bool {
    value: bool,
}

#[derive(Debug, Deserialize)]
struct Print {
    value: Box<Term>,
    location: Location,
}

#[derive(Debug, Deserialize)]
struct Binary {
    lhs: Box<Term>,
    op: BinaryOp,
    rhs: Box<Term>,
    location: Location,
}

#[derive(Debug, Deserialize)]
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

#[derive(Debug, Deserialize)]
#[serde(tag = "kind")]
enum Term {
    Int(Int),
    Str(Str),
    Bool(Bool),
    Print(Print),
    Binary(Binary),
}

#[derive(Debug, Deserialize)]
enum Val {
    Bool(bool),
    Str(String),
    Int(i32),
    Void,
}

fn run(term: Term) -> Result<Val, KaykompilerError> {
    match term {
        Term::Int(term) => Ok(Val::Int(term.value)),
        Term::Str(term) => Ok(Val::Str(term.value)),
        Term::Bool(term) => Ok(Val::Bool(term.value)),
        Term::Print(term) => {
            let val = run(*term.value);

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
                let lhs = run(*term.lhs);
                let rhs = run(*term.rhs);

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
                let lhs = run(*term.lhs);
                let rhs = run(*term.rhs);

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
                let lhs = run(*term.lhs);
                let rhs = run(*term.rhs);

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
                let lhs = run(*term.lhs);
                let rhs = run(*term.rhs);

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
                let lhs = run(*term.lhs);
                let rhs = run(*term.rhs);

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
                let lhs = run(*term.lhs);
                let rhs = run(*term.rhs);

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
                let lhs = run(*term.lhs);
                let rhs = run(*term.rhs);

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
            BinaryOp::Lt => todo!(),
            BinaryOp::Gt => todo!(),
            BinaryOp::Lte => todo!(),
            BinaryOp::Gte => todo!(),
            BinaryOp::And => todo!(),
            BinaryOp::Or => todo!(),
        },
    }
}

fn main() {
    let program: Program = serde_json::from_reader(stdin().lock()).unwrap();

    match run(program.expression) {
        Err(e) => {
            eprintln!(
                "ERROR: {} {}:{}:{}",
                e.message, e.location.filename, e.location.start, e.location.end
            );
            exit(1);
        }
        _ => {}
    }
}
