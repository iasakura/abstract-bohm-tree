use anyhow::{Context, Result};
use indexmap::IndexMap;
use std::collections::VecDeque;

use crate::NF;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum Alphabet {
    Init,         // ●
    Bound(usize), // Bound variables
    Free(String), // Free variables
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct PMove {
    alphabet: Alphabet,
    pointer: Option<usize>,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct OMove {
    alphabet: Alphabet,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Strategy {
    tree: IndexMap<OMove, (PMove, Strategy)>,
}

#[derive(Clone, Debug)]
pub struct EvenPosition(Vec<(OMove, PMove)>);

#[derive(Clone, Debug)]
pub struct OddPosition {
    seq: EvenPosition,
    cur: OMove,
}

impl EvenPosition {
    fn push(self, o: OMove) -> OddPosition {
        OddPosition { seq: self, cur: o }
    }
}

impl OddPosition {
    fn push(self, p: PMove) -> EvenPosition {
        let mut s = self.seq;
        s.0.push((self.cur, p));
        s
    }
}

#[derive(Debug)]
pub enum Position {
    Even(EvenPosition),
    Odd(OddPosition),
}

#[derive(Clone, Debug)]
pub struct Closure {
    position: EvenPosition,
    env: Box<Env>,
}

#[derive(Clone, Debug)]
pub enum Env {
    Nil,
    Cons { head: Closure, tail: Box<Env> },
}

impl Env {
    fn get(&self, i: usize) -> Option<&Closure> {
        if i == 0 {
            match self {
                Env::Nil => None,
                Env::Cons { head, .. } => Some(head),
            }
        } else {
            match self {
                Env::Nil => None,
                Env::Cons { tail, .. } => tail.as_ref().get(i - 1),
            }
        }
    }
}

fn compile_var_to_move(name: &String, context: &[Vec<String>]) -> PMove {
    if context.len() == 0 {
        PMove {
            alphabet: Alphabet::Free(name.clone()),
            pointer: None,
        }
    } else {
        let head = &context[0];
        let idx = head.iter().position(|v| v == name);
        match idx {
            Some(idx) => PMove {
                alphabet: Alphabet::Bound(idx),
                pointer: Some(0),
            },
            None => match compile_var_to_move(name, &context[1..]) {
                PMove {
                    alphabet: Alphabet::Bound(i),
                    pointer: Some(j),
                } => PMove {
                    alphabet: Alphabet::Bound(i),
                    pointer: Some(j + 1),
                },
                PMove {
                    alphabet: Alphabet::Free(name),
                    pointer: None,
                } => PMove {
                    alphabet: Alphabet::Free(name),
                    pointer: None,
                },
                _ => panic!("something bad happens..."),
            },
        }
    }
}

pub fn compile_nf_to_strategy(nf: &NF, context: &VecDeque<Vec<String>>) -> Strategy {
    let NF { names, head, args } = nf;
    let mut context = context.clone();
    context.push_front(names.clone());
    let pm = compile_var_to_move(head, context.make_contiguous());
    let rests = args
        .iter()
        .enumerate()
        .map(|(i, arg)| {
            let mut rest = compile_nf_to_strategy(&arg, &context);
            if let Some(val) = rest.tree.remove(&OMove {
                alphabet: Alphabet::Init,
            }) {
                (
                    OMove {
                        alphabet: Alphabet::Bound(i),
                    },
                    val,
                )
            } else {
                panic!("Something bad...")
            }
        })
        .collect::<IndexMap<_, _>>();
    Strategy {
        tree: vec![(
            OMove {
                alphabet: Alphabet::Init,
            },
            (pm, Strategy { tree: rests }),
        )]
        .into_iter()
        .collect(),
    }
}

pub fn compile(nf: &NF, nfs: &IndexMap<String, NF>) -> Strategy {
    let phi = compile_nf_to_strategy(nf, &VecDeque::new());
    let mut psi = nfs
        .iter()
        .map(|(name, nf)| {
            let mut t = compile_nf_to_strategy(&nf, &VecDeque::new());
            if let Some(rest) = t.tree.remove(&OMove {
                alphabet: Alphabet::Init,
            }) {
                (
                    OMove {
                        alphabet: Alphabet::Free(name.clone()),
                    },
                    rest,
                )
            } else {
                panic!("Something bad...")
            }
        })
        .collect::<IndexMap<_, _>>();
    psi.extend(phi.tree);
    Strategy { tree: psi }
}

fn get_next_even<'a>(s: &'a Strategy, q: &'a EvenPosition) -> Result<&'a Strategy> {
    let mut cur_s = s;
    for (om, pm) in q.0.iter() {
        let (pm_next, s) = s
            .tree
            .get(om)
            .context(format!("tree don't have key {om:?}: {:?}", s.tree))?;
        if pm != pm_next {
            return Err(anyhow::anyhow!("Player move is inconsistent"));
        }
        cur_s = s;
    }
    Ok(cur_s)
}

fn get_next_odd<'a>(s: &'a Strategy, q: &'a OddPosition) -> Result<&'a PMove> {
    let strategy = get_next_even(s, &q.seq)?;
    let (next, _) = strategy
        .tree
        .get(&q.cur)
        .context(format!("tree don't have key {:?}: {:?}", &q.cur, s.tree))?;

    Ok(next)
}

// (1)
fn init() -> (OddPosition, Env) {
    (
        OddPosition {
            seq: EvenPosition(vec![]),
            cur: OMove {
                alphabet: Alphabet::Init,
            },
        },
        Env::Nil,
    )
}

// (2n') / (2n+1)'
fn step_2np(s: &Strategy, position: OddPosition, env: Env) -> Result<(EvenPosition, Env)> {
    let next = get_next_odd(s, &position)?.clone();
    Ok((position.push(next.clone()), env))
}

// (n)_b or (2n)_f
fn step_n(position: EvenPosition, env: Env) -> Result<(OddPosition, Env)> {
    // Last P-move
    let (
        _,
        PMove {
            alphabet: a,
            pointer,
        },
    ) = position.0.last().context("position is empty")?;

    match pointer {
        // The head variable is bound to i-th binder
        Some(i) => {
            // Each entry of environment holds the closures of i-th binder is bounded to.
            let Closure {
                position,
                env: new_env,
            } = env
                .get(*i)
                .context(format!("env don't have {i}-th argument: {env:?}"))?
                .clone();
            Ok((
                // a-th argument
                OddPosition {
                    seq: position.clone(),
                    cur: OMove {
                        alphabet: a.clone(),
                    },
                },
                Env::Cons {
                    // Store the closure of arguments of the head variables.
                    head: Closure {
                        position: position,
                        env: Box::new(env),
                    },
                    tail: new_env,
                },
            ))
        }
        None => Ok((
            OddPosition {
                seq: EvenPosition(vec![]),
                cur: OMove {
                    alphabet: a.clone(),
                },
            },
            Env::Cons {
                head: Closure {
                    position,
                    env: Box::new(env),
                },
                tail: Box::new(Env::Nil),
            },
        )),
    }
}

pub fn step(s: &Strategy, position: Position, env: Env) -> Result<(Position, Env)> {
    match position {
        Position::Even(even) => {
            let (pos, env) = step_n(even, env)?;
            Ok((Position::Odd(pos), env))
        }
        Position::Odd(odd) => {
            let (pos, env) = step_2np(s, odd, env)?;
            Ok((Position::Even(pos), env))
        }
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_debug_snapshot;

    use crate::mocks::nf::{nf1, nf2};

    use super::*;

    fn test1() -> (NF, IndexMap<String, NF>) {
        (
            NF {
                names: vec![],
                head: "u".to_owned(),
                args: vec![nf1()],
            },
            vec![("u".to_owned(), nf2())].into_iter().collect(),
        )
    }

    #[test]
    fn test_compile_nf_to_strategy() {
        let (t, env) = test1();
        let res = compile(&t, &env);
        assert_debug_snapshot!(res)
    }

    #[test]
    fn test_step() {
        let (t, ts) = test1();
        let s = compile(&t, &ts);
        let (pos, env) = init();
        let pos = Position::Odd(pos);

        let (pos, env) = step(&s, pos, env).unwrap();
        assert_debug_snapshot!(pos);
        assert_debug_snapshot!(env);

        let (pos, env) = step(&s, pos, env).unwrap();
        assert_debug_snapshot!(pos);
        assert_debug_snapshot!(env);

        let (pos, env) = step(&s, pos, env).unwrap();
        assert_debug_snapshot!(pos);
        assert_debug_snapshot!(env);

        let (pos, env) = step(&s, pos, env).unwrap();
        assert_debug_snapshot!(pos);
        assert_debug_snapshot!(env);

        let (pos, env) = step(&s, pos, env).unwrap();
        assert_debug_snapshot!(pos);
        assert_debug_snapshot!(env);

        let (pos, env) = step(&s, pos, env).unwrap();
        assert_debug_snapshot!(pos);
        assert_debug_snapshot!(env);

        let (pos, env) = step(&s, pos, env).unwrap();
        assert_debug_snapshot!(pos);
        assert_debug_snapshot!(env);

        let (pos, env) = step(&s, pos, env).unwrap();
        assert_debug_snapshot!(pos);
        assert_debug_snapshot!(env);

        let (pos, env) = step(&s, pos, env).unwrap();
        assert_debug_snapshot!(pos);
        assert_debug_snapshot!(env);
    }
}
