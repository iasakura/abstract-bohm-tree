use crate::abt::{Alphabet, OMove, PMove, Strategy};

use anyhow::{Context, Result};
use derive_new::new;

#[derive(Clone, Debug)]
pub struct EvenPosition(pub Vec<(OMove, PMove)>);

#[derive(Clone, Debug, new)]
pub struct OddPosition {
    pub seq: EvenPosition,
    pub cur: OMove,
}

impl EvenPosition {
    #[allow(dead_code)]
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

pub fn get_next_even<'a>(s: &'a Strategy, q: &'a EvenPosition) -> Result<&'a Strategy> {
    let mut cur_s = s;
    for (om, pm) in q.0.iter() {
        let (pm_next, s) = cur_s
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

pub fn get_next_odd<'a>(s: &'a Strategy, q: &'a OddPosition) -> Result<(&'a PMove, &'a Strategy)> {
    let strategy = get_next_even(s, &q.seq)?;
    let (next, str) = strategy
        .tree
        .get(&q.cur)
        .context(format!("tree don't have key {:?}: {:?}", &q.cur, s.tree))?;

    Ok((next, str))
}

// (1)
pub fn init() -> (OddPosition, Env) {
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
    let next = get_next_odd(s, &position)?.0.clone();
    Ok((position.push(next), env))
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
                        position,
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
    use indexmap::IndexMap;
    use insta::assert_debug_snapshot;

    use crate::{
        abt::compile,
        mocks::nf::{nf1, nf2},
        NF,
    };

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
