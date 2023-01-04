use crate::NF;
use anyhow::{Context, Result};
use std::collections::HashMap;
use typed_arena::Arena;

pub struct Entry<'arena> {
    binds: HashMap<String, &'arena NF>,
    env: &'arena Env<'arena>,
}
pub enum Env<'arena> {
    Cons {
        head: &'arena Entry<'arena>,
        tail: &'arena Env<'arena>,
    },
    Nil,
}

impl<'arena> Env<'arena> {
    fn find_map<T, F>(&self, f: F) -> Option<T>
    where
        F: Fn(&'arena Entry<'arena>) -> Option<T>,
    {
        match self {
            Self::Cons { head, tail } => match f(head) {
                None => tail.find_map(f),
                Some(x) => Some(x),
            },
            Self::Nil => None,
        }
    }
}

pub fn kam_iter<'arena>(
    arena: &'arena Arena<Entry<'arena>>,
    head: String,
    args: &'arena [&'arena NF],
    env: &'arena Env<'arena>,
) -> Result<(String, &'arena Vec<NF>, Env)> {
    let (&new_term, &new_env) = env
        .find_map(|Entry { binds, env }| binds.get(&head).map(|nf| (nf, env)))
        .context("Cannot find bound variables")?;

    let new_env = Env::Cons {
        head: arena.alloc(Entry {
            binds: new_term
                .names
                .iter()
                .zip(args.iter())
                .map(|(name, &arg)| (name.clone(), arg))
                .collect::<HashMap<_, _>>(),
            env,
        }),
        tail: new_env,
    };

    Ok((new_term.head.clone(), &new_term.args, new_env))
}

#[cfg(test)]
mod test {
    use crate::mocks::nf::{nf1, nf2};

    use super::*;

    #[test]
    fn exec_one_step() {
        let arena = Arena::new();

        let mut binds = HashMap::new();
        let t2 = nf2();
        binds.insert("u".to_owned(), &t2);

        let env = Env::Cons {
            head: arena.alloc(Entry {
                binds,
                env: &Env::Nil,
            }),
            tail: &Env::Nil,
        };

        let t1 = nf1();
        let args = &vec![&t1];
        let (head, term, new_env) = kam_iter(&arena, "u".to_owned(), args, &env).unwrap();
        assert_eq!("r", head);
        assert_eq!(
            &vec![NF {
                names: vec![],
                head: "r".to_owned(),
                args: vec![NF {
                    names: vec![],
                    head: "z".to_owned(),
                    args: vec![]
                }]
            }],
            term
        );

        let args = &term.iter().map(|x| x).collect();
        let (head, term, _new_env) = kam_iter(&arena, head, args, &new_env).unwrap();
        assert_eq!("u", head);
        assert_eq!(
            &vec![NF {
                names: vec!["y".to_owned()],
                head: "x".to_owned(),
                args: vec![]
            }],
            term
        );
    }
}
