use std::collections::VecDeque;

pub use derive_new::new;
use indexmap::IndexMap;

use crate::NF;

#[derive(PartialEq, Eq, Hash, Debug, Clone, new)]
pub enum Alphabet {
    Init,         // ●
    Bound(usize), // Bound variables
    Free(String), // Free variables
}

#[derive(PartialEq, Eq, Debug, Clone, new)]
pub struct PMove {
    pub alphabet: Alphabet,
    pub pointer: Option<usize>,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, new)]
pub struct OMove {
    pub alphabet: Alphabet,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Strategy {
    pub tree: IndexMap<OMove, (PMove, Strategy)>,
}

fn compile_var_to_move(name: &String, context: &[Vec<String>]) -> PMove {
    if context.is_empty() {
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
            let mut rest = compile_nf_to_strategy(arg, &context);
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
            let mut t = compile_nf_to_strategy(nf, &VecDeque::new());
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
}
