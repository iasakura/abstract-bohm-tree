use crate::{
    abt::{Alphabet, PMove, Strategy},
    sam::{get_next_odd, EvenPosition, OddPosition, Position},
};
use anyhow::{Context, Result};
use derive_new::new;
use indexmap::IndexMap;
use log::debug;

#[derive(Clone, Debug, Hash, PartialEq, Eq, new)]
pub struct OMove {
    alphabet: Alphabet,
    step: usize,
}

#[derive(PartialEq, Eq, Debug, Default, new)]
pub struct GAMTree {
    pub tree: IndexMap<OMove, Option<(PMove, GAMTree)>>,
}

impl GAMTree {
    fn get(&self, n: usize) -> Option<DynPosition> {
        self.tree.iter().find_map(|(k, v)| {
            if k.step == n {
                Some(DynPosition::new(
                    EvenDynPosition::new(vec![]),
                    Some(k.clone()),
                ))
            } else if k.step == n - 1 {
                v.as_ref().map(|(pm, _)| {
                    DynPosition::new(EvenDynPosition::new(vec![(k.clone(), pm.clone())]), None)
                })
            } else {
                match v {
                    None => None,
                    Some((pm, tree)) => match tree.get(n) {
                        None => None,
                        Some(pos) => {
                            let mut seq = vec![(k.clone(), pm.clone())];
                            seq.extend(pos.seq.0); // inefficient...
                            Some(DynPosition::new(EvenDynPosition(seq), pos.cur))
                        }
                    },
                }
            }
        })
    }

    fn add(&mut self, pos: DynPosition) {
        let mut tree = &mut self.tree;
        for (om, pm) in pos.seq.0 {
            let entry = tree.entry(om.clone());
            match entry {
                indexmap::map::Entry::Occupied(mut entry) => match entry.get_mut() {
                    None => {
                        let t = IndexMap::new();
                        entry.insert(Some((pm, GAMTree::new(t))));
                    }
                    Some((next_pm, _tree)) => {
                        if &pm != next_pm {
                            panic!("TODO: invariant broken")
                        }
                    }
                },
                indexmap::map::Entry::Vacant(entry) => {
                    let t = IndexMap::new();
                    entry.insert(Some((pm, GAMTree::new(t))));
                }
            }
            let t = tree.get_mut(&om).unwrap().as_mut().unwrap();
            tree = &mut t.1.tree;
        }
        if let Some(om) = pos.cur {
            let entry = tree.entry(om);
            match entry {
                indexmap::map::Entry::Occupied(mut entry) => match entry.get_mut() {
                    None => {
                        entry.insert(None);
                    }
                    Some(_) => {
                        // NOP
                    }
                },
                indexmap::map::Entry::Vacant(entry) => {
                    entry.insert(None);
                }
            }
        }
    }
}

#[derive(Clone, Debug, new)]
pub struct EvenDynPosition(Vec<(OMove, PMove)>);

#[derive(Clone, Debug, new)]
pub struct OddDynPosition {
    pub seq: EvenDynPosition,
    pub cur: OMove,
}

#[derive(Clone, Debug, new)]
pub struct DynPosition {
    seq: EvenDynPosition,
    cur: Option<OMove>,
}

impl From<OMove> for crate::abt::OMove {
    fn from(om: OMove) -> Self {
        crate::abt::OMove::new(om.alphabet)
    }
}

impl From<EvenDynPosition> for EvenPosition {
    fn from(pos: EvenDynPosition) -> Self {
        EvenPosition(pos.0.into_iter().map(|(om, pm)| (om.into(), pm)).collect())
    }
}

impl From<OddDynPosition> for OddPosition {
    fn from(pos: OddDynPosition) -> Self {
        OddPosition::new(pos.seq.into(), pos.cur.into())
    }
}

impl DynPosition {
    fn even(self) -> Option<EvenDynPosition> {
        match self.cur {
            None => Some(self.seq),
            Some(_) => None,
        }
    }

    fn odd(self) -> Option<OddDynPosition> {
        match self.cur {
            None => None,
            Some(cur) => Some(OddDynPosition::new(self.seq, cur)),
        }
    }
}

impl From<DynPosition> for Position {
    fn from(pos: DynPosition) -> Position {
        match pos.cur {
            None => Position::Even(pos.seq.into()),
            Some(cur) => Position::Odd(OddPosition {
                seq: pos.seq.into(),
                cur: cur.into(),
            }),
        }
    }
}

impl From<OddDynPosition> for DynPosition {
    fn from(pos: OddDynPosition) -> DynPosition {
        DynPosition {
            seq: pos.seq,
            cur: Some(pos.cur),
        }
    }
}

impl From<EvenDynPosition> for DynPosition {
    fn from(pos: EvenDynPosition) -> DynPosition {
        DynPosition {
            seq: pos,
            cur: None,
        }
    }
}

pub fn init() -> (GAMTree, GAMTree, Turn) {
    (
        GAMTree::new(
            [(OMove::new(Alphabet::Init, 1), None)]
                .into_iter()
                .collect(),
        ),
        GAMTree::default(),
        Turn::new(Player::P, 2),
    )
}

fn step_p(
    s: &Strategy,
    mut phi: GAMTree,
    mut psi: GAMTree,
    step: usize,
) -> Result<(GAMTree, GAMTree)> {
    let tree = if step % 2 == 0 { &mut phi } else { &mut psi };
    let dyn_pos = tree
        .get(step - 1)
        .context(format!("Cannot find {step} in {tree:?}"))?
        .odd()
        .context("Should return odd position")?;
    let (dyn_pos, pos) = {
        let t = dyn_pos.clone();
        (dyn_pos, t.into())
    };
    let next = get_next_odd(s, &pos)?;
    let mut next_pos = dyn_pos.seq.0;
    next_pos.push((dyn_pos.cur, next.0.clone()));
    let next_pos = EvenDynPosition::new(next_pos);

    tree.add(DynPosition::new(next_pos, None));
    Ok((phi, psi))
}

fn step_o(
    _s: &Strategy,
    mut phi: GAMTree,
    mut psi: GAMTree,
    step: usize,
) -> Result<(GAMTree, GAMTree)> {
    let (ptree, otree) = if step % 2 == 0 {
        (&mut phi, &mut psi)
    } else {
        (&mut psi, &mut phi)
    };

    let dyn_pos = ptree
        .get(step)
        .context(format!("Cannot find ${step}"))?
        .even()
        .context("Should return even position")?;
    debug!("dyn_pos = {dyn_pos:?}");
    let pm = dyn_pos
        .0
        .last()
        .context("Should have at least one element")?
        .1
        .clone();
    let next_pos = match pm.pointer {
        None => OddDynPosition::new(EvenDynPosition::new(vec![]), OMove::new(pm.alphabet, step)),
        Some(ptr) => {
            let (OMove { step: idx, .. }, _) = dyn_pos.0.get(dyn_pos.0.len() - 1 - ptr).context(
                format!("Invalid index: len = {}, ptr = {}", dyn_pos.0.len(), ptr),
            )?;
            let next = otree
                .get(*idx)
                .context(format!("Cannot find ${step}"))?
                .even()
                .context("Should return even position")?;
            OddDynPosition::new(next, OMove::new(pm.alphabet, step))
        }
    };

    otree.add(next_pos.into());
    Ok((phi, psi))
}

pub enum Player {
    P,
    O,
}

#[derive(new)]
pub struct Turn {
    player: Player,
    step: usize,
}

pub fn step(
    s: &Strategy,
    phi: GAMTree,
    psi: GAMTree,
    turn: Turn,
) -> Result<(GAMTree, GAMTree, Turn)> {
    match turn {
        Turn {
            player: Player::P,
            step,
        } => {
            let res = step_p(s, phi, psi, step)?;
            Ok((res.0, res.1, Turn::new(Player::O, step)))
        }
        Turn {
            player: Player::O,
            step,
        } => {
            let res = step_o(s, phi, psi, step)?;
            Ok((res.0, res.1, Turn::new(Player::P, step + 1)))
        }
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_debug_snapshot;

    use super::*;
    use crate::{
        abt::compile,
        mocks::nf::{nf1, nf2},
        NF,
    };
    use test_log;

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

    #[test_log::test]
    fn test_step() {
        let (t, ts) = test1();
        let s = compile(&t, &ts);
        let (mut phi, mut psi, mut turn) = init();

        (phi, psi, turn) = step(&s, phi, psi, turn).unwrap();
        assert_debug_snapshot!(phi);
        assert_debug_snapshot!(psi);

        (phi, psi, turn) = step(&s, phi, psi, turn).unwrap();
        assert_debug_snapshot!(phi);
        assert_debug_snapshot!(psi);

        (phi, psi, turn) = step(&s, phi, psi, turn).unwrap();
        assert_debug_snapshot!(phi);
        assert_debug_snapshot!(psi);

        (phi, psi, _) = step(&s, phi, psi, turn).unwrap();
        assert_debug_snapshot!(phi);
        assert_debug_snapshot!(psi);
    }
}
