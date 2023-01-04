pub mod abt;
pub mod gam;
pub mod kam;
pub mod mocks;
pub mod sam;

pub enum Term {
    Abs { name: String, body: Box<Term> },
    App { fun: Box<Term>, arg: Box<Term> },
    Var { name: String },
}

#[derive(Debug, PartialEq, Eq)]
pub struct NF {
    names: Vec<String>,
    head: String,
    args: Vec<NF>,
}
