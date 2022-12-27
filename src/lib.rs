pub mod kam;

pub enum Term {
    Abs { name: String, body: Box<Term> },
    App { fun: Box<Term>, arg: Box<Term> },
    Var { name: String },
}

#[derive(Debug, PartialEq, Eq)]
pub struct NF {
    name: Vec<String>,
    head: String,
    args: Vec<NF>,
}
