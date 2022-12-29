// \x.u(\y.x)
use crate::NF;

// \x.u(\y.x)
pub fn nf1() -> NF {
    NF {
        names: vec!["x".to_owned()],
        head: "u".to_owned(),
        args: vec![NF {
            names: vec!["y".to_owned()],
            head: "x".to_owned(),
            args: vec![],
        }],
    }
}

// \r.r(r(z))
pub fn nf2() -> NF {
    NF {
        names: vec!["r".to_owned()],
        head: "r".to_owned(),
        args: vec![NF {
            names: vec![],
            head: "r".to_owned(),
            args: vec![NF {
                names: vec![],
                head: "z".to_owned(),
                args: vec![],
            }],
        }],
    }
}
