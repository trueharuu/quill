use std::fmt::Display;

pub const ESCAPES: &[(&[&str], &str)] = &[
    (&["\\plus", "+"], "+"),
    (&["\\minus", "-"], "-"),
    (&["\\asterisk", "*"], "*"),
    (&["\\slash", "/"], "/"),
    (&["\\percent", "%"], "%"),
    (&["\\eq", "="], "="),
    (&["\\identical", "=="], "≡"),
    (&["\\ne", "!="], "≠"),
    (&["\\ge", ">="], "≥"),
    (&["\\gt", ">"], ">"),
    (&["\\le", "<="], "≤"),
    (&["\\lt", "<"], "<"),
    (&["\\defined", ":="], ":="),
    (&["\\arrow"], "->"),
    (&["\\union", "\\union"], "∪"),
    (&["\\intersection", "\\intersection"], "∩"),
    (&["\\emptyset", "\\emptyset"], "∅"),
    (&["\\in", "\\in"], "∈"),
    (&["\\notin", "\\notin"], "∉"),
    (&["\\contains", "\\contains"], "∋"),
    (&["\\notcontains", "\\notcontains"], "∌"),
    (&["\\forall", "\\forall"], "∀"),
    (&["\\exists", "\\exists"], "∃"),
    (&["\\notexists", "\\notexists"], "∄"),
    (&["\\subset", "\\subset"], "⊂"),
    (&["\\subseteq", "\\subseteq"], "⊆"),
    (&["\\subsetne", "\\subsetne"], "⊄"),
    (&["\\superset", "\\superset"], "⊃"),
    (&["\\superseteq", "\\superseteq"], "⊇"),
    (&["\\supersetne", "\\supersetne"], "⊅"),
    (&["\\and", "&&"], "∧"),
    (&["\\or", "||"], "∨"),
    (&["\\xor", "^^"], "⊻"),
    (&["\\nand", "~&"], "⊼"),
    (&["\\nor", "~|"], "⊽"),
    (&["\\not", "~"], "¬"),
    (&["\\cross", "\\cross"], "×"),
    (&["\\dot", "\\dot"], "·"),
    (&["\\infinity", "\\infinity"], "∞"),
];

pub fn escape(t: impl Display) -> String {
    let mut u = format!("{t}");
    for (from, to) in ESCAPES {
        for f in *from {
            u = u.replace(f, to);
        }
    }

    u
}
