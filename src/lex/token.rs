use std::fmt::{Debug, Display};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Eof,
    Natural(usize),
    Real(f64),
    Label(String),

    True,  // true
    False, // false

    // symbols, syntax
    Plus,      // +
    Minus,     // -
    Asterisk,  // *
    Slash,     // /
    Percent,   // %
    Semicolon, // ;
    // Backslash, // \
    Eq,          // =
    Ne,          // !=, ≠
    Gt,          // >
    Ge,          // >=, ≥
    Lt,          // <
    Le,          // <=, ≤
    Identical,   // ≡, ==
    Defined,     // :=
    DoubleColon, // ::

    // other
    Apostrophe, // '
    Colon,      // :
    Arrow,      // ->, →

    // symbols, grouping
    LeftBracket,  // [
    RightBracket, // ]
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    Comma,        // ,

    // symbols, set notation
    Union,        // ∪
    Intersection, // ∩
    EmptySet,     // ∅
    In,           // ∈
    NotIn,        // ∉
    Contains,     // ∋
    NotContains,  // ∌
    ForAll,       // ∀
    Exists,       // ∃
    NotExists,    // ∄
    Subset,       // ⊂
    SubsetEq,     // ⊆
    SubsetNe,     // ⊄
    Superset,     // ⊃
    SupersetEq,   // ⊇
    SupersetNe,   // ⊅

    // symbols, logic
    And,       // ∧
    Or,        // ∨
    Ampersand, // &
    Pipe,      // |
    Xor,       // ⊻
    Caret,     // ^
    Nand,      // ⊼
    Nor,       // ⊽
    Not,       // ¬, ~

    // symbol, general math
    Cross,      // ×
    Interpunct, // ·
    Infinity,   // ∞
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Eof => String::new(),
                Self::Natural(u) => u.to_string(),
                Self::Real(f) => f.to_string(),
                Self::True => "true".to_string(),
                Self::False => "false".to_string(),
                Self::Label(l) => (*l).to_string(),
                Self::Semicolon => ";".to_string(),

                // symbols, syntax
                Self::Plus => "+".to_string(),
                Self::Minus => "-".to_string(),
                Self::Asterisk => "*".to_string(),
                Self::Slash => "/".to_string(),
                Self::Percent => "%".to_string(),
                // Self::Backslash => "\\".to_string(),
                Self::Eq => "=".to_string(),
                Self::Ne => "≠".to_string(),
                Self::Gt => ">".to_string(),
                Self::Ge => "≥".to_string(),
                Self::Lt => "<".to_string(),
                Self::Le => "≤".to_string(),
                Self::Identical => "≡".to_string(),
                Self::Defined => ":=".to_string(),
                Self::DoubleColon => "::".to_string(),

                // other
                Self::Apostrophe => "'".to_string(),
                Self::Colon => ":".to_string(),
                Self::Arrow => "→".to_string(),

                // symbols, grouping
                Self::LeftBracket => "[".to_string(),
                Self::RightBracket => "]".to_string(),
                Self::LeftParen => "(".to_string(),
                Self::RightParen => ")".to_string(),
                Self::LeftBrace => "{".to_string(),
                Self::RightBrace => "}".to_string(),
                Self::Comma => ",".to_string(),

                // symbols, set notation
                Self::Union => "∪".to_string(),
                Self::Intersection => "∩".to_string(),
                Self::EmptySet => "∅".to_string(),
                Self::ForAll => "∀".to_string(),
                Self::Exists => "∃".to_string(),
                Self::NotExists => "∄".to_string(),
                Self::In => "∈".to_string(),
                Self::NotIn => "∉".to_string(),
                Self::Contains => "∋".to_string(),
                Self::NotContains => "∌".to_string(),

                Self::Subset => "⊂".to_string(),
                Self::SubsetEq => "⊆".to_string(),
                Self::SubsetNe => "⊄".to_string(),
                Self::Superset => "⊃".to_string(),
                Self::SupersetEq => "⊇".to_string(),
                Self::SupersetNe => "⊅".to_string(),

                // symbols, logic
                Self::And => "∧".to_string(),
                Self::Or => "∨".to_string(),
                Self::Ampersand => "&".to_string(),
                Self::Pipe => "|".to_string(),
                Self::Xor => "⊻".to_string(),
                Self::Caret => "^".to_string(),
                Self::Nand => "⊼".to_string(),
                Self::Nor => "⊽".to_string(),
                Self::Not => "¬".to_string(),

                // symbol, general math
                Self::Cross => "×".to_string(),
                Self::Interpunct => "·".to_string(),
                Self::Infinity => "∞".to_string(),
                // #[allow(unreachable_patterns)]
                // c => format!("{c:?}"),
            }
        )
    }
}
