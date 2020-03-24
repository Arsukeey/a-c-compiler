#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Char,

    // integral types
    Int,
    Float,
    Void,
    Signed,
    Unsigned,
    Long,
    Double,

    // Structures
    If,
    Else,
    For,
    Do,
    While,
    Switch,
    Case,
    Default,
    Break,
    Continue,
    Return,

    Sizeof,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Symbol {
    OpeningParen,
    ClosingParen,

    OpeningCurlyBrac,
    ClosingCurlyBrac,

    OpeningSquareBrac,
    ClosingSquareBrac,

    Comma,
    Semicolon,
    Colon,
    Point,
    Arrow,
    Inc,
    Dec,
    Add,
    Sub,

    Ampersand,
    Asterisk,
    Div,
    Mod,
    Not,
    BitwiseNot,
    Shl,
    Shr,

    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    Xor,
    Or,
    LAnd,
    LOr,

    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignShl,
    AssignShr,
    AssignAnd,
    AssignXor,
    AssignOr,

    Hash,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IntBits {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenValue {
    Keyword(Keyword),
    Ident(String),
    NumInt(i64),
    NumFloat(f64),
    String(String),
    Char(char),
    Symbol(Symbol),
    Newline,
}

impl Keyword {
    pub fn get(input: &str) -> Result<Self, String> {
        match input {
            "char" => Ok(Keyword::Char),

            // integral types
            "int" => Ok(Keyword::Int),
            "float" => Ok(Keyword::Float),
            "void" => Ok(Keyword::Void),
            "signed" => Ok(Keyword::Signed),
            "unsigned" => Ok(Keyword::Unsigned),
            "long" => Ok(Keyword::Long),
            "double" => Ok(Keyword::Double),

            // Structures
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            "for" => Ok(Keyword::For),
            "do" => Ok(Keyword::Do),
            "while" => Ok(Keyword::While),
            "switch" => Ok(Keyword::Switch),
            "case" => Ok(Keyword::Case),
            "default" => Ok(Keyword::Default),
            "break" => Ok(Keyword::Break),
            "continue" => Ok(Keyword::Continue),
            "return" => Ok(Keyword::Return),
            "sizeof" => Ok(Keyword::Sizeof),

            x => Err(format!("{} not expected in Keyword::get()", x)),
        }
    }
}

impl Symbol {
    pub fn get(input: &str) -> Result<Self, String> {
        match input {
            "(" => Ok(Symbol::OpeningParen),
            ")" => Ok(Symbol::ClosingParen),

            "{" => Ok(Symbol::OpeningCurlyBrac),
            "}" => Ok(Symbol::ClosingCurlyBrac),

            "[" => Ok(Symbol::OpeningSquareBrac),
            "]" => Ok(Symbol::ClosingSquareBrac),

            "," => Ok(Symbol::Comma),
            ";" => Ok(Symbol::Semicolon),
            ":" => Ok(Symbol::Colon),
            "." => Ok(Symbol::Point),
            "->" => Ok(Symbol::Arrow),

            "++" => Ok(Symbol::Inc),
            "--" => Ok(Symbol::Dec),

            "+" => Ok(Symbol::Add),
            "-" => Ok(Symbol::Sub),
            "&" => Ok(Symbol::Ampersand),
            "*" => Ok(Symbol::Asterisk),
            "/" => Ok(Symbol::Div),
            "%" => Ok(Symbol::Mod),
            "!" => Ok(Symbol::Not),
            "~" => Ok(Symbol::BitwiseNot),
            "<<" => Ok(Symbol::Shl),
            ">>" => Ok(Symbol::Shr),

            "<" => Ok(Symbol::Lt),
            "<=" => Ok(Symbol::Le),
            ">" => Ok(Symbol::Gt),
            ">=" => Ok(Symbol::Ge),
            "==" => Ok(Symbol::Eq),
            "!=" => Ok(Symbol::Ne),
            "^" => Ok(Symbol::Xor),
            "|" => Ok(Symbol::Or),

            "&&" => Ok(Symbol::LAnd),
            "||" => Ok(Symbol::LOr),

            "=" => Ok(Symbol::Assign),
            "+=" => Ok(Symbol::AssignAdd),
            "-=" => Ok(Symbol::AssignSub),
            "*=" => Ok(Symbol::AssignMul),
            "/=" => Ok(Symbol::AssignDiv),
            "<<=" => Ok(Symbol::AssignShl),
            ">>=" => Ok(Symbol::AssignShr),
            "&=" => Ok(Symbol::AssignAnd),
            "^=" => Ok(Symbol::AssignXor),
            "|=" => Ok(Symbol::AssignOr),

            "#" => Ok(Symbol::Hash),
            x => Err(format!("{} not expected in Symbol:::get()", x)),
        }
    }
}

impl TokenValue {
    pub fn ident(&self) -> String {
        match self {
            Self::Ident(x) => x.to_string(),
            _ => "".to_string(),
        }
    }

    /*
    pub fn ident_mut(&mut self) -> &mut String {
        match self {
            Self::Ident(x) => &mut x.clone().to_string(),
            _ => &mut "".to_string(),
        }
    }
    */

    pub fn string(&self) -> String {
        match self {
            Self::String(x) => x.to_string(),
            _ => panic!(),
        }
    }
}
