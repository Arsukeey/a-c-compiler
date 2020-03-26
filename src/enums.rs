use std::fmt;

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
    Short,

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
    Goto,

    Sizeof,

    Extern,
    Static,
    Auto,
    Restrict,
    Register,
    Const,
    ConstExpr,
    Volatile,

    Typedef,
    Struct,
    Enum,
    Union,

    Noreturn,
    Inline,
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

    Question,
    Hash,
    Vararg,
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
            "short" => Ok(Keyword::Short),

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

            "extern" => Ok(Keyword::Extern),
            "static" => Ok(Keyword::Static),
            "auto" => Ok(Keyword::Auto),
            "restrict" => Ok(Keyword::Restrict),
            "register" => Ok(Keyword::Register),
            "const" => Ok(Keyword::Const),
            "constexpr" => Ok(Keyword::ConstExpr),
            "volatile" => Ok(Keyword::Volatile),

            "typedef" => Ok(Keyword::Typedef),
            "struct" => Ok(Keyword::Struct),
            "enum" => Ok(Keyword::Enum),
            "union" => Ok(Keyword::Union),

            "noreturn" => Ok(Keyword::Noreturn),
            "inline" => Ok(Keyword::Inline),
            "goto" => Ok(Keyword::Goto),

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
            "?" => Ok(Symbol::Question),
            "..." => Ok(Symbol::Vararg),
            x => Err(format!("{} not expected in Symbol:::get()", x)),
        }
    }
}

impl fmt::Display for TokenValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Keyword(kw) => write!(f, "{}", kw),
            Self::Ident(st) => write!(f, "{}", st),
            Self::NumInt(n) => write!(f, "{}", n),
            Self::NumFloat(n) => write!(f, "{}", n),
            Self::String(st) => write!(f, "{}", st),
            Self::Char(c) => write!(f, "{}", c),
            Self::Symbol(sym) => write!(f, "{}", sym),
            Self::Newline => write!(f, "\n"),
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let m = match self {
            Keyword::Char => "char",
            Keyword::Int => "int",
            Keyword::Float => "float",
            Keyword::Void => "void",
            Keyword::Signed => "signed",
            Keyword::Unsigned => "unsigned",
            Keyword::Long => "long",
            Keyword::Double => "double",
            Keyword::Short => "short",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::For => "for",
            Keyword::Do => "do",
            Keyword::While => "while",
            Keyword::Switch => "switch",
            Keyword::Case => "case",
            Keyword::Default => "default",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Return => "return",
            Keyword::Sizeof => "sizeof",
            Keyword::Extern => "extern",
            Keyword::Static => "static",
            Keyword::Auto => "auto",
            Keyword::Restrict => "restrict",
            Keyword::Register => "register",
            Keyword::Const => "const",
            Keyword::ConstExpr => "constexpr",
            Keyword::Volatile => "volatile",
            Keyword::Typedef => "typedef",
            Keyword::Struct => "struct",
            Keyword::Enum => "enum",
            Keyword::Union => "union",
            Keyword::Noreturn => "noreturn",
            Keyword::Inline => "inline",
            Keyword::Goto => "goto",
        };

        write!(f, "{}", m)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let m = match self {
            Symbol::OpeningParen => "(",
            Symbol::ClosingParen => ")",

            Symbol::OpeningCurlyBrac => "{",
            Symbol::ClosingCurlyBrac => "}",

            Symbol::OpeningSquareBrac => "[",
            Symbol::ClosingSquareBrac => "]",

            Symbol::Comma => ",",
            Symbol::Semicolon => ";",
            Symbol::Colon => ":",
            Symbol::Point => ".",
            Symbol::Arrow => "->",
            Symbol::Inc => "++",
            Symbol::Dec => "--",
            Symbol::Add => "+",
            Symbol::Sub => "-",

            Symbol::Ampersand => "&",
            Symbol::Asterisk => "*",
            Symbol::Div => "/",
            Symbol::Mod => "%",
            Symbol::Not => "!",
            Symbol::BitwiseNot => "~",
            Symbol::Shl => "<<",
            Symbol::Shr => ">>",

            Symbol::Lt => "<",
            Symbol::Le => "<=",
            Symbol::Gt => ">",
            Symbol::Ge => ">=",
            Symbol::Eq => "==",
            Symbol::Ne => "!=",
            Symbol::Xor => "^",
            Symbol::Or => "|",
            Symbol::LAnd => "&&",
            Symbol::LOr => "||",

            Symbol::Assign => "=",
            Symbol::AssignAdd => "+=",
            Symbol::AssignSub => "-=",
            Symbol::AssignMul => "*=",
            Symbol::AssignDiv => "/=",
            Symbol::AssignShl => "<<=",
            Symbol::AssignShr => ">>=",
            Symbol::AssignAnd => "&=",
            Symbol::AssignXor => "^=",
            Symbol::AssignOr => "|=",

            Symbol::Hash => "#",
            Symbol::Question => "?",
            Symbol::Vararg => "...",
        };

        write!(f, "{}", m)
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
