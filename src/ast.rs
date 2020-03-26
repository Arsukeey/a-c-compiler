use crate::types::*;

#[derive(Debug, PartialEq, Clone)]
pub enum AST {
    Int(i64),
    Float(f64),
    Char(i32),
    String(String),
    Typedef(Type, String), // from, to ( typedef from to; )
    TypeCast(Box<AST>, Type),
    Load(Box<AST>),
    Variable(Type, String),
    VariableDecl(Type, String, StorageClass, Option<Box<AST>>), // type, name, init val
    ConstArray(Vec<AST>),
    ConstStruct(Vec<AST>),
    UnaryOp(Box<AST>, UnaryOps),
    BinaryOp(Box<AST>, Box<AST>, BinOps),
    TernaryOp(Box<AST>, Box<AST>, Box<AST>), // cond then else
    FuncDef(Type, Vec<String>, String, Box<AST>), // functype, param names, func name, body
    Block(Vec<AST>),
    Compound(Vec<AST>),
    If(Box<AST>, Box<AST>, Box<AST>), // cond, then stmt, else stmt
    For(Box<AST>, Box<AST>, Box<AST>, Box<AST>), // init, cond, step, body
    While(Box<AST>, Box<AST>),        // cond, body
    DoWhile(Box<AST>, Box<AST>),      // cond, body
    Switch(Box<AST>, Box<AST>),       // cond, stmt
    Case(Box<AST>),
    DefaultL,
    Goto(String),  // label name
    Label(String), // label name
    FuncCall(Box<AST>, Vec<AST>),
    StructRef(Box<AST>, String), // String is name of struct field
    Break,
    Continue,
    Return(Option<Box<AST>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOps {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    LAnd,
    LOr,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Shl,
    Shr,
    Comma,
    Assign,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOps {
    LNot,
    BNot,
    Minus,
    // TODO: Inc and Dec is actually POSTFIX.
    Inc,
    Dec,
    Deref,
    Addr,
    Sizeof,
    // TODO: add Cast, Sizeof
}

impl AST {
    // since c doesn't have bools, we return i64 instead of an (i64, bool) enum.
    pub fn eval(&self) -> Result<i64, String> {
        Ok(match self {
            AST::Int(n) => *n,
            AST::TypeCast(ref e, _) => e.eval()?,
            AST::UnaryOp(ref e, UnaryOps::LNot) => (e.eval()? == 0) as i64,
            AST::UnaryOp(ref e, UnaryOps::BNot) => !e.eval()?,
            AST::UnaryOp(ref e, UnaryOps::Minus) => -e.eval()?,
            AST::UnaryOp(ref e, UnaryOps::Inc) => e.eval()? + 1,
            AST::UnaryOp(ref e, UnaryOps::Dec) => e.eval()? - 1,
            AST::UnaryOp(ref e, UnaryOps::Deref) => e.eval()?,
            AST::UnaryOp(ref e, UnaryOps::Addr) => e.eval()?,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Add) => lhs.eval()? + rhs.eval()?,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Sub) => lhs.eval()? - rhs.eval()?,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Mul) => lhs.eval()? * rhs.eval()?,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Div) => lhs.eval()? / rhs.eval()?,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Rem) => lhs.eval()? % rhs.eval()?,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::And) => lhs.eval()? & rhs.eval()?,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Or) => lhs.eval()? | rhs.eval()?,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Xor) => lhs.eval()? ^ rhs.eval()?,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::LAnd) => lhs.eval()? & rhs.eval()?,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::LOr) => lhs.eval()? | rhs.eval()?,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Eq) => (lhs.eval()? == rhs.eval()?) as i64,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Ne) => (lhs.eval()? != rhs.eval()?) as i64,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Lt) => (lhs.eval()? < rhs.eval()?) as i64,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Gt) => (lhs.eval()? > rhs.eval()?) as i64,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Le) => (lhs.eval()? <= rhs.eval()?) as i64,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Ge) => (lhs.eval()? >= rhs.eval()?) as i64,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Shl) => lhs.eval()? << rhs.eval()?,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Shr) => lhs.eval()? >> rhs.eval()?,
            AST::BinaryOp(ref lhs, ref rhs, BinOps::Comma) => {
                lhs.eval()?;
                rhs.eval()?
            }
            AST::BinaryOp(ref lhs, ref rhs, _) => {
                lhs.eval()?;
                rhs.eval()?;
                0
            }
            AST::TernaryOp(ref cond, ref lhs, ref rhs) => {
                if cond.eval()? != 0 {
                    lhs.eval()?
                } else {
                    rhs.eval()?
                }
            }
            _ => return Err("unexpected eval".to_string()),
        })
    }
}
