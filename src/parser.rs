use crate::ast::AST;
use crate::enums::{Keyword, Symbol, TokenValue};
use crate::lexer::{Lexer, Token};
use crate::types::*;

use std::collections::HashMap;
use std::collections::VecDeque;

pub struct Parser<'a> {
    pub lexer: &'a mut Lexer<'a>,
    pub err_counts: usize,
    env: Env<AST>,
}

type ParseResult = Result<AST, String>;

pub struct Env<T: Clone>(pub VecDeque<HashMap<String, T>>);

impl<T: Clone> Env<T> {
    fn new() -> Env<T> {
        let mut env = VecDeque::new();
        env.push_back(HashMap::new());
        Env(env)
    }
    fn push(&mut self) {
        let localenv = (*self.0.back().unwrap()).clone();
        self.0.push_back(localenv);
    }
    fn pop(&mut self) {
        self.0.pop_back();
    }
    fn add(&mut self, name: String, val: T) {
        self.0.back_mut().unwrap().insert(name, val);
    }
    fn add_globally(&mut self, name: String, val: T) {
        self.0[0].insert(name.clone(), val.clone());
        self.0.back_mut().unwrap().insert(name, val);
    }
    fn is_local(&self) -> bool {
        self.0.len() > 1
    }
    fn back_mut(&mut self) -> Option<&mut HashMap<String, T>> {
        self.0.back_mut()
    }
    fn get(&mut self, name: &str) -> Option<&T> {
        self.0.back_mut().unwrap().get(name)
    }
    fn contains(&mut self, name: &str) -> bool {
        self.0.back_mut().unwrap().contains_key(name)
    }
}

impl<'a> Parser<'a> {
    fn expect_symbol(&mut self, sym: Symbol) -> Result<(), String> {
        let t = self.lexer.read_token()?;
        if let Token::Symbol(s) = t {
            if sym != s {
                return Err(format!("Expected '{}', found {}.", sym, s));
            }
        } else {
            return Err(format!("Expected '{}', found {}.", sym, t));
        }
        Ok(())
    }

    fn expect_keyword(&mut self, kw: Keyword) -> Result<(), String> {
        let t = self.lexer.read_token()?;
        if let Token::Keyword(k) = t {
            if kw != k {
                return Err(format!("Expected '{}', found {}.", kw, k));
            }
        } else {
            return Err(format!("Expected '{}', found {}.", kw, t));
        }
        Ok(())
    }

    fn is_type(&mut self, token: &Token) -> bool {
        if let Token::Keyword(kw) = token {
            match kw {
                Keyword::Typedef
                | Keyword::Extern
                | Keyword::Static
                | Keyword::Auto
                | Keyword::Register
                | Keyword::Const
                | Keyword::Volatile
                | Keyword::Void
                | Keyword::Signed
                | Keyword::Unsigned
                | Keyword::Char
                | Keyword::Int
                | Keyword::Short
                | Keyword::Long
                | Keyword::Float
                | Keyword::Double
                | Keyword::Struct
                | Keyword::Enum
                | Keyword::Union
                | Keyword::Noreturn
                | Keyword::Inline
                | Keyword::Restrict => true,
                _ => false,
            }
        } else if let Token::Ident(ident) = token {
            match self.env.get(ident.as_str()) {
                Some(ast) => match ast {
                    AST::Typedef(_, _) => true,
                    _ => false,
                },
                None => false,
            }
        } else {
            false
        }
    }

    fn skip_until(&mut self, sym: Symbol) {
        let ts = Token::Symbol(sym);
        while match self.lexer.read_token() {
            Ok(tok) => tok != ts,
            Err(_) => false,
        } {}
    }

    fn parse_case_label(&mut self) -> ParseResult {
        // let expr = try!(self.read_expr());
        self.expect_symbol(Symbol::Colon)?;
        // Ok(AST::Case(Box::new(expr)))
        unimplemented!()
    }
    fn parse_default_label(&mut self) -> ParseResult {
        self.expect_symbol(Symbol::Colon)?;
        Ok(AST::DefaultL)
    }
    fn parse_goto_stmt(&mut self) -> ParseResult {
        let label_name = self.lexer.read_token()?.ident();
        self.expect_symbol(Symbol::Semicolon)?;
        Ok(AST::Goto(label_name))
    }
    fn parse_label(&mut self, tok: Token) -> ParseResult {
        let label_name = tok.ident();
        self.expect_symbol(Symbol::Colon)?;
        Ok(AST::Label(label_name))
    }
    fn parse_continue_stmt(&mut self) -> ParseResult {
        self.expect_symbol(Symbol::Semicolon)?;
        Ok(AST::Continue)
    }
    fn parse_break_stmt(&mut self) -> ParseResult {
        self.expect_symbol(Symbol::Semicolon)?;
        Ok(AST::Break)
    }

    fn read_if_stmt(&mut self) -> ParseResult {
        self.expect_symbol(Symbol::OpeningParen)?;
        unimplemented!();
    }
}
