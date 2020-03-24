use crate::enums::{Keyword, Symbol, TokenValue};
use std::iter::Peekable;
use std::str::Chars;

/*
pub struct Token {
    pub value: TokenValue,
    pub space: bool, // leading space

    // for error outputting
    pub line: usize,
    pub column: usize,
}
 */

type Token = TokenValue;
type LexResult = Result<Token, String>;

macro_rules! matches {
    ($e:expr, $p:pat) => {
        match $e {
            $p => true,
            _ => false,
        }
    };
}

/*
impl Token {
    pub fn new(value: TokenValue, line: usize, column: usize) -> Self {
        Self {
            value,
            space: false,

            line,
            column,
        }
    }
}
*/

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

/// Implementation of a DFA.
impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            input: source.chars().peekable(),
        }
    }

    fn try_convert_to_symbol(&mut self, token: Token) -> Token {
        let v = token.ident();

        let symbol = Symbol::get(v.as_str());
        if let Ok(sym) = symbol {
            return Token::Symbol(sym);
        } else {
            return token;
        }
    }

    fn try_convert_to_keyword(&mut self, token: Token) -> Token {
        let v = token.ident();

        if v.len() > 0 && v.chars().nth(0).unwrap().is_alphanumeric() {
            let keyword = Keyword::get(&v);
            if let Ok(keyw) = keyword {
                return Token::Keyword(keyw);
            } else {
                return token;
            }
        }
        token
    }

    fn get_next(&mut self) -> Result<char, String> {
        if let Some(x) = self.input.next() {
            Ok(x)
        } else {
            Err("eof reached".to_string())
        }
    }

    fn peek_next(&mut self) -> Result<&char, String> {
        if let Some(x) = self.input.peek() {
            Ok(x)
        } else {
            Err("eof reached".to_string())
        }
    }

    fn lex_identifier(&mut self, c: char) -> LexResult {
        let mut ident = String::new();
        ident.push(c);

        loop {
            if let Ok(&c) = self.peek_next() {
                if c.is_alphanumeric() || c == '_' {
                    ident.push(self.get_next()?);
                } else {
                    break Ok(Token::Ident(ident));
                }
            }
        }
    }

    fn lex_number_literal(&mut self, c: char) -> LexResult {
        let mut num = "".to_string();
        num.push(c);
        let mut is_float = false;

        loop {
            let c = *self.peek_next()?;
            if !c.is_alphanumeric() && c != '.' {
                break;
            } else {
                is_float = is_float || c == '.';
                num.push(self.get_next()?);
            }
        }

        if is_float {
            num = num
                .trim_end_matches(|c| match c {
                    'a'..='z' | 'A'..='Z' | '+' | '-' => true,
                    _ => false,
                })
                .to_string();

            let f: f64 = num.parse().unwrap();

            return Ok(Token::NumFloat(f));
        }

        let num = if num.len() > 2 && num.chars().nth(1).unwrap() == 'x' {
            parse_hex_num(&num[2..]).0 // .1 of pair is the suffix
        } else if num.chars().nth(0) == Some('0') {
            parse_oct_num(&num[1..]).0
        } else {
            parse_dec_num(num.as_str()).0
        };

        Ok(Token::NumInt(num))
    }

    fn lex_symbol(&mut self, c: char) -> LexResult {
        let mut symbol = String::new();
        symbol.push(c);
        if let Ok(&next_c) = self.peek_next() {
            match c {
                '+' | '-' => {
                    if next_c == '=' || next_c == '>' || next_c == c {
                        symbol.push(self.get_next()?);
                    }
                }
                '*' | '/' | '%' | '=' | '^' | '!' => {
                    if next_c == '=' {
                        symbol.push(self.get_next()?);
                    }
                }
                '<' | '>' | '&' | '|' => {
                    if next_c == c || next_c == '=' {
                        symbol.push(self.get_next()?);
                    }
                }
                _ => (),
            }
        }

        Ok(Token::Symbol(Symbol::get(&symbol).unwrap()))
    }

    fn lex_escaped_char(&mut self) -> Result<char, String> {
        let c = self.get_next()?;
        match c {
            '\'' | '"' | '?' | '\\' => Ok(c),
            'a' => Ok('\x07'),
            'b' => Ok('\x08'),
            'f' => Ok('\x0c'),
            'n' => Ok('\x0a'),
            'r' => Ok('\x0d'),
            't' => Ok('\x09'),
            'v' => Ok('\x0b'),
            'x' => {
                let mut hex = "".to_string();
                loop {
                    let c = self.get_next()?;
                    if c.is_alphanumeric() {
                        hex.push(c);
                    } else {
                        break;
                    }
                    self.get_next()?;
                }
                Ok(parse_hex_num(hex.as_str()).0 as i32 as u8 as char)
            }
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' => {
                // if '0', check whether octal number \nnn or null \0
                if self.get_next()?.is_numeric() {
                    let mut oct = "".to_string();
                    oct.push(c);
                    loop {
                        let c = self.get_next()?;
                        oct.push(c);
                        if !c.is_numeric() {
                            oct.pop();
                            break;
                        }
                    }
                    Ok(parse_oct_num(oct.as_str()).0 as i32 as u8 as char)
                } else {
                    Ok('\x00')
                }
            }
            _ => Ok(c),
        }
    }

    fn lex_string(&mut self) -> LexResult {
        let mut string = String::new();

        loop {
            match self.get_next()? {
                '"' => break Ok(Token::String(string)),
                '\\' => string.push(self.lex_escaped_char()?),
                c => string.push(c),
            }
        }
    }

    fn lex_char(&mut self) -> LexResult {
        let c = {
            let v = self.get_next()?;
            if v == '\\' {
                self.lex_escaped_char()?
            } else {
                v
            }
        };

        if self.get_next()? != '\'' {
            panic!("missing terminating \' char");
        }

        Ok(Token::Char(c))
    }

    fn lex_primary(&mut self) -> LexResult {
        let c = self.get_next()?;
        match c {
            'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier(c),

            // TODO: set leading space
            ' ' | '\t' => self.lex_primary(),

            '0'..='9' => self.lex_number_literal(c),
            '\"' => self.lex_string(),
            '\'' => self.lex_char(),
            '\n' => Ok(Token::Newline),

            '\\' => {
                while self.get_next()? != '\n' {}
                self.lex_primary()
            }

            '/' => {
                // Handle multiline comments
                if self.peek_next()? == &'*' {
                    self.get_next()?; // eat '*'

                    let mut last = ' ';
                    while !(last == '*' && self.peek_next()? == &'/') {
                        last = self.get_next()?;
                    }
                    self.get_next()?; // eat '/'

                    self.lex_primary()
                }
                // Handle singleline comments
                else if self.peek_next()? == &'/' {
                    self.get_next()?;
                    while self.peek_next()? != &'\n' {
                        self.get_next()?;
                    }

                    self.lex_primary()
                } else {
                    self.lex_symbol(c)
                }
            }
            '(' | ')' | '{' | '}' | '[' | ']' | ',' | ';' | ':' | '.' | '+' | '-' | '*' | '%'
            | '!' | '~' | '<' | '>' | '^' | '|' | '&' | '=' | '#' => self.lex_symbol(c),

            _ => {
                // TODO: should handle next file
                if let Err(x) = self.peek_next() {
                    Err(x)
                } else {
                    self.lex_primary()
                }
            }
        }
    }

    pub fn read_token(&mut self) -> LexResult {
        let token = self.lex_primary();
        token.and_then(|tok| match tok {
            Token::Newline => self.read_token(),
            Token::Ident(_) => match self.try_convert_to_keyword(tok.clone()) {
                Token::Keyword(token) => Ok(Token::Keyword(token)),
                Token::Ident(_) => Ok(self.try_convert_to_symbol(tok)),
                _ => Ok(tok),
            },
            // Token::Ident(_) => Ok(self.try_convert_to_symbol(tok)),
            _ => Ok(tok),
        })
    }

    fn lex_single_arg(&mut self, end: &mut bool) -> Result<Vec<Token>, String> {
        let mut nest = 0;
        let mut arg = Vec::new();
        loop {
            let tok = self.lex_primary()?;
            let val = tok.ident();
            if nest == 0 {
                match val.as_str() {
                    ")" => {
                        *end = true;
                        break;
                    }
                    "," => break,
                    _ => {}
                }
            }
            match val.as_str() {
                "(" => nest += 1,
                ")" => nest -= 1,
                _ => {}
            }
            arg.push(tok);
        }
        Ok(arg)
    }
}

fn parse_hex_num(v: &str) -> (i64, String) {
    let mut suffix = String::new();
    let n = v.chars().fold(0, |n, c| match c {
        '0'..='9' | 'A'..='F' | 'a'..='f' => n * 16 + c.to_digit(16).unwrap() as u64,
        _ => {
            suffix.push(c);
            n
        }
    });
    (n as i64, suffix)
}

fn parse_dec_num(v: &str) -> (i64, String) {
    let mut suffix = "".to_string();
    let n = v.chars().fold(0, |n, c| match c {
        '0'..='9' => n * 10 + c.to_digit(10).unwrap() as u64,
        _ => {
            suffix.push(c);
            n
        }
    });
    (n as i64, suffix)
}

fn parse_oct_num(v: &str) -> (i64, String) {
    let mut suffix = "".to_string();
    let n = v.chars().fold(0, |n, c| match c {
        '0'..='7' => n * 8 + c.to_digit(8).unwrap() as u64,
        _ => {
            suffix.push(c);
            n
        }
    });
    (n as i64, suffix)
}

#[test]
fn assign_number() {
    let test_str = "int x=0xA ;";
    let mut l = Lexer::new(test_str);
    assert_eq!(l.read_token(), Ok(Token::Keyword(Keyword::Int)));
    assert_eq!(l.read_token(), Ok(Token::Ident("x".to_string())));
    assert_eq!(l.read_token(), Ok(Token::Symbol(Symbol::Assign)));
    assert_eq!(l.read_token(), Ok(Token::NumInt(10)));
    assert_eq!(l.read_token(), Ok(Token::Symbol(Symbol::Semicolon)));
    assert_eq!(l.read_token(), Err("eof reached".to_string()));
}

#[test]
fn if_else() {
    let test_str = "if (x == 3) { y; } else { z; }";
    let mut l = Lexer::new(test_str);

    assert_eq!(l.read_token(), Ok(Token::Keyword(Keyword::If)));
    assert_eq!(l.read_token(), Ok(Token::Symbol(Symbol::OpeningParen)));
    assert_eq!(l.read_token(), Ok(Token::Ident("x".to_string())));
    assert_eq!(l.read_token(), Ok(Token::Symbol(Symbol::Eq)));
    assert_eq!(l.read_token(), Ok(Token::NumInt(3)));
    assert_eq!(l.read_token(), Ok(Token::Symbol(Symbol::ClosingParen)));

    assert_eq!(l.read_token(), Ok(Token::Symbol(Symbol::OpeningCurlyBrac)));
    assert_eq!(l.read_token(), Ok(Token::Ident("y".to_string())));
    assert_eq!(l.read_token(), Ok(Token::Symbol(Symbol::Semicolon)));
    assert_eq!(l.read_token(), Ok(Token::Symbol(Symbol::ClosingCurlyBrac)));

    assert_eq!(l.read_token(), Ok(Token::Keyword(Keyword::Else)));
    assert_eq!(l.read_token(), Ok(Token::Symbol(Symbol::OpeningCurlyBrac)));
    assert_eq!(l.read_token(), Ok(Token::Ident("z".to_string())));
    assert_eq!(l.read_token(), Ok(Token::Symbol(Symbol::Semicolon)));
    assert_eq!(
        l.read_token().unwrap(),
        Token::Symbol(Symbol::ClosingCurlyBrac)
    );
    assert_eq!(l.read_token(), Err("eof reached".to_string()));
}

#[test]
fn string_and_char() {
    let test_str = "char abc = 'a'; \"oie\"";
    let mut l = Lexer::new(test_str);
    assert_eq!(l.read_token(), Ok(Token::Keyword(Keyword::Char)));
    assert_eq!(l.read_token(), Ok(Token::Ident("abc".to_string())));
    assert_eq!(l.read_token(), Ok(Token::Symbol(Symbol::Assign)));
    assert_eq!(l.read_token(), Ok(Token::Char('a')));
    assert_eq!(l.read_token(), Ok(Token::Symbol(Symbol::Semicolon)));
    assert_eq!(l.read_token(), Ok(Token::String("oie".to_string())));
    assert_eq!(l.read_token(), Err("eof reached".to_string()));
}

#[test]
fn comment() {
    let test_str = "// dsadsadsadas\nx // aa \n/*dsada\n\n*/";
    let mut l = Lexer::new(test_str);
    assert_eq!(l.read_token(), Ok(Token::Ident("x".to_string())));
    assert_eq!(l.read_token(), Err("eof reached".to_string()));
}
