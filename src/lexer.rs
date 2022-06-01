use std::fmt;
use std::iter::Peekable;

#[derive(Default, Debug, Clone)]
pub struct Loc {
    pub file_path: Option<String>,
    pub row: usize,
    pub col: usize,
}

macro_rules! loc_here {
    () => {
        Loc {
            file_path: Some(file!().to_string()),
            row: line!() as usize,
            col: column!() as usize
        }
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.file_path {
            Some(file_path) => write!(f, "{}:{}:{}", file_path, self.row, self.col),
            None => write!(f, "{}:{}", self.row, self.col),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub enum TokenKind {
    Ident,
    Str,

    // Keywords
    Undo,
    Quit,
    Delete,
    Load,
    Save,

    // Special Characters
    OpenParen,
    CloseParen,
    Comma,
    Equals,
    Colon,
    DoubleColon,
    OpenCurly,
    CloseCurly,
    Bar,
    Bang,

    // Binary Operators
    Plus,
    Dash,
    Asterisk,
    Slash,
    Caret,
    Percent,
    EqualsEquals,

    // Terminators
    Invalid,
    UnclosedStr,
    End,
}

fn keyword_by_name(text: &str) -> Option<TokenKind> {
    match text {
        "quit"   => Some(TokenKind::Quit),
        "undo"   => Some(TokenKind::Undo),
        "delete" => Some(TokenKind::Delete),
        "load"   => Some(TokenKind::Load),
        "save"   => Some(TokenKind::Save),
        _ => None,
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenKind::*;
        match self {
            Ident => write!(f, "identifier"),
            Str => write!(f, "string"),
            Undo => write!(f, "`undo`"),
            Quit => write!(f, "`quit`"),
            Delete => write!(f, "`delete`"),
            Load => write!(f, "`load`"),
            Save => write!(f, "`save`"),
            OpenParen => write!(f, "open paren"),
            CloseParen => write!(f, "close paren"),
            OpenCurly => write!(f, "open curly"),
            CloseCurly => write!(f, "close curly"),
            Comma => write!(f, "comma"),
            Equals => write!(f, "equals"),
            EqualsEquals => write!(f, "double equals"),
            Colon => write!(f, "colon"),
            DoubleColon => write!(f, "double colon"),
            Percent => write!(f, "percent"),
            Invalid => write!(f, "invalid token"),
            UnclosedStr => write!(f, "unclosed string literal"),
            Plus => write!(f, "plus"),
            Dash => write!(f, "dash"),
            Asterisk => write!(f, "asterisk"),
            Slash => write!(f, "slash"),
            Caret => write!(f, "caret"),
            Bar => write!(f, "bar"),
            Bang => write!(f, "bang"),
            End => write!(f, "end of input"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub loc: Loc,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.text.is_empty() {
            write!(f, "{}", self.kind)
        } else {
            write!(f, "{} `{}`", self.kind, self.text)
        }
    }
}

pub struct Lexer<Chars: Iterator<Item=char>> {
    chars: Peekable<Chars>,
    peeked: Option<Token>,
    exhausted: bool,
    file_path: Option<String>,
    lnum: usize,
    bol: usize,
    cnum: usize,
}

impl<Chars: Iterator<Item=char>> Lexer<Chars> {
    pub fn new(chars: Chars, file_path: Option<String>) -> Self {
        Self {
            chars: chars.peekable(),
            peeked: None,
            exhausted: false,
            file_path,
            lnum: 0,
            bol: 0,
            cnum: 0,
        }
    }

    pub fn loc(&self) -> Loc {
        Loc {
            file_path: self.file_path.clone(),
            row: self.lnum + 1,
            col: self.cnum - self.bol + 1,
        }
    }

    pub fn expect_token(&mut self, kind: TokenKind) -> Result<Token, Token> {
        let token = self.next_token();
        if kind == token.kind {
            Ok(token)
        } else {
            Err(token)
        }
    }

    pub fn peek_token(&mut self) -> &Token {
        let token = self.next_token();
        self.peeked.insert(token)
    }

    pub fn next_token(&mut self) -> Token {
        self.peeked.take().unwrap_or_else(|| self.chop_tokens_from_chars())
    }

    fn drop_line(&mut self) {
        while self.chars.next_if(|x| *x != '\n').is_some() {
            self.cnum += 1
        }
        if self.chars.next_if(|x| *x == '\n').is_some() {
            self.cnum += 1;
            self.lnum += 1;
            self.bol = self.cnum
        }
    }

    fn trim_whitespaces(&mut self) {
        while self.chars.next_if(|x| x.is_whitespace() && *x != '\n').is_some() {
            self.cnum += 1
        }
    }

    fn chop_tokens_from_chars(&mut self) -> Token {
        assert!(!self.exhausted, "Completely exhausted lexer. The lexer MUST ALWAYS end with the terminators. If the lexer caller tries to pull tokens after the terminators, this is a bug.");

        self.trim_whitespaces();
        while let Some(x) = self.chars.peek() {
            if *x != '\n' && *x != '#' {
                break
            }

            self.drop_line();
            self.trim_whitespaces();
        }

        let loc = self.loc();
        match self.chars.next() {
            Some(x) => {
                self.cnum += 1;
                let mut text = x.to_string();
                match x {
                    '(' => Token {text, loc, kind: TokenKind::OpenParen},
                    ')' => Token {text, loc, kind: TokenKind::CloseParen},
                    ',' => Token {text, loc, kind: TokenKind::Comma},
                    '=' => if self.chars.next_if(|x| *x == '=').is_some() {
                        self.cnum += 1;
                        text.push('=');
                        Token {text, loc, kind: TokenKind::EqualsEquals}
                    } else {
                        Token {text, loc, kind: TokenKind::Equals}
                    },
                    ':' => if self.chars.next_if(|x| *x == ':').is_some() {
                        self.cnum += 1;
                        text.push(':');
                        Token {text, loc, kind: TokenKind::DoubleColon}
                    } else {
                        Token {text, loc, kind: TokenKind::Colon}
                    },
                    '+' => Token {text, loc, kind: TokenKind::Plus},
                    '-' => Token {text, loc, kind: TokenKind::Dash},
                    '*' => Token {text, loc, kind: TokenKind::Asterisk},
                    '/' => Token {text, loc, kind: TokenKind::Slash},
                    '^' => Token {text, loc, kind: TokenKind::Caret},
                    '%' => Token {text, loc, kind: TokenKind::Percent},
                    '{' => Token {text, loc, kind: TokenKind::OpenCurly},
                    '}' => Token {text, loc, kind: TokenKind::CloseCurly},
                    '|' => Token {text, loc, kind: TokenKind::Bar},
                    '!' => Token {text, loc, kind: TokenKind::Bang},
                    '"' => {
                        // TODO: no support for escaped sequences inside of string literals
                        text.clear();
                        while let Some(x) = self.chars.next_if(|x| *x != '"') {
                            text.push(x)
                        }
                        Token {text, loc, kind:
                            if self.chars.next_if(|x| *x == '"').is_some() {
                                TokenKind::Str
                            } else {
                                TokenKind::UnclosedStr
                            }
                        }
                    }
                    _ => {
                        if !is_ident_char(&x) {
                            self.exhausted = true;
                            Token{text, loc, kind: TokenKind::Invalid}
                        } else {
                            while let Some(x) = self.chars.next_if(is_ident_char) {
                                self.cnum += 1;
                                text.push(x)
                            }
                            if let Some(kind) = keyword_by_name(&text) {
                                Token{text, loc, kind}
                            } else {
                                Token{text, loc, kind: TokenKind::Ident}
                            }
                        }
                    }
                }
            }
            None => {
                self.cnum += 1;
                self.exhausted = true;
                Token{text: "".to_string(), loc, kind: TokenKind::End}
            }
        }
    }
}

impl<Chars: Iterator<Item=char>> Iterator for Lexer<Chars> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.exhausted {
            None
        } else {
            Some(self.next_token())
        }
    }
}

fn is_ident_char(x: &char) -> bool {
    let extra_chars = "_.";
    x.is_alphanumeric() || extra_chars.contains(*x)
}
