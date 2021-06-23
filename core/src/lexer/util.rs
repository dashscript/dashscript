use crate::{TokenKind, Lexer, Keyword, TinyString};

impl Lexer {
    
    pub fn escape_char(character: char) -> char {
        match character {
            't' => '\t',
            'n' => '\n',
            'r' => '\r',
            _ => character
        }
    }

    pub fn resolve_kind(word: String) -> TokenKind {
        match word.as_str() {
            "null" => TokenKind::Null,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "return" => TokenKind::Keyword(Keyword::Return),
            "if" => TokenKind::Keyword(Keyword::If),
            "elif" => TokenKind::Keyword(Keyword::Elif),
            "else" => TokenKind::Keyword(Keyword::Else),
            "break" => TokenKind::Keyword(Keyword::Break),
            "while" => TokenKind::Keyword(Keyword::While),
            "for" => TokenKind::Keyword(Keyword::For),
            "continue" => TokenKind::Keyword(Keyword::Continue),
            "try" => TokenKind::Keyword(Keyword::Try),
            "expect" => TokenKind::Keyword(Keyword::Expect),
            _ => TokenKind::Word(TinyString::new(word.as_bytes()))
        }
    }
    
    pub fn resolve_keyword(word: String) -> TokenKind {
        match word.as_str() {
            "return" => TokenKind::Keyword(Keyword::Return),
            "if" => TokenKind::Keyword(Keyword::If),
            "elif" => TokenKind::Keyword(Keyword::Elif),
            "else" => TokenKind::Keyword(Keyword::Else),
            "break" => TokenKind::Keyword(Keyword::Break),
            "while" => TokenKind::Keyword(Keyword::While),
            "continue" => TokenKind::Keyword(Keyword::Continue),
            "let" => TokenKind::Keyword(Keyword::Let),
            "const" => TokenKind::Keyword(Keyword::Const),
            "func" => TokenKind::Keyword(Keyword::Func),
            "async" => TokenKind::Keyword(Keyword::Async),
            "await" => TokenKind::Keyword(Keyword::Await),
            "for" => TokenKind::Keyword(Keyword::For),
            "in" => TokenKind::Keyword(Keyword::In),
            "try" => TokenKind::Keyword(Keyword::Try),
            "expect" => TokenKind::Keyword(Keyword::Expect),
            "import" => TokenKind::Keyword(Keyword::Import),
            "as" => TokenKind::Keyword(Keyword::As),
            "null" => TokenKind::Null,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Word(TinyString::new(word.as_bytes()))
        }
    }

}