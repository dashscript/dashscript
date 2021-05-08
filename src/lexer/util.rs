use crate::parser::{TokenType, Keyword};

pub fn resolve_escape_char(char: char) -> char {
    match char {
        't' => '\t',
        'n' => '\n',
        'r' => '\r',
        _ => char
    }
}

pub fn get_word_token_type(word: String) -> TokenType {
    match word.as_str() {
        "null" => TokenType::Null,
        "true" => TokenType::Boolean(true),
        "false" => TokenType::Boolean(false),
        "return" => TokenType::Keyword(Keyword::Return),
        "if" => TokenType::Keyword(Keyword::If),
        "elif" => TokenType::Keyword(Keyword::Elif),
        "else" => TokenType::Keyword(Keyword::Else),
        "break" => TokenType::Keyword(Keyword::Break),
        "while" => TokenType::Keyword(Keyword::While),
        "for" => TokenType::Keyword(Keyword::For),
        "continue" => TokenType::Keyword(Keyword::Continue),
        _ => TokenType::Word(word)
    }
}

pub fn check_word_for_keywords(word: String) -> TokenType {
    match word.as_str() {
        "return" => TokenType::Keyword(Keyword::Return),
        "if" => TokenType::Keyword(Keyword::If),
        "elif" => TokenType::Keyword(Keyword::Elif),
        "else" => TokenType::Keyword(Keyword::Else),
        "break" => TokenType::Keyword(Keyword::Break),
        "while" => TokenType::Keyword(Keyword::While),
        "continue" => TokenType::Keyword(Keyword::Continue),
        "var" => TokenType::Keyword(Keyword::Var),
        "const" => TokenType::Keyword(Keyword::Const),
        "func" => TokenType::Keyword(Keyword::Func),
        "async" => TokenType::Keyword(Keyword::Async),
        "await" => TokenType::Keyword(Keyword::Await),
        "for" => TokenType::Keyword(Keyword::For),
        "in" => TokenType::Keyword(Keyword::In),
        "class" => TokenType::Keyword(Keyword::Class),
        "static" => TokenType::Keyword(Keyword::Static),
        "null" => TokenType::Null,
        "true" => TokenType::Boolean(true),
        "false" => TokenType::Boolean(false),
        _ => TokenType::Word(word)
    }
}