use super::parser::TokenType;

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
        "return" | "if" | "elif" | "else" | "break" | "for" | "while" | "continue" => TokenType::Keyword(word),
        _ => TokenType::Word(word)
    }
}

pub fn check_word_for_keywords(word: String) -> TokenType {
    match word.as_str() {
        "var" | "const" | "func" | "return" | "if" | "elif" | "else" | "async" | "await" | "break" | "while" | "for" | "in" | "continue" | "class" | "static" => TokenType::Keyword(word),
        "null" => TokenType::Null,
        "true" => TokenType::Boolean(true),
        "false" => TokenType::Boolean(false),
        _ => TokenType::Word(word)
    }
}