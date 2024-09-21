use super::tokens::TokenValue;
use logos::Lexer;
use logos::Logos;

fn token_value_callback(lexer: &mut Lexer<Token>) -> TokenValue {
    let slice = lexer.slice();
    let line = lexer.extras.line;
    let column = lexer.extras.column;
    TokenValue {
        value: slice.to_string(),
        line,
        column,
    }
}

#[derive(Debug, Logos, PartialEq)]
#[logos(extras = TokenValue, skip r"[ \t\n\f]+")]
pub enum Token {
    #[regex(r"[0-9]+", token_value_callback)]
    Number(TokenValue),
    #[token("let", token_value_callback)]
    Let(TokenValue),
    #[token("import", token_value_callback)]
    Import(TokenValue),
    #[regex(r#""[^"]*""#, token_value_callback)]
    String(TokenValue),

    #[token("go", token_value_callback)]
    Go(TokenValue),
    #[token("if", token_value_callback)]
    If(TokenValue),
    #[token("else", token_value_callback)]
    Else(TokenValue),
    #[token("true", token_value_callback)]
    True(TokenValue),
    #[token("false", token_value_callback)]
    False(TokenValue),
    #[token("return", token_value_callback)]
    Return(TokenValue),
    #[token("match", token_value_callback)]
    Match(TokenValue),

    #[regex(r"\+|-|\/|%|\^|<<|>>", token_value_callback)]
    Operator(TokenValue),

    #[regex(r"[a-zA-Z_$@][a-zA-Z0-9_$@]*", token_value_callback)]
    Identifier(TokenValue),

    #[token("=", token_value_callback)]
    Assign(TokenValue),
    #[token("{", token_value_callback)]
    LBrace(TokenValue),
    #[token("}", token_value_callback)]
    RBrace(TokenValue),
    #[token("[", token_value_callback)]
    LBracket(TokenValue),
    #[token("]", token_value_callback)]
    RBracket(TokenValue),
    #[token(":", token_value_callback)]
    Colon(TokenValue),
    #[token("(", token_value_callback)]
    LParen(TokenValue),
    #[token(")", token_value_callback)]
    RParen(TokenValue),
    #[token(",", token_value_callback)]
    Comma(TokenValue),
    #[token(".", token_value_callback)]
    Dot(TokenValue),
    #[token("&", token_value_callback)]
    Pointer(TokenValue),
    #[token("*", token_value_callback)]
    Deref(TokenValue),
    #[token("<-", token_value_callback)]
    Channel(TokenValue),
    #[token("::", token_value_callback)]
    Append(TokenValue),
    #[token("type", token_value_callback)]
    TypeKeyword(TokenValue),
    #[token("pub", token_value_callback)]
    Pub(TokenValue),
    #[token("mut", token_value_callback)]
    Mut(TokenValue),
    #[token("fn", token_value_callback)]
    Function(TokenValue),
    #[token("==", token_value_callback)]
    Equality(TokenValue),
    #[token(">", token_value_callback)]
    GT(TokenValue),
    #[token("<", token_value_callback)]
    LT(TokenValue),
    #[token(">=", token_value_callback)]
    GTE(TokenValue),
    #[token("<=", token_value_callback)]
    LTE(TokenValue),
    EOF(TokenValue),
    #[token("&&", token_value_callback)]
    And(TokenValue),
    #[token("||", token_value_callback)]
    Or(TokenValue),
    #[token("|", token_value_callback)]
    Pipe(TokenValue),
    #[token("..", token_value_callback)]
    Range(TokenValue),
    #[regex(r"//.*", token_value_callback)]
    Comment(TokenValue),
    #[token("->", token_value_callback)]
    ReturnType(TokenValue),
    #[token("enum", token_value_callback)]
    EnumKeyword(TokenValue),
    #[token("interface", token_value_callback)]
    Interface(TokenValue),
}

#[derive(Debug)]
pub enum LexerError {
    InvalidToken,
    FileNoteFound,
}

pub fn lex(file_path: &str) -> Result<Vec<Result<Token, ()>>, LexerError> {
    let input = match std::fs::read_to_string(file_path) {
        Ok(i) => i,
        Err(_) => return Err(LexerError::FileNoteFound),
    };
    let lexer = Token::lexer(&input);
    Ok(lexer.collect::<Vec<Result<Token, ()>>>())
}
