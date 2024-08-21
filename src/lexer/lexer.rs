use super::tokens::{Token, TokenValue};

struct Lexer {
    tokens: Vec<Token>,
    buffer: Vec<char>,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer {
            tokens: Vec::new(),
            buffer: Vec::new(),
            line: 1,
            column: 1,
        }
    }
}

pub fn lex(file_path: String) -> Vec<Token> {
    std::fs::read_to_string(file_path)
        .unwrap()
        .as_str()
        .chars()
        .into_iter()
        .fold(Ok(Lexer::new()), |acc, c| {
            acc.map(|state| f_inner(state, c))?
        })
        .expect("Error")
        .tokens
}

fn f_inner(mut state: Lexer, c: char) -> Result<Lexer, String> {
    match state.buffer[..] {
        ['"', ..] => {
            if c == '"' {
                let collected = state.buffer[1..].iter().collect::<String>();
                state.buffer.clear();
                state = build_token(state, |t| Token::String(t), collected);
                Ok(state)
            } else {
                match c {
                    '\n' => state.line += 1,
                    '\t' => state.column += 4,
                    _ => state.column += 1,
                }
                state.buffer.push(c);
                Ok(state)
            }
        }
        ['.'] => match c {
            '.' => {
                state = build_token(state, |t| Token::Range(t), "..".to_string());
                state.buffer.clear();
                Ok(state)
            }
            _ => {
                state = build_token(state, |t| Token::Dot(t), ".".to_string());
                state.buffer.clear();
                match_char(state, c)
            }
        },
        ['='] => match c {
            '=' => {
                state = build_token(state, |t| Token::Equality(t), "==".to_string());
                state.buffer.clear();
                Ok(state)
            }
            _ => {
                state = build_token(state, |t| Token::Assign(t), "=".to_string());
                state.buffer.clear();
                match_char(state, c)
            }
        },
        ['-'] => match c {
            '>' => {
                state = build_token(state, |t| Token::ReturnType(t), "->".to_string());
                state.buffer.clear();
                Ok(state)
            }
            _ => {
                state = build_token(state, |t| Token::Operator(t), "-".to_string());
                state.buffer.clear();
                match_char(state, c)
            }
        },
        ['<'] => match c {
            '=' => {
                state = build_token(state, |t| Token::LTE(t), "<=".to_string());
                state.buffer.clear();
                Ok(state)
            }
            '-' => {
                state = build_token(state, |t| Token::Channel(t), "<-".to_string());
                state.buffer.clear();
                Ok(state)
            }
            _ => {
                state = build_token(state, |t| Token::LT(t), "<".to_string());
                state.buffer.clear();
                match_char(state, c)
            }
        },
        ['*'] => {
            if c.is_whitespace() {
                state = build_token(state, |t| Token::Operator(t), "*".to_string());
                state.buffer.clear();
                Ok(state)
            } else {
                state = build_token(state, |t| Token::Deref(t), "*".to_string());
                match_char(state, c)
            }
        }
        ['>'] => match c {
            '=' => {
                state = build_token(state, |t| Token::GTE(t), ">=".to_string());
                state.buffer.clear();
                Ok(state)
            }

            _ => {
                state = build_token(state, |t| Token::GT(t), ">".to_string());
                state.buffer.clear();
                match_char(state, c)
            }
        },
        ['&'] => {
            if c.is_whitespace() {
                Err("Unexpected whitespace after &".to_string())
            } else {
                match c {
                    '&' => {
                        state = build_token(state, |t| Token::And(t), "&&".to_string());
                        state.buffer.clear();
                        Ok(state)
                    }
                    _ => {
                        state = build_token(state, |t| Token::Pointer(t), "&".to_string());
                        match_char(state, c)
                    }
                }
            }
        }
        ['|'] => match c {
            '|' => {
                state = build_token(state, |t| Token::Or(t), "||".to_string());
                state.buffer.clear();
                Ok(state)
            }
            '>' => {
                state = build_token(state, |t| Token::Pipe(t), "|>".to_string());
                state.buffer.clear();
                Ok(state)
            }
            _ => {
                state = build_token(state, |t| Token::TypeSeparator(t), "|".to_string());
                state.buffer.clear();
                match_char(state, c)
            }
        },
        [':'] => match c {
            ':' => {
                state = build_token(state, |t| Token::Append(t), "::".to_string());
                state.buffer.clear();
                Ok(state)
            }
            _ => {
                state = build_token(state, |t| Token::Colon(t), ":".to_string());
                state.buffer.clear();
                match_char(state, c)
            }
        },
        ['/'] => match c {
            '/' => {
                state = build_token(state, |t| Token::Comment(t), "//".to_string());
                state.buffer.clear();
                Ok(state)
            }
            _ => {
                state = build_token(state, |t| Token::Operator(t), "/".to_string());
                state.buffer.clear();
                match_char(state, c)
            }
        },
        _ => match_char(state, c),
    }
}

fn match_char(mut state: Lexer, c: char) -> Result<Lexer, String> {
    match c {
        '"' => {
            state = parse_buffer(state);
            state.buffer.push(c);
            Ok(state)
        }
        '+' | '%' | '^' | '~' => {
            state = parse_buffer(state);
            state = build_token(state, |t| Token::Operator(t), c.to_string());
            Ok(state)
        }
        ',' => {
            state = parse_buffer(state);
            state = build_token(state, |t| Token::Comma(t), c.to_string());
            Ok(state)
        }
        '(' => {
            state = parse_buffer(state);
            state = build_token(state, |t| Token::LParen(t), c.to_string());
            Ok(state)
        }
        ')' => {
            state = parse_buffer(state);
            state = build_token(state, |t| Token::RParen(t), c.to_string());
            Ok(state)
        }
        '{' => {
            state = parse_buffer(state);
            state = build_token(state, |t| Token::LBrace(t), c.to_string());
            Ok(state)
        }
        '}' => {
            state = parse_buffer(state);
            state = build_token(state, |t| Token::RBrace(t), c.to_string());
            Ok(state)
        }
        '[' => {
            state = parse_buffer(state);
            state = build_token(state, |t| Token::LBracket(t), c.to_string());
            Ok(state)
        }
        ']' => {
            state = parse_buffer(state);
            state = build_token(state, |t| Token::RBracket(t), c.to_string());
            Ok(state)
        }
        _ => Ok(check_whitespace(state, c)),
    }
}

fn check_whitespace(mut state: Lexer, c: char) -> Lexer {
    if c.is_whitespace() {
        state = parse_buffer(state);
        match c {
            '\n' => {
                state.line += 1;
                state.column = 1;
                state = build_token(state, |t| Token::NewLine(t), "\n".to_string());
            }
            '\t' => state.column += 4,
            _ => state.column += 1,
        };
    } else {
        state.buffer.push(c);
    }
    state
}

fn parse_buffer(mut state: Lexer) -> Lexer {
    let collected = state
        .buffer
        .iter()
        .filter(|c| !c.is_whitespace())
        .collect::<String>();
    if !collected.is_empty() {
        state.buffer.clear();
        if check_number(collected.chars().collect::<Vec<char>>()) {
            state = build_token(state, |t| Token::Number(t), collected);
            return state;
        }
        match collected.as_str() {
            "let" => state = build_token(state, |t| Token::Let(t), collected),
            "pub" => {
                state = build_token(state, |t| Token::Pub(t), collected);
            }
            "go" => {
                state = build_token(state, |t| Token::Go(t), collected);
            }
            "type" => {
                state = build_token(state, |t| Token::TypeKeyword(t), collected);
            }
            "mut" => {
                state = build_token(state, |t| Token::Mut(t), collected);
            }
            "import" => {
                state = build_token(state, |t| Token::Import(t), collected);
            }
            "match" => {
                state = build_token(state, |t| Token::Match(t), collected);
            }
            "fn" => {
                state = build_token(state, |t| Token::Function(t), collected);
            }
            "if" => {
                state = build_token(state, |t| Token::If(t), collected);
            }
            "else" => {
                state = build_token(state, |t| Token::Else(t), collected);
            }
            "return" => {
                state = build_token(state, |t| Token::Return(t), collected);
            }
            "true" => {
                state = build_token(state, |t| Token::True(t), collected);
            }
            "false" => {
                state = build_token(state, |t| Token::False(t), collected);
            }
            "enum" => {
                state = build_token(state, |t| Token::EnumKeyword(t), collected);
            }
            "interface" => {
                state = build_token(state, |t| Token::Interface(t), collected);
            }
            _ => {
                state = build_token(state, |t| Token::Identifier(t), collected);
            }
        }
    }
    state
}

fn check_number(l: Vec<char>) -> bool {
    if l.len() == 0 {
        return false;
    }
    if !l[0].is_numeric() {
        return false;
    }
    l.iter().all(|c| {
        c.is_numeric()
            || match c {
                '.' | &'_' => true,
                _ => false,
            }
    })
}

fn build_token<F>(mut state: Lexer, token_constructor: F, value: String) -> Lexer
where
    F: FnOnce(TokenValue) -> Token,
{
    let length = value.len();
    let token_value = generate_token_value(state.line, state.column, value);
    state.column += length;
    state.tokens.push(token_constructor(token_value));
    state
}

fn generate_token_value(line: usize, column: usize, value: String) -> TokenValue {
    return TokenValue {
        value,
        line,
        column,
    };
}
