use std::fmt::Display;

use super::ast::*;
use crate::lexer::lexer::{Token, TokenValue};
use error::{Error, Kind};
use types::*;

#[allow(dead_code)]
mod error {
    use super::types::State;
    use crate::lexer::lexer::Token;
    // Line, column

    #[derive(Debug)]
    pub struct ErrorState {
        pub line: usize,
        pub column: usize,
        pub message: String,
        pub token: Option<Token>,
        pub length: usize,
    }

    pub enum Kind {
        UnexpectedToken,
        InvalidImport,
        UnexpectedEOF,
        NoDelimiter,
    }

    #[derive(Debug)]
    pub enum Error {
        UnexpectedToken(ErrorState),
        InvalidImport(ErrorState),
        UnexpectedEOF(ErrorState),
        NoDelimiter(ErrorState),
    }

    impl Error {
        pub fn new(error_kind: Kind, message: &str, state: &State, token: Option<Token>) -> Self {
            println!("Parser error");
            println!("{:?}", &state.tokens[..5]);
            match error_kind {
                Kind::UnexpectedToken => Self::unexpected_token(message, state, token),
                Kind::InvalidImport => Self::invalid_import(message, state, token),
                Kind::UnexpectedEOF => Self::unexpected_eof(message, state, token),
                Kind::NoDelimiter => Self::no_delimiter(message, state, token),
            }
        }
        fn build_state(message: &str, state: &State, token: Option<Token>) -> ErrorState {
            ErrorState {
                line: state.line,
                column: state.column,
                message: message.to_string(),
                token,
                length: state.tokens.len(),
            }
        }
        fn unexpected_token(message: &str, state: &State, token: Option<Token>) -> Self {
            Self::UnexpectedToken(Self::build_state(message, state, token))
        }

        fn unexpected_eof(message: &str, state: &State, token: Option<Token>) -> Self {
            Self::UnexpectedEOF(Self::build_state(message, state, token))
        }
        fn invalid_import(message: &str, state: &State, token: Option<Token>) -> Self {
            Self::InvalidImport(Self::build_state(message, state, token))
        }
        fn no_delimiter(message: &str, state: &State, token: Option<Token>) -> Self {
            Self::NoDelimiter(Self::build_state(message, state, token))
        }
    }
}

#[allow(dead_code)]
mod types {

    use super::Error;
    use crate::lexer::lexer::{Token, TokenValue};
    #[derive(Debug)]
    pub struct State<'a> {
        pub line: usize,
        pub column: usize,
        pub tokens: &'a [Token],
    }

    impl<'a> State<'a> {
        pub fn update_position(&'a self, token: &'a TokenValue) -> State<'a> {
            State {
                line: token.line,
                column: token.column,
                ..*self
            }
        }

        pub fn update(self, rest: &'a [Token], value: Option<&'a TokenValue>) -> State<'a> {
            State {
                tokens: rest,
                ..(match value {
                    Some(v) => self.update_position(v),
                    None => self,
                })
            }
        }
    }

    pub type ParserReturn<'a, T> = Result<(T, State<'a>), Error>;

    pub enum Delimiter {
        Func(Box<dyn Fn(&[Token]) -> bool>),
        None,
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct ErrorReturn {
    line: usize,
    column: usize,
    message: String,
    token: Option<Token>,
    tokens: Vec<Token>,
    position: usize,
}

impl Display for ErrorReturn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

fn eof_error(state: &State) -> Error {
    Error::new(Kind::UnexpectedEOF, "Unexpected EOF", state, None)
}

fn debug_print(msg: &str, state: &State) {
    println!("{}", msg);
    println!("{:?}", &state.tokens[..5]);
}

pub fn parse(tokens: Vec<Token>) -> Result<ASTNode, ErrorReturn> {
    let state = State {
        line: 1,
        column: 1,
        tokens: &tokens[..],
    };
    state.tokens.iter().for_each(|x| println!("{:?}", x));
    match parse_top_level(state) {
        Ok((vec, _)) => Ok(ASTNode::Root(vec)),
        Err(e) => match e {
            Error::UnexpectedToken(x)
            | Error::UnexpectedEOF(x)
            | Error::NoDelimiter(x)
            | Error::InvalidImport(x) => Err(ErrorReturn {
                message: x.message,
                line: x.line,
                column: x.column,
                token: x.token,
                position: tokens.len() - x.length,
                tokens,
            }),
        },
    }
}

fn parse_top_level<'a>(mut state: State<'a>) -> Result<(Vec<ASTNode>, State<'a>), Error> {
    let mut vec = Vec::<ASTNode>::new();
    let mut is_pub = false;
    loop {
        if state.tokens.is_empty() {
            break;
        }
        match state.tokens {
            [] => break,
            [Token::NewLine, rest @ ..] => {
                state = state.update(rest, None);
                continue;
            }
            [x @ Token::Import(_), rest @ ..] => {
                if is_pub {
                    return Err(Error::new(
                        Kind::UnexpectedToken,
                        "Cannot have public import",
                        &state,
                        Some(x.clone()),
                    ));
                }
                let (node, new_state) = parse_import(state.update(rest, None))?;
                state = new_state;
                vec.push(node);
                continue;
            }
            [tok @ Token::Pub(x), rest @ ..] => {
                if is_pub {
                    return Err(Error::new(
                        Kind::UnexpectedToken,
                        "Cannot have multiple public statements",
                        &state,
                        Some(tok.clone()),
                    ));
                }
                is_pub = true;
                state = state.update(rest, Some(x));
            }
            [Token::Function(_), Token::LParen(x), rest @ ..] => {
                let new_state = state.update(rest, Some(x));
                let ((name, function), new_state) = parse_struct_method_definition(new_state)?;
                state = new_state;
                vec.push(ASTNode::TopLevel(
                    is_pub,
                    TopLevel::StructMethodDefinition(name, function),
                ));
                is_pub = false;
            }
            [Token::Function(x), Token::Identifier(name), rest @ ..] => {
                let name = take_value(name);
                let (node, new_state) = parse_function(state.update(rest, Some(x)), Some(name))?;
                state = new_state;
                vec.push(ASTNode::TopLevel(
                    is_pub,
                    TopLevel::FunctionDefinition(node),
                ));
                is_pub = false;
            }
            [Token::TypeKeyword(x), Token::Identifier(tok), Token::Assign(_), rest @ ..] => {
                let new_state = state.update(rest, Some(x));
                let (node, new_state) = parse_type_definition(take_value(tok), new_state)?;
                state = new_state;
                vec.push(ASTNode::TopLevel(is_pub, TopLevel::TopLevelTypeDef(node)));
                is_pub = false;
            }
            [Token::EnumKeyword(_), Token::Identifier(name), Token::LBrace(_), rest @ ..] => {
                let (node, new_state) = parse_enum(state.update(rest, Some(name)))?;
                let n = take_value(name);
                state = new_state;
                vec.push(ASTNode::TopLevel(
                    is_pub,
                    TopLevel::TopLevelTypeDef(TypeDef::EnumDefinition(n, node)),
                ));
                is_pub = false;
            }
            _ => {
                return Err(Error::new(
                    Kind::UnexpectedToken,
                    "Invalid top level statement",
                    &state,
                    Some(state.tokens[0].clone()),
                ));
            }
        }
    }
    Ok((vec, state))
}

fn internal_parse<'a>(
    mut state: State<'a>,
    delim: Delimiter,
) -> Result<(Vec<ASTNode>, State<'a>), Error> {
    let mut vec = Vec::<ASTNode>::new();
    loop {
        if state.tokens.is_empty() {
            break;
        }
        match delim {
            Delimiter::Func(ref f) => {
                if f(state.tokens) {
                    let tokens = &state.tokens[1..];
                    return Ok((vec, state.update(tokens, None)));
                }
            }
            _ => (),
        }

        match parse_expression(state) {
            Ok((new_node, new_state)) => {
                state = new_state;
                vec.push(new_node);
            }
            Err(e) => return Err(e),
        }
    }
    Ok((vec, state))
}

fn take_value(token: &TokenValue) -> ASTString {
    std::mem::take(&mut (*token.value).to_string())
}

fn parse_statement<'a>(state: State<'a>) -> ParserReturn<ASTNode> {
    match state.tokens {
        [Token::Let(x), rest @ ..] => parse_let_statement(state.update(rest, Some(x))),
        _ => parse_expression(state),
    }
}

fn parse_let_statement<'a>(state: State<'a>) -> ParserReturn<ASTNode> {
    let (left, new_state) = parse_left_let_statement(state)?;
    match new_state.tokens {
        [Token::Assign(x), rest @ ..] => {
            let (right, new_state) = parse_expression(new_state.update(rest, Some(x)))?;
            Ok((
                ASTNode::LetExpression(LetExpression {
                    identifier: left,
                    value: Box::new(right),
                }),
                new_state,
            ))
        }
        [x, ..] => Err(Error::new(
            Kind::UnexpectedToken,
            "Invalid let statement",
            &new_state,
            Some(x.clone()),
        )),
        [] => {
            return Err(Error::new(
                Kind::UnexpectedEOF,
                "Unexpected EOF",
                &new_state,
                None,
            ))
        }
    }
}

fn parse_left_let_statement<'a>(state: State<'a>) -> ParserReturn<IdentifierType> {
    let (ident, state) = parse_identifier(state)?;
    Ok((ident, state))

    // match state.tokens {
    //     [Token::Identifier(n), rest @ ..] => {
    //         let new_state = state.update(rest, Some(n));
    //         ident = Some(IdentifierType::Identifier(
    //             Identifier {
    //                 value: take_value(n),
    //                 mutable: is_mut,
    //             },
    //             None,
    //         ));
    //         state = new_state;
    //     }
    //     [Token::LParen(x), rest @ ..] => {
    //         state = state.update(rest, Some(x));
    //         let mut idents: Vec<Identifier> = vec![];
    //         loop {
    //             match state.tokens {
    //                 [Token::RParen(x), rest @ ..] => {
    //                     state = state.update(rest, Some(x));
    //                     ident = Some(IdentifierType::TupleDestructure(idents, None));
    //                     break;
    //                 }
    //                 [Token::Comma(x), rest @ ..] => {
    //                     state = state.update(rest, Some(x));
    //                     continue;
    //                 }
    //                 [Token::Identifier(n), rest @ ..] => {
    //                     let new_state = state.update(rest, Some(n));
    //                     let ident = Identifier {
    //                         value: take_value(n),
    //                         mutable: is_mut,
    //                     };
    //                     idents.push(ident);
    //                     state = new_state;
    //                 }
    //                 _ => {
    //                     return Err(Error::new(
    //                         Kind::UnexpectedToken,
    //                         "Invalid let statement",
    //                         &state,
    //                     ))
    //                 }
    //             }
    //         }
    //     }
    //     [Token::LBrace(x), rest @ ..] => {
    //         state = state.update(rest, Some(x));
    //         let mut idents: Vec<Identifier> = vec![];
    //         loop {
    //             match state.tokens {
    //                 [Token::RBrace(x), rest @ ..] => {
    //                     state = state.update(rest, Some(x));
    //                     ident = Some(IdentifierType::RecordDestructure((idents, None)));
    //                     break;
    //                 }
    //                 [Token::Comma(x), rest @ ..] => {
    //                     state = state.update(rest, Some(x));
    //                     continue;
    //                 }
    //                 [Token::Identifier(n), rest @ ..] => {
    //                     let new_state = state.update(rest, Some(n));
    //                     let ident = Identifier {
    //                         value: take_value(n),
    //                         mutable: is_mut,
    //                     };
    //                     idents.push(ident);
    //                     state = new_state;
    //                 }
    //                 _ => {
    //                     return Err(Error::new(
    //                         Kind::UnexpectedToken,
    //                         "Invalid let statement",
    //                         &state,
    //                     ))
    //                 }
    //             }
    //         }
    //     }
    //     [Token::LBracket(x), rest @ ..] => {
    //         state = state.update(rest, Some(x));
    //         let mut idents: Vec<Identifier> = vec![];
    //         loop {
    //             match state.tokens {
    //                 [Token::RBracket(x), rest @ ..] => {
    //                     state = state.update(rest, Some(x));
    //                     ident = Some(IdentifierType::ArrayDestructure((idents, None)));
    //                     break;
    //                 }
    //                 [Token::Comma(x), rest @ ..] => {
    //                     state = state.update(rest, Some(x));
    //                     continue;
    //                 }
    //                 [Token::Identifier(n), rest @ ..] => {
    //                     let new_state = state.update(rest, Some(n));
    //                     let ident = Identifier {
    //                         value: take_value(n),
    //                         mutable: is_mut,
    //                     };
    //                     idents.push(ident);
    //                     state = new_state;
    //                 }
    //                 _ => {
    //                     return Err(Error::new(
    //                         Kind::UnexpectedToken,
    //                         "Invalid let statement",
    //                         &state,
    //                     ))
    //                 }
    //             }
    //         }
    //     }
    //     _ => {
    //         return Err(Error::new(
    //             Kind::UnexpectedToken,
    //             "Invalid let statement",
    //             &state,
    //         ))
    //     }
    // };
    //
    // loop {
    //     match state.tokens {
    //         [Token::Colon(x), rest @ ..] => {
    //             let (t, new_state) = parse_type_literal(state.update(rest, Some(x)))?;
    //             state = new_state;
    //             ident = match ident {
    //                 Some(IdentifierType::Identifier((i, _))) => {
    //                     Some(IdentifierType::Identifier((i, Some(t))))
    //                 }
    //                 _ => {
    //                     return Err(Error::new(
    //                         Kind::UnexpectedToken,
    //                         "Invalid let statement",
    //                         &state,
    //                     ))
    //                 }
    //             };
    //         }
    //         [Token::Assign(x), rest @ ..] => {
    //             return Ok((ident.unwrap(), state.update(rest, Some(x))));
    //         }
    //         _ => {
    //             return Err(Error::new(
    //                 Kind::UnexpectedToken,
    //                 "Invalid let statement",
    //                 &state,
    //             ))
    //         }
    //     }
    // }
}

fn parse_expression(state: State) -> ParserReturn<ASTNode> {
    let (node, new_state) = match &state.tokens {
        [Token::String(s), rest @ ..] => (
            ASTNode::StringLiteral(take_value(s)),
            state.update(rest, Some(s)),
        ),
        [Token::Number(s), rest @ ..] => {
            let new_state = state.update(rest, Some(s));
            (ASTNode::NumberLiteral(take_value(s)), new_state)
        }
        [Token::LParen(x), rest @ ..] => parse_paren_expression(state.update(rest, Some(x)))?,
        [Token::LBrace(x), rest @ ..] => parse_brace_expression(state.update(rest, Some(x)))?,
        [Token::LBracket(x), rest @ ..] => parse_array_literal(state.update(rest, Some(x)))?,
        [Token::Function(_), Token::LParen(x), rest @ ..] => {
            let new_state = state.update(rest, Some(x));
            parse_function(new_state, None).map(|(f, s)| (ASTNode::FunctionDefinition(f), s))?
        }
        [x, ..] => Err(Error::new(
            Kind::UnexpectedToken,
            "Invalid expression",
            &state,
            Some(x.clone()),
        ))?,
        [] => Err(Error::new(
            Kind::UnexpectedEOF,
            "Unexpected EOF",
            &state,
            None,
        ))?,
    };

    match &new_state.tokens {
        [Token::Pipe(_), rest @ ..] => Ok((node, new_state.update(rest, None))),
        [Token::Assign(_), rest @ ..] => Ok((node, new_state.update(rest, None))),
        [Token::NewLine, rest @ ..] => Ok((node, new_state.update(rest, None))),
        [x, ..] => Err(Error::new(
            Kind::UnexpectedToken,
            "Invalid expression",
            &new_state,
            Some(x.clone()),
        )),
        [] => Err(Error::new(
            Kind::UnexpectedEOF,
            "Unexpected EOF",
            &new_state,
            None,
        )),
    }
}

fn parse_paren_expression(state: State) -> ParserReturn<ASTNode> {
    let delim = Box::new(|tokens: &[Token]| match tokens {
        [Token::RParen(_), ..] => true,
        _ => false,
    });
    match internal_parse(state, Delimiter::Func(delim)) {
        Ok((mut nodes, state)) => {
            let first = std::mem::take(&mut nodes[0]);
            let new_node = ASTNode::ParenExpression(Some(Box::new(first)));
            return Ok((new_node, state));
        }
        Err(e) => Err(e),
    }
}

fn parse_brace_expression(state: State) -> ParserReturn<ASTNode> {
    let delim = Box::new(|tokens: &[Token]| match tokens {
        [Token::RBrace(_), ..] => true,
        _ => false,
    });
    match internal_parse(state, Delimiter::Func(delim)) {
        Ok((nodes, state)) => {
            let new_node = ASTNode::LogicBlock(nodes);
            return Ok((new_node, state));
        }
        Err(e) => Err(e),
    }
}

fn parse_array_literal(state: State) -> ParserReturn<ASTNode> {
    let delim = Box::new(|tokens: &[Token]| match tokens {
        [Token::RBracket(_), ..] => true,
        _ => false,
    });
    match internal_parse(state, Delimiter::Func(delim)) {
        Ok((nodes, state)) => {
            let new_node = ASTNode::ArrayLiteral(nodes);
            return Ok((new_node, state));
        }
        Err(e) => Err(e),
    }
}

fn parse_import(state: State) -> ParserReturn<ASTNode> {
    match state.tokens {
        [Token::String(s), rest @ ..] => Ok((
            ASTNode::GoImport(GoImport {
                module: s.value.clone(),
                alias: None,
            }),
            state.update(rest, Some(s)),
        )),
        [Token::Identifier(t), Token::String(s), rest @ ..] => Ok((
            ASTNode::GoImport(GoImport {
                module: s.value.clone(),
                alias: Some(t.value.clone()),
            }),
            state.update(rest, Some(s)),
        )),
        [Token::Identifier(t), rest @ ..] => Ok((
            ASTNode::GoferImport(GoferImport {
                module: t.value.clone(),
            }),
            state.update(rest, Some(t)),
        )),
        [x, ..] => Err(Error::new(
            Kind::InvalidImport,
            "Invalid import statement",
            &state,
            Some(x.clone()),
        )),
        [] => Err(Error::new(
            Kind::UnexpectedEOF,
            "Unexpected EOF",
            &state,
            None,
        )),
    }
}

fn parse_function(mut state: State, name: Option<String>) -> ParserReturn<FunctionDefinition> {
    // let match_return_type = |state: State| match &state.tokens {
    //     [Token::ReturnType(_), tail @ ..] => Some(parse_type_literal(state.update(tail, None))),
    //     _ => None,
    // };
    debug_print("Parsing function", &state);
    match state.tokens {
        [Token::LParen(x), rest @ ..] => {
            state = state.update(rest, Some(x));
        }
        [x, ..] => {
            return Err(Error::new(
                Kind::UnexpectedToken,
                "Invalid function syntax",
                &state,
                Some(x.clone()),
            ));
        }
        [] => Err(eof_error(&state))?,
    }
    debug_print("Parsing function 2", &state);
    let (args, new_state) = parse_function_args(state)?;
    state = new_state;

    // Check for a return value
    let explicit_return_type = match state.tokens {
        [Token::ReturnType(x), rest @ ..] => {
            let (type_, new_state) = parse_type_literal(state.update(rest, Some(x)))?;
            state = new_state;
            Some(type_)
        }
        _ => None,
    };

    match state.tokens {
        [Token::LBrace(x), rest @ ..] => {
            state = state.update(rest, Some(x));
            let (logic_block, new_state) = internal_parse(
                state,
                Delimiter::Func(Box::new(|tokens| match tokens {
                    [Token::RBrace(_), ..] => true,
                    _ => false,
                })),
            )?;
            state = new_state;
            Ok((
                FunctionDefinition {
                    name,
                    arguments: args,
                    return_type: explicit_return_type,
                    body: logic_block,
                    pointer: None,
                },
                state,
            ))
        }
        [x, ..] => {
            return Err(Error::new(
                Kind::UnexpectedToken,
                "Invalid function syntax",
                &state,
                Some(x.clone()),
            ));
        }
        _ => Err(eof_error(&state))?,
    }
}

fn parse_function_args<'a>(
    mut state: State<'a>,
) -> Result<(Vec<IdentifierType>, State<'a>), Error> {
    let mut previous_was_comma = false;
    let mut args = vec![];
    loop {
        match (state.tokens, (previous_was_comma && args.len() < 1)) {
            ([], _) => {
                return Err(eof_error(&state));
            }
            ([Token::Comma(x), rest @ ..], false) => {
                previous_was_comma = true;
                state = state.update(rest, Some(x));
            }
            ([Token::RParen(x), rest @ ..], false) => {
                debug_print("This is the final function arg", &state);
                return Ok((args, state.update(rest, Some(x))));
            }
            ([tok @ Token::RParen(x), rest @ ..], true) => {
                state = state.update(rest, Some(x));
                return Err(Error::new(
                    Kind::UnexpectedToken,
                    "Invalid function args",
                    &state,
                    Some(tok.clone()),
                ));
            }
            (rest @ _, _) => {
                println!("parsing function args, {:?}", &rest[..5]);
                let (i, new_state) = parse_identifier(state)?;
                state = new_state;
                args.push(i);
            }
        }
    }
}

fn parse_struct_method_definition(state: State) -> ParserReturn<(String, FunctionDefinition)> {
    match state.tokens {
        [Token::Identifier(t), rest @ ..] => {
            let (type_, new_state) = parse_type_literal(state)?;
            let new_state = match new_state.tokens {
                [Token::RParen(x), rest @ ..] => new_state.update(rest, Some(x)),
                _ => new_state,
            };
            todo!("parse_struct_method_definition")
        }
        [x, ..] => Err(Error::new(
            Kind::UnexpectedToken,
            "Invalid struct method definition",
            &state,
            Some(x.clone()),
        )),
        [] => Err(eof_error(&state))?,
    }
}

fn recurse_identifier(mut state: State, delim: Delimiter) -> ParserReturn<Vec<IdentifierType>> {
    let func = match delim {
        Delimiter::None => {
            return Err(Error::new(
                Kind::NoDelimiter,
                "No delimiter provided to recursive identifier function",
                &state,
                None,
            ));
        }
        Delimiter::Func(f) => f,
    };
    let mut idents = vec![];
    let mut last_was_comma = false;
    loop {
        match (state.tokens, last_was_comma) {
            ([], _) => {
                return Err(eof_error(&state));
            }
            (toks @ [_, rest @ ..], _) if func(toks) => {
                state = state.update(rest, None);
                return Ok((idents, state));
            }
            ([Token::Comma(x), rest @ ..], false) => {
                state = state.update(rest, Some(x));
                last_was_comma = true;
            }
            ([tok @ Token::Comma(_), ..], true) => {
                return Err(Error::new(
                    Kind::UnexpectedToken,
                    "Invalid identifier",
                    &state,
                    Some(tok.clone()),
                ));
            }
            _ => {
                let (i, new_state) = parse_identifier(state)?;
                state = new_state;
                idents.push(i);
                last_was_comma = false;
            }
        }
    }
}

fn parse_identifier(mut state: State) -> ParserReturn<IdentifierType> {
    let mutable = match state.tokens {
        [Token::Mut(x), rest @ ..] => {
            state = state.update(rest, Some(x));
            true
        }
        _ => false,
    };
    let ident = match state.tokens {
        [] => return Err(eof_error(&state)),
        [Token::Identifier(name), rest @ ..] => {
            println!("Inside regular identifier, {:?}", &state.tokens[..5]);
            state = state.update(rest, Some(name));
            IdentifierType::Identifier(
                Identifier {
                    value: take_value(name),
                    mutable,
                },
                None,
            )
        }
        [Token::LBracket(x), rest @ ..] => {
            let (i, new_state) = recurse_identifier(
                state.update(rest, Some(x)),
                Delimiter::Func(Box::new(|tokens| match tokens {
                    [Token::RBracket(_), ..] => true,
                    _ => false,
                })),
            )?;
            state = new_state;
            IdentifierType::ArrayDestructure(i, None)
        }
        [Token::LBrace(x), rest @ ..] => {
            let (i, new_state) = recurse_identifier(
                state.update(rest, Some(x)),
                Delimiter::Func(Box::new(|tokens| match tokens {
                    [Token::RBrace(_), ..] => true,
                    _ => false,
                })),
            )?;
            state = new_state;
            IdentifierType::RecordDestructure(i, None)
        }
        [Token::LParen(x), rest @ ..] => {
            let (i, new_state) = recurse_identifier(
                state.update(rest, Some(x)),
                Delimiter::Func(Box::new(|tokens| match tokens {
                    [Token::RParen(_), ..] => true,
                    _ => false,
                })),
            )?;
            state = new_state;
            IdentifierType::TupleDestructure(i, None)
        }
        [x, ..] => {
            return Err(Error::new(
                Kind::UnexpectedToken,
                "Invalid identifier",
                &state,
                Some(x.clone()),
            ));
        }
    };
    let type_ = match state.tokens {
        [Token::Colon(x), rest @ ..] => {
            let (t, new_state) = parse_type_literal(state.update(rest, Some(x)))?;
            state = new_state;
            Some(t)
        }
        _ => None,
    };

    match ident {
        IdentifierType::Identifier(i, _) => Ok((IdentifierType::Identifier(i, type_), state)),
        IdentifierType::ArrayDestructure(i, _) => {
            Ok((IdentifierType::ArrayDestructure(i, type_), state))
        }
        IdentifierType::RecordDestructure(i, _) => {
            Ok((IdentifierType::RecordDestructure(i, type_), state))
        }
        IdentifierType::TupleDestructure(i, _) => {
            Ok((IdentifierType::TupleDestructure(i, type_), state))
        }
    }
}

fn parse_type_literal(state: State) -> Result<(Type, State), Error> {
    let (is_slice, state) = match state.tokens {
        [Token::LBracket(_), Token::RBracket(_), rest @ ..] => (true, state.update(rest, None)),
        _ => (false, state),
    };

    let (is_pointer, state) = match state.tokens {
        [Token::Deref(_), rest @ ..] => (true, state.update(rest, None)),
        _ => (false, state),
    };

    match state.tokens {
        [Token::Identifier(t), Token::Dot(_), Token::Identifier(t2), rest @ ..] => {
            let type_ = Type {
                module: Some(t.value.clone()),
                name: t2.value.clone(),
                pointer: is_pointer,
                slice: is_slice,
            };
            Ok((type_, state.update(rest, Some(t2))))
        }
        [Token::Identifier(t), rest @ ..] => {
            let type_ = Type {
                module: None,
                name: t.value.clone(),
                pointer: is_pointer,
                slice: is_slice,
            };
            Ok((type_, state.update(rest, Some(t))))
        }
        [] => Err(eof_error(&state)),
        [x, ..] => Err(Error::new(
            Kind::UnexpectedToken,
            "Invalid type literal",
            &state,
            Some(x.clone()),
        )),
    }
}

fn parse_type_definition(name: ASTString, state: State) -> ParserReturn<TypeDef> {
    match state.tokens {
        [Token::LBrace(x), rest @ ..] => {
            let (t, new_state) = parse_record_type(state.update(rest, Some(x)))?;
            Ok((TypeDef::RecordDefinition(name, t), new_state))
        }
        _ => {
            let (type_, new_state) = parse_type_literal(state)?;
            Ok((TypeDef::Type(name.clone(), type_), new_state))
        }
    }
}

fn parse_record_type(mut state: State) -> ParserReturn<RecordDefinition> {
    let mut fields: Vec<RecordDefinitionField> = vec![];
    let mut check_for_comma = false;
    loop {
        // println!("{:?}", fields);
        // println!("{:?}", &state.tokens[..5]);
        match (check_for_comma, state.tokens) {
            (_, [Token::RBrace(x), rest @ ..]) => {
                state = state.update(rest, Some(x));
                let record = RecordDefinition { fields };
                return Ok((record, state));
            }
            (false, [Token::Identifier(name), Token::Colon(_), rest @ ..]) => {
                let (type_, new_state) = parse_type_literal(state.update(rest, Some(name)))?;
                let n = take_value(name);
                let field = RecordDefinitionField {
                    name: n.clone(),
                    type_: TypeDef::Type(n, type_),
                    is_pub: false,
                };
                fields.push(field);
                state = new_state;
                check_for_comma = true;
            }
            (false, [Token::Pub(_), Token::Identifier(name), Token::Colon(_), rest @ ..]) => {
                let (type_, new_state) = parse_type_literal(state.update(rest, Some(name)))?;
                let n = take_value(name);
                let field = RecordDefinitionField {
                    name: n.clone(),
                    type_: TypeDef::Type(n, type_),
                    is_pub: true,
                };
                fields.push(field);
                state = new_state;
                check_for_comma = true;
            }
            (true, [Token::Comma(x), rest @ ..]) => {
                state = state.update(rest, Some(x));
                check_for_comma = false;
            }
            (_, []) => return Err(eof_error(&state)),
            (_, [x, ..]) => {
                // println!("\n\n\n");
                // println!("{:?}", &state.tokens[..5]);
                return Err(Error::new(
                    Kind::UnexpectedToken,
                    "Invalid record definition",
                    &state,
                    Some(x.clone()),
                ));
            }
        }
    }
}

fn parse_enum(mut state: State) -> ParserReturn<EnumDefiniton> {
    let mut fields: Vec<(ASTString, Option<Type>)> = vec![];
    let mut comma_check = false;
    loop {
        match (comma_check || fields.len() < 1, state.tokens) {
            (_, [Token::RBrace(x), rest @ ..]) => {
                state = state.update(rest, Some(x));
                return Ok((EnumDefiniton { fields }, state));
            }
            (false, [Token::Identifier(name), Token::LParen(_), rest @ ..]) => {
                let (type_, new_state) = parse_type_literal(state.update(rest, Some(name)))?;
                fields.push((take_value(name), Some(type_)));
                state = new_state;
                comma_check = true;
            }
            (false, [Token::Identifier(name), Token::Comma(x), rest @ ..]) => {
                fields.push((take_value(name), None));
                state = state.update(rest, Some(x));
            }
            (true, [Token::Comma(x), rest @ ..]) => {
                comma_check = false;
            }
            (_, []) => return Err(eof_error(&state)),
            (_, [x, ..]) => {
                return Err(Error::new(
                    Kind::UnexpectedToken,
                    "Invalid enum definition",
                    &state,
                    Some(x.clone()),
                ));
            }
        }
    }
}
