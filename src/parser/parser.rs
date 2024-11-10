use super::ast::*;
use crate::lexer::lexer::{Token, TokenKind};
use error::{Error, Kind};
use types::*;

#[allow(dead_code)]
mod error {
    use super::types::State;

    #[derive(Debug)]
    pub struct ErrorState {
        pub message: String,
        pub state: State,
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
        pub fn new(error_kind: Kind, message: &str, state: State) -> Self {
            println!("Parser error");
            println!("{:?}", &state.tokens[..5]);
            match error_kind {
                Kind::UnexpectedToken => Self::unexpected_token(message, state),
                Kind::InvalidImport => Self::invalid_import(message, state),
                Kind::UnexpectedEOF => Self::unexpected_eof(message, state),
                Kind::NoDelimiter => Self::no_delimiter(message, state),
            }
        }
        fn build_state(message: &str, state: State) -> ErrorState {
            ErrorState {
                message: message.to_string(),
                length: state.tokens.len(),
                state,
            }
        }
        fn unexpected_token(message: &str, state: State) -> Self {
            Self::UnexpectedToken(Self::build_state(message, state))
        }

        fn unexpected_eof(message: &str, state: State) -> Self {
            Self::UnexpectedEOF(Self::build_state(message, state))
        }
        fn invalid_import(message: &str, state: State) -> Self {
            Self::InvalidImport(Self::build_state(message, state))
        }
        fn no_delimiter(message: &str, state: State) -> Self {
            Self::NoDelimiter(Self::build_state(message, state))
        }
    }
}

#[allow(dead_code)]
mod types {
    use super::Error;
    use crate::lexer::lexer::{Token, TokenKind};
    #[derive(Debug)]
    pub struct State {
        pub tokens: Vec<TokenKind>,
        pub contexts: Vec<Token>,
        pub position: usize,
    }

    impl State {
        pub fn update(mut self, len: usize) -> Self {
            self.position = self.tokens.len() - len;
            self
        }
    }

    pub type ParserReturn<T> = Result<(T, State), Error>;

    pub enum Delimiter {
        Func(Box<dyn Fn(&[TokenKind]) -> bool>),
        None,
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct ErrorReturn {
    message: String,
    position: usize,
    tokens: Vec<Token>,
}

impl ErrorReturn {
    pub fn new(error: Error) -> Self {
        match error {
            Error::UnexpectedToken(x)
            | Error::UnexpectedEOF(x)
            | Error::NoDelimiter(x)
            | Error::InvalidImport(x) => Self {
                message: x.message,
                position: x.state.position,
                tokens: x.state.contexts,
            },
        }
    }
}

fn eof_error(state: State) -> Error {
    Error::new(Kind::UnexpectedEOF, "Unexpected EOF", state)
}

fn debug_print(msg: &str, state: &State) {
    println!("{}", msg);
    println!("{:?}", &state.tokens[..5]);
}

pub fn parse(tokens: Vec<Token>) -> Result<ASTNode, ErrorReturn> {
    let kinds = tokens.iter().map(|x| x.kind.clone()).collect();
    let state = State {
        tokens: kinds,
        contexts: tokens,
        position: 0,
    };
    state.tokens.iter().for_each(|x| println!("{:?}", x));
    match parse_top_level(state) {
        Ok((vec, _)) => Ok(ASTNode::Root(vec)),
        Err(e) => Err(ErrorReturn::new(e)),
    }
}

fn parse_top_level(mut state: State) -> Result<(Vec<ASTNode>, State), Error> {
    let mut vec = Vec::<ASTNode>::new();
    let mut is_pub = false;
    loop {
        if state.tokens.is_empty() {
            break;
        }
        match &state.tokens[state.position..] {
            [] => break,
            [x @ TokenKind::Import, rest @ ..] => {
                if is_pub {
                    return Err(Error::new(
                        Kind::UnexpectedToken,
                        "Cannot have public import",
                        state,
                    ));
                }
                let (node, new_state) = parse_import(state.update(rest.len()))?;
                state = new_state;
                vec.push(node);
                continue;
            }
            [TokenKind::Let, rest @ ..] => {
                let new_state = state.update(rest.len());
                let (node, new_state) = parse_let_statement(new_state)?;
                vec.push(node);
                state = new_state;
            }
            [TokenKind::TypeKeyword, TokenKind::Identifier(tok), TokenKind::Assign, rest @ ..] => {
                let new_state = state.update(rest.len());
                let (node, new_state) = parse_type_definition(*tok, new_state)?;
                state = new_state;
                vec.push(ASTNode::TopLevel(is_pub, TopLevel::TopLevelTypeDef(node)));
                is_pub = false;
            }
            _ => {
                return Err(Error::new(
                    Kind::UnexpectedToken,
                    "Invalid top level statement",
                    state,
                ));
            }
        }
    }
    Ok((vec, state))
}

fn internal_parse(mut state: State, delim: Delimiter) -> Result<(Vec<ASTNode>, State), Error> {
    let mut vec = Vec::<ASTNode>::new();
    loop {
        if state.tokens.is_empty() {
            break;
        }
        match delim {
            Delimiter::Func(ref f) => {
                if f(&state.tokens[state.position..]) {
                    let position = state.position + 1;
                    let new_pos = state.tokens[position..].len();
                    let new_state = state.update(new_pos);
                    return Ok((vec, new_state));
                }
            }
            _ => (),
        }
        match &state.tokens[state.position..] {
            [TokenKind::NewLine, rest @ ..] => {
                let new_pos = rest.len();
                state = state.update(new_pos);
                continue;
            }
            _ => (),
        }

        match parse_statement(state) {
            Ok((new_node, new_state)) => {
                state = new_state;
                vec.push(new_node);
            }
            Err(e) => return Err(e),
        }
    }
    Ok((vec, state))
}

fn parse_statement<'a>(state: State) -> ParserReturn<ASTNode> {
    match &state.tokens[state.position..] {
        [TokenKind::Let, rest @ ..] => {
            let position = rest.len();
            let new_state = state.update(position);
            parse_let_statement(new_state)
        }
        _ => parse_expression(state),
    }
}

fn parse_let_statement(mut state: State) -> ParserReturn<ASTNode> {
    let mutable = match &state.tokens[state.position..] {
        [TokenKind::Mut, rest @ ..] => {
            let new_pos = rest.len();
            state = state.update(new_pos);
            true
        }
        _ => false,
    };

    let (left, new_state) = parse_left_let_statement(state)?;

    match &new_state.tokens[new_state.position..] {
        [TokenKind::Assign, rest @ ..] => {
            let position = rest.len();
            let (right, new_state) = parse_expression(new_state.update(position))?;
            Ok((
                ASTNode::LetExpression(LetExpression {
                    identifier: left,
                    value: Box::new(right),
                    mutable,
                }),
                new_state,
            ))
        }
        [] => return Err(Error::new(Kind::UnexpectedEOF, "Unexpected EOF", new_state)),
        _ => Err(Error::new(
            Kind::UnexpectedToken,
            "Invalid let statement",
            new_state,
        )),
    }
}

fn parse_left_let_statement<'a>(state: State) -> ParserReturn<IdentifierType> {
    let (ident, state) = parse_identifier(state)?;
    Ok((ident, state))

    // match state.tokens {
    //     [TokenKind::Identifier(n), rest @ ..] => {
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
    //     [TokenKind::LParen(x), rest @ ..] => {
    //         state = state.update(rest, Some(x));
    //         let mut idents: Vec<Identifier> = vec![];
    //         loop {
    //             match state.tokens {
    //                 [TokenKind::RParen(x), rest @ ..] => {
    //                     state = state.update(rest, Some(x));
    //                     ident = Some(IdentifierType::TupleDestructure(idents, None));
    //                     break;
    //                 }
    //                 [TokenKind::Comma(x), rest @ ..] => {
    //                     state = state.update(rest, Some(x));
    //                     continue;
    //                 }
    //                 [TokenKind::Identifier(n), rest @ ..] => {
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
    //     [TokenKind::LBrace(x), rest @ ..] => {
    //         state = state.update(rest, Some(x));
    //         let mut idents: Vec<Identifier> = vec![];
    //         loop {
    //             match state.tokens {
    //                 [TokenKind::RBrace(x), rest @ ..] => {
    //                     state = state.update(rest, Some(x));
    //                     ident = Some(IdentifierType::RecordDestructure((idents, None)));
    //                     break;
    //                 }
    //                 [TokenKind::Comma(x), rest @ ..] => {
    //                     state = state.update(rest, Some(x));
    //                     continue;
    //                 }
    //                 [TokenKind::Identifier(n), rest @ ..] => {
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
    //     [TokenKind::LBracket(x), rest @ ..] => {
    //         state = state.update(rest, Some(x));
    //         let mut idents: Vec<Identifier> = vec![];
    //         loop {
    //             match state.tokens {
    //                 [TokenKind::RBracket(x), rest @ ..] => {
    //                     state = state.update(rest, Some(x));
    //                     ident = Some(IdentifierType::ArrayDestructure((idents, None)));
    //                     break;
    //                 }
    //                 [TokenKind::Comma(x), rest @ ..] => {
    //                     state = state.update(rest, Some(x));
    //                     continue;
    //                 }
    //                 [TokenKind::Identifier(n), rest @ ..] => {
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
    //         [TokenKind::Colon(x), rest @ ..] => {
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
    //         [TokenKind::Assign(x), rest @ ..] => {
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
    let (node, mut new_state) = match &state.tokens[state.position..] {
        [TokenKind::StringLiteral(s), rest @ ..] => {
            (ASTNode::StringLiteral(*s), state.update(rest.len()))
        }
        [TokenKind::NumberLiteral(s), rest @ ..] => {
            let new_state = state.update(rest.len());
            (ASTNode::NumberLiteral(*s), new_state)
        }
        [TokenKind::LParen, rest @ ..] => parse_paren_expression(state.update(rest.len()))?,
        [TokenKind::LBrace, rest @ ..] => parse_brace_expression(state.update(rest.len()))?,
        [TokenKind::LBracket, rest @ ..] => parse_array_literal(state.update(rest.len()))?,
        [TokenKind::Function, TokenKind::LParen, rest @ ..] => {
            let new_pos = rest.len();
            let new_state = state.update(new_pos);
            parse_function(new_state, None).map(|(f, s)| (ASTNode::FunctionDefinition(f), s))?
        }
        [TokenKind::Identifier(_), ..] => {
            let (i, s) = parse_identifier(state)?;
            (ASTNode::Identifier(i), s)
        }
        [x, ..] => Err(Error::new(
            Kind::UnexpectedToken,
            "Invalid expression",
            state,
        ))?,
        [] => Err(Error::new(Kind::UnexpectedEOF, "Unexpected EOF", state))?,
    };

    let (node, new_state) = parse_accessor(new_state, node)?;

    match &new_state.tokens[new_state.position..] {
        [TokenKind::PipeRight, rest @ ..] => {
            let left = Box::new(node);
            let (right, new_state) = parse_expression(new_state.update(rest.len()))?;
            let pipe = PipeRight {
                left,
                right: Box::new(right),
            };
            Ok((ASTNode::PipeRight(pipe), new_state))
        }
        [TokenKind::Assign, rest @ ..] => {
            let left = Box::new(node);
            let (right, new_state) = parse_expression(new_state.update(rest.len()))?;
            let assign = Assign {
                left,
                right: Box::new(right),
            };
            Ok((ASTNode::Assign(assign), new_state))
        }
        [] => Err(Error::new(Kind::UnexpectedEOF, "Unexpected EOF", new_state)),
        _ => Ok((node, new_state)),
    }
}

fn parse_accessor(mut state: State, mut node: ASTNode) -> ParserReturn<ASTNode> {
    loop {
        match state.tokens[state.position..] {
            [TokenKind::Dot, TokenKind::Identifier(mut name), ref rest @ ..] => {
                let new_pos = rest.len();
                let left = Box::new(node);
                let accessor = ASTNode::Accessor(Accessor {
                    left,
                    right: Some(std::mem::take(&mut name)),
                });
                node = accessor;
                state = state.update(new_pos);
            }
            _ => return Ok((node, state)),
        }
    }
}

fn parse_paren_expression(state: State) -> ParserReturn<ASTNode> {
    let delim = Box::new(|tokens: &[TokenKind]| match tokens {
        [TokenKind::RParen, ..] => true,
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

fn parse_brace_expression(mut state: State) -> ParserReturn<ASTNode> {
    let delim = Box::new(|tokens: &[TokenKind]| match tokens {
        [TokenKind::RBrace, ..] => true,
        _ => false,
    });
    match state.tokens[state.position..] {
        [] => return Err(eof_error(state)),
        [TokenKind::Identifier(_), TokenKind::Colon, ..] => return parse_record_literal(state),
        _ => (),
    }
    match internal_parse(state, Delimiter::Func(delim)) {
        Ok((nodes, state)) => {
            let new_node = ASTNode::LogicBlock(nodes);
            return Ok((new_node, state));
        }
        Err(e) => Err(e),
    }
}

fn parse_record_literal(mut state: State) -> ParserReturn<ASTNode> {
    let mut previous_was_comma = false;
    let mut fields = vec![];
    loop {
        match (previous_was_comma || fields.len() < 1, state.tokens) {
            (_, []) => Err(Error::new(
                Kind::UnexpectedEOF,
                "Unexpected EOF",
                &state,
                None,
            ))?,
            (_, [TokenKind::NewLine, rest @ ..]) => {
                state = state.update(rest, None);
                continue;
            }
            (_, [TokenKind::RBrace(x), rest @ ..]) => {
                state = state.update(rest, Some(x));
                break;
            }
            (false, [TokenKind::Comma(x), rest @ ..]) => {
                state = state.update(rest, Some(x));
                previous_was_comma = true;
            }
            (true, [tok @ TokenKind::Comma(_), ..]) => {
                return Err(Error::new(
                    Kind::UnexpectedToken,
                    "Invalid record literal",
                    &state,
                    Some(tok.clone()),
                ));
            }
            (true, [TokenKind::Identifier(name), TokenKind::Colon(x), rest @ ..]) => {
                state = state.update(rest, Some(x));
                let (value, new_state) = parse_expression(state)?;
                state = new_state;
                fields.push(RecordField {
                    name: take_value(name),
                    value,
                });
                previous_was_comma = false;
            }
            (_, [tok, ..]) => {
                return Err(Error::new(
                    Kind::UnexpectedToken,
                    "Invalid record literal",
                    &state,
                    Some(tok.clone()),
                ));
            }
        }
    }
    Ok((ASTNode::RecordLiteral(RecordLiteral { fields }), state))
}

fn parse_array_literal(state: State) -> ParserReturn<ASTNode> {
    let delim = Box::new(|tokens: &[Token]| match tokens {
        [TokenKind::RBracket(_), ..] => true,
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
        [TokenKind::String(s), rest @ ..] => Ok((
            ASTNode::GoImport(GoImport {
                module: s.value.clone(),
                alias: None,
            }),
            state.update(rest, Some(s)),
        )),
        [TokenKind::Identifier(t), TokenKind::String(s), rest @ ..] => Ok((
            ASTNode::GoImport(GoImport {
                module: s.value.clone(),
                alias: Some(t.value.clone()),
            }),
            state.update(rest, Some(s)),
        )),
        [TokenKind::Identifier(t), rest @ ..] => Ok((
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
    //     [TokenKind::ReturnType(_), tail @ ..] => Some(parse_type_literal(state.update(tail, None))),
    //     _ => None,
    // };
    debug_print("Parsing function", &state);
    match state.tokens {
        [TokenKind::LParen(x), rest @ ..] => {
            state = state.update(rest, Some(x));
        }
        // [x, ..] => {
        //     return Err(Error::new(
        //         Kind::UnexpectedToken,
        //         "Invalid function syntax",
        //         &state,
        //         Some(x.clone()),
        //     ));
        // }
        [] => Err(eof_error(&state))?,
        _ => (),
    }
    debug_print("Parsing function 2", &state);
    let (args, new_state) = parse_function_args(state)?;
    state = new_state;

    // Check for a return value
    let explicit_return_type = match state.tokens {
        [TokenKind::ReturnType(x), rest @ ..] => {
            let (type_, new_state) = parse_type_literal(state.update(rest, Some(x)))?;
            state = new_state;
            Some(type_)
        }
        _ => None,
    };

    match state.tokens {
        [TokenKind::LBrace(x), rest @ ..] => {
            state = state.update(rest, Some(x));
            let (logic_block, new_state) = internal_parse(
                state,
                Delimiter::Func(Box::new(|tokens| match tokens {
                    [TokenKind::RBrace(_), ..] => true,
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

fn parse_function_args<'a>(mut state: State) -> Result<(Vec<FunctionArgument>, State), Error> {
    let mut previous_was_comma = false;
    let mut mutable = false;
    let mut args = vec![];
    loop {
        match (
            &state.tokens[state.position..],
            (previous_was_comma || args.len() < 1),
        ) {
            ([], _) => {
                return Err(eof_error(&state));
            }
            ([tok @ TokenKind::Mut(x), rest @ ..], true) => {
                if mutable {
                    return Err(Error::new(
                        Kind::UnexpectedToken,
                        "Invalid function args",
                        &state,
                        Some(state.contexts[state.position]),
                    ));
                }
                mutable = true;
                state = state.update(rest);
            }
            ([TokenKind::Comma(x), rest @ ..], false) => {
                previous_was_comma = true;
                state = state.update(rest, Some(x));
            }
            ([TokenKind::RParen(x), rest @ ..], false) => {
                return Ok((args, state.update(rest, Some(x))));
            }
            ([tok @ TokenKind::RParen(x), rest @ ..], true) => {
                if args.len() == 0 {
                    return Ok((args, state.update(rest, Some(x))));
                }
                return Err(Error::new(
                    Kind::UnexpectedToken,
                    "Invalid function args",
                    &state,
                    Some(tok.clone()),
                ));
            }
            (_, true) => {
                let (i, new_state) = parse_identifier(state)?;
                args.push(FunctionArgument {
                    identifier: i,
                    mutable,
                });
                state = new_state;
                mutable = false;
            }
            _ => {
                return Err(Error::new(
                    Kind::UnexpectedToken,
                    "Invalid function args",
                    &state,
                    None,
                ));
            }
        }
    }
}

fn parse_struct_method_definition(state: State) -> ParserReturn<(String, FunctionDefinition)> {
    match state.tokens {
        [TokenKind::Identifier(t), rest @ ..] => {
            let (type_, new_state) = parse_type_literal(state)?;
            let new_state = match new_state.tokens {
                [TokenKind::RParen(x), rest @ ..] => new_state.update(rest, Some(x)),
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
            ([TokenKind::Comma(x), rest @ ..], false) => {
                state = state.update(rest, Some(x));
                last_was_comma = true;
            }
            ([tok @ TokenKind::Comma(_), ..], true) => {
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
    let ident = match state.tokens {
        [] => return Err(eof_error(&state)),
        [TokenKind::Identifier(name), rest @ ..] => {
            state = state.update(rest, Some(name));
            IdentifierType::Identifier(
                Identifier {
                    value: take_value(name),
                },
                None,
            )
        }
        [TokenKind::LBracket(x), rest @ ..] => {
            let (i, new_state) = recurse_identifier(
                state.update(rest, Some(x)),
                Delimiter::Func(Box::new(|tokens| match tokens {
                    [TokenKind::RBracket(_), ..] => true,
                    _ => false,
                })),
            )?;
            state = new_state;
            IdentifierType::ArrayDestructure(i, None)
        }
        [TokenKind::LBrace(x), rest @ ..] => {
            let (i, new_state) = recurse_identifier(
                state.update(rest, Some(x)),
                Delimiter::Func(Box::new(|tokens| match tokens {
                    [TokenKind::RBrace(_), ..] => true,
                    _ => false,
                })),
            )?;
            state = new_state;
            IdentifierType::RecordDestructure(i, None)
        }
        [TokenKind::LParen(x), rest @ ..] => {
            let (i, new_state) = recurse_identifier(
                state.update(rest, Some(x)),
                Delimiter::Func(Box::new(|tokens| match tokens {
                    [TokenKind::RParen(_), ..] => true,
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
        [TokenKind::Colon(x), rest @ ..] => {
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
        [TokenKind::LBracket(_), TokenKind::RBracket(_), rest @ ..] => {
            (true, state.update(rest, None))
        }
        _ => (false, state),
    };

    let (is_pointer, state) = match state.tokens {
        [TokenKind::Deref(_), rest @ ..] => (true, state.update(rest, None)),
        _ => (false, state),
    };

    match state.tokens {
        [TokenKind::Identifier(t), TokenKind::Dot(_), TokenKind::Identifier(t2), rest @ ..] => {
            let type_ = Type {
                module: Some(t.value.clone()),
                name: t2.value.clone(),
                pointer: is_pointer,
                slice: is_slice,
            };
            Ok((type_, state.update(rest, Some(t2))))
        }
        [TokenKind::Identifier(t), rest @ ..] => {
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
    match &state.tokens[state.position..] {
        [TokenKind::LBrace, rest @ ..] => {
            let (t, new_state) = parse_record_type(state.update(rest))?;
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
        match (check_for_comma, &state.tokens[state.position..]) {
            (_, [TokenKind::NewLine, rest @ ..]) => {
                state = state.update(rest);
                continue;
            }
            (_, [TokenKind::RBrace, rest @ ..]) => {
                state = state.update(rest);
                let record = RecordDefinition { fields };
                return Ok((record, state));
            }
            (false, [TokenKind::Identifier(name), TokenKind::Colon, rest @ ..]) => {
                let (type_, new_state) = parse_type_literal(state.update(rest))?;
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
            (false, [TokenKind::Pub, TokenKind::Identifier(name), TokenKind::Colon, rest @ ..]) => {
                let (type_, new_state) = parse_type_literal(state.update(rest))?;
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
            (true, [TokenKind::Comma, rest @ ..]) => {
                state = state.update(rest);
                check_for_comma = false;
            }
            (_, []) => return Err(eof_error(&state)),
            (_, [x, ..]) => {
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
        match (
            comma_check || fields.len() < 1,
            &state.tokens[state.position..],
        ) {
            (_, [TokenKind::RBrace, rest @ ..]) => {
                state = state.update(rest);
                return Ok((EnumDefiniton { fields }, state));
            }
            (false, [TokenKind::Identifier(name), TokenKind::LParen, rest @ ..]) => {
                let (type_, new_state) = parse_type_literal(state.update(rest))?;
                fields.push((take_value(name), Some(type_)));
                state = new_state;
                comma_check = true;
            }
            (false, [TokenKind::Identifier(name), TokenKind::Comma, rest @ ..]) => {
                fields.push((take_value(name), None));
                state = state.update(rest);
            }
            (true, [TokenKind::Comma, rest @ ..]) => {
                comma_check = false;
            }
            (_, []) => return Err(eof_error(&state)),
            (_, [x, ..]) => {
                return Err(Error::new(
                    Kind::UnexpectedToken,
                    "Invalid enum definition",
                    &state,
                    Some(state.contexts[state.position]),
                ));
            }
        }
    }
}
