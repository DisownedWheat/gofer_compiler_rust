use super::ast::*;
use crate::lexer::logos_lexer::Token;
use crate::lexer::tokens::TokenValue;

// Line, column
#[derive(Debug)]
struct State<'a> {
    line: usize,
    column: usize,
    tokens: &'a [Token],
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

type ParserReturn<'a, T> = Result<(T, State<'a>), String>;

pub enum Delimiter {
    Func(Box<dyn Fn(&[Token]) -> bool>),
    None,
}

pub fn parse(tokens: Vec<Token>) -> Result<ASTNode, String> {
    let state = State {
        line: 1,
        column: 1,
        tokens: &tokens[..],
    };
    match parse_top_level(state) {
        Ok((vec, _)) => Ok(ASTNode::Root(vec)),
        Err(e) => Err(e),
    }
}

fn parse_top_level<'a>(mut state: State<'a>) -> Result<(Vec<ASTNode>, State<'a>), String> {
    let mut vec = Vec::<ASTNode>::new();
    let mut is_pub = false;
    loop {
        println!("{:?}", vec);
        if state.tokens.is_empty() {
            break;
        }
        match state.tokens {
            [Token::EOF(_)] => break,
            [Token::Import(_), rest @ ..] => {
                if is_pub {
                    return Err("Cannot have a public import".to_string());
                }
                let (node, new_state) = parse_import(state.update(rest, None))?;
                state = new_state;
                vec.push(node);
                continue;
            }
            [Token::Pub(x), rest @ ..] => {
                if is_pub {
                    return Err("Cannot have multiple public declarations".to_string());
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
            [Token::Function(x), Token::Identifier(_), ..] => {
                let new_tokens = &state.tokens[1..];
                let (node, new_state) = parse_function(state.update(new_tokens, Some(x)))?;
                state = new_state;
                vec.push(ASTNode::TopLevel(
                    is_pub,
                    TopLevel::FunctionDefinition(node),
                ));
                is_pub = false;
            }
            [Token::TypeKeyword(x), Token::Identifier(tok), Token::Assign(_), rest @ ..] => {
                let new_state = state.update(rest, Some(x));
                let (node, new_state) = parse_type_definition(tok.value.clone(), new_state)?;
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
                todo!("Is this necessary?");
                // let (node, new_state) = process(state)?;
                // state = new_state;
                // vec.push(ASTNode::TopLevel(false, Box::new(node)));
            }
        }
    }
    Ok((vec, state))
}

fn internal_parse<'a>(
    mut state: State<'a>,
    delim: Delimiter,
) -> Result<(Vec<ASTNode>, State<'a>), String> {
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

fn take_value(token: &TokenValue) -> ASTString {
    std::mem::take(&mut (*token.value).to_string())
}

fn parse_statement<'a>(state: State<'a>) -> ParserReturn<ASTNode> {
    match state.tokens {
        [Token::Let(x), rest @ ..] => parse_let_statement(state.update(rest, Some(x))),
        _ => process(state),
    }
}

fn parse_let_statement<'a>(state: State<'a>) -> ParserReturn<ASTNode> {
    let (left, new_state) = parse_left_let_statement(state)?;
    match new_state.tokens {
        [Token::Assign(x), rest @ ..] => {
            let (right, new_state) = process(new_state.update(rest, Some(x)))?;
            Ok((
                ASTNode::LetExpression(LetExpression {
                    identifier: left,
                    value: Box::new(right),
                }),
                new_state,
            ))
        }
        _ => Err("Invalid let statement".to_string()),
    }
}

fn parse_left_let_statement<'a>(mut state: State<'a>) -> ParserReturn<IdentifierType> {
    let mut ident: Option<IdentifierType> = None;
    let is_mut = match state.tokens {
        [Token::Mut(x), rest @ ..] => {
            state = state.update(rest, Some(x));
            true
        }
        _ => false,
    };

    match state.tokens {
        [Token::Identifier(n), rest @ ..] => {
            let new_state = state.update(rest, Some(n));
            ident = Some(IdentifierType::Identifier((
                Identifier {
                    value: take_value(n),
                    mutable: is_mut,
                },
                None,
            )));
            state = new_state;
        }
        [Token::LParen(x), rest @ ..] => {
            state = state.update(rest, Some(x));
            let mut idents: Vec<Identifier> = vec![];
            loop {
                match state.tokens {
                    [Token::RParen(x), rest @ ..] => {
                        state = state.update(rest, Some(x));
                        ident = Some(IdentifierType::TupleDestructure((idents, None)));
                        break;
                    }
                    [Token::Comma(x), rest @ ..] => {
                        state = state.update(rest, Some(x));
                        continue;
                    }
                    [Token::Identifier(n), rest @ ..] => {
                        let new_state = state.update(rest, Some(n));
                        let ident = Identifier {
                            value: take_value(n),
                            mutable: is_mut,
                        };
                        idents.push(ident);
                        state = new_state;
                    }
                    _ => return Err("Invalid let statement".to_string()),
                }
            }
        }
        [Token::LBrace(x), rest @ ..] => {
            state = state.update(rest, Some(x));
            let mut idents: Vec<Identifier> = vec![];
            loop {
                match state.tokens {
                    [Token::RBrace(x), rest @ ..] => {
                        state = state.update(rest, Some(x));
                        ident = Some(IdentifierType::RecordDestructure((idents, None)));
                        break;
                    }
                    [Token::Comma(x), rest @ ..] => {
                        state = state.update(rest, Some(x));
                        continue;
                    }
                    [Token::Identifier(n), rest @ ..] => {
                        let new_state = state.update(rest, Some(n));
                        let ident = Identifier {
                            value: take_value(n),
                            mutable: is_mut,
                        };
                        idents.push(ident);
                        state = new_state;
                    }
                    _ => return Err("Invalid let statement".to_string()),
                }
            }
        }
        [Token::LBracket(x), rest @ ..] => {
            state = state.update(rest, Some(x));
            let mut idents: Vec<Identifier> = vec![];
            loop {
                match state.tokens {
                    [Token::RBracket(x), rest @ ..] => {
                        state = state.update(rest, Some(x));
                        ident = Some(IdentifierType::ArrayDestructure((idents, None)));
                        break;
                    }
                    [Token::Comma(x), rest @ ..] => {
                        state = state.update(rest, Some(x));
                        continue;
                    }
                    [Token::Identifier(n), rest @ ..] => {
                        let new_state = state.update(rest, Some(n));
                        let ident = Identifier {
                            value: take_value(n),
                            mutable: is_mut,
                        };
                        idents.push(ident);
                        state = new_state;
                    }
                    _ => return Err("Invalid let statement".to_string()),
                }
            }
        }
        _ => return Err("Invalid let statement".to_string()),
    };

    loop {
        match state.tokens {
            [Token::Colon(x), rest @ ..] => {
                let (t, new_state) = parse_type_literal(state.update(rest, Some(x)))?;
                state = new_state;
                ident = match ident {
                    Some(IdentifierType::Identifier((i, _))) => {
                        Some(IdentifierType::Identifier((i, Some(t))))
                    }
                    _ => return Err("Invalid let statement".to_string()),
                };
            }
            [Token::Assign(x), rest @ ..] => {
                return Ok((ident.unwrap(), state.update(rest, Some(x))));
            }
            _ => {
                return Err(format!(
                    "Invalid let statement at {:?}\n {:?}",
                    state,
                    state.tokens.first().unwrap()
                ))
            }
        }
    }
}

fn process<'a>(state: State<'a>) -> ParserReturn<ASTNode> {
    let (node, new_state) = match &state.tokens {
        [] => (ASTNode::EOF, state),
        [Token::EOF(_), rest @ ..] => (ASTNode::EOF, state.update(rest, None)),
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
        [Token::Function(x) | Token::Let(x), rest @ ..] => {
            parse_function(state.update(rest, Some(x)))
                .map(|(f, s)| (ASTNode::FunctionDefinition(f), s))?
        }
        _ => Err(format!(
            "Not implemented: {:?}",
            state.tokens.first().unwrap()
        ))
        .unwrap(),
    };

    match &new_state.tokens {
        [Token::Pipe(_), rest @ ..] => Ok((node, new_state.update(rest, None))),
        [Token::Assign(_), rest @ ..] => Ok((node, new_state.update(rest, None))),
        _ => Ok((node, new_state)),
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
        _ => Err(format!(
            "Invalid import statement at {:?}\n {:?}",
            state,
            state.tokens.first().unwrap()
        )),
    }
}

fn parse_function(state: State) -> ParserReturn<FunctionDefinition> {
    // let match_return_type = |state: State| match &state.tokens {
    //     [Token::ReturnType(_), tail @ ..] => Some(parse_type_literal(state.update(tail, None))),
    //     _ => None,
    // };

    match state.tokens {
        [Token::Identifier(_), Token::LParen(t), rest @ ..] => {
            todo!("Actually parse named function")
        }
        _ => todo!("parse anonymous function"),
    }
}

fn parse_function_args<'a>(state: State<'a>) -> Result<(Vec<FunctionArgument>, State<'a>), String> {
    let mut previous_was_comma = false;
    let mut mutable = false;
    let mut args = vec![];
    loop {
        match (state.tokens, (previous_was_comma || args.len() < 1)) {
            ([], _) => return Err(format!("Invalid function args at {:?}\n", state)),
            ([Token::RParen(x), rest @ ..], _) => return Ok((args, state.update(rest, Some(x)))),
            _ => todo!("parse_function_args"),
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
        _ => Err(format!(
            "Invalid struct method definition at {:?}\n {:?}",
            state,
            state.tokens.first().unwrap()
        )),
    }
}

fn parse_type_literal(state: State) -> Result<(Type, State), String> {
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
        _ => Err(format!(
            "Invalid type declaration at {:?}\n {:?}",
            state,
            state.tokens.first().unwrap()
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
        println!("{:?}", fields);
        println!("{:?}", &state.tokens[..5]);
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
                    mutable: false,
                };
                fields.push(field);
                state = new_state;
                check_for_comma = true;
            }
            (false, [Token::Mut(_), Token::Identifier(name), Token::Colon(_), rest @ ..]) => {
                let (type_, new_state) = parse_type_literal(state.update(rest, Some(name)))?;
                let n = take_value(name);
                let field = RecordDefinitionField {
                    name: n.clone(),
                    type_: TypeDef::Type(n, type_),
                    mutable: true,
                };
                fields.push(field);
                state = new_state;
                check_for_comma = true;
            }
            (true, [Token::Comma(x), rest @ ..]) => {
                state = state.update(rest, Some(x));
                check_for_comma = false;
            }
            _ => return Err("Invalid record type".to_string()),
        }
    }
}

fn parse_enum(mut state: State) -> ParserReturn<EnumDefiniton> {
    let mut fields: Vec<(ASTString, Option<Type>)> = vec![];
    let mut comma_check = false;
    loop {
        match (comma_check, state.tokens) {
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
            _ => return Err("Invalid enum type".to_string()),
        }
    }
}
