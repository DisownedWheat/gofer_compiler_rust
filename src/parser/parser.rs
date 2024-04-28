use super::ast::*;
use crate::lexer::tokens::Token;

enum ParserReturnTypes<'a> {
    Expression(ParserReturn<'a>),
    Parent(ParserState),
}

type ParserReturn<'a> = Result<(ParserState, &'a [Token]), String>;

pub fn parse(mut tokens: &[Token]) -> Result<ASTNode, String> {
    let mut state = ParserState::new(StateAST::Root, Delimiter::None);
    loop {
        if tokens.len() <= 0 {
            break;
        }
        match process(state, tokens) {
            Ok((new_state, new_tokens)) => {
                state = new_state;
                tokens = new_tokens;
            }
            Err(e) => return Err(e),
        }
    }
    loop {
        let t = &state.type_;
        match t {
            StateAST::Root => break,
            _ => match unpack_state(state, tokens) {
                Ok((new_state, _)) => state = new_state,
                Err(e) => return Err(e),
            },
        }
    }
    Ok(ASTNode::Root(state.ast_buffer))
}

fn add_to_buffer<'a>(state: &mut ParserState, node: ASTNode) -> &mut ParserState {
    state.ast_buffer.push(node);
    return state;
}

fn unpack_state<'a>(mut state: ParserState, tokens: &'a [Token]) -> ParserReturn<'a> {
    match (state).type_ {
        StateAST::ParenExpression => {
            let mut parent = (state).parent_state.unwrap();
            let mut children = std::mem::take(&mut state.ast_buffer);
            let length = children.len();
            if length < 1 {
                let new_node = ASTNode::ParenExpression(None);
                state = *add_to_buffer(&mut state, new_node);
                Ok((state, &tokens[1..]))
            } else {
                let child = std::mem::replace(&mut children[0], ASTNode::NoOp);
                let new_ast = ASTNode::ParenExpression(Some(Box::new(child)));
                let new_state = add_to_buffer(&mut parent, new_ast);
                return Ok((*new_state, &tokens[1..]));
            }
        }
        _ => Err("Not implemented".to_string()),
    }
}

fn process<'a>(state: ParserState, tokens: &'a [Token]) -> ParserReturn<'a> {
    match &(state).delimiter {
        Delimiter::None => (),
        Delimiter::Func(f) => {
            if f(&state, &tokens) {
                return unpack_state(state, tokens);
            }
        }
    }
    match tokens {
        [] => {
            let mut tempt_state = state;
            Ok((*add_to_buffer(&mut tempt_state, ASTNode::EOF), tokens))
        }
        [Token::String(s), rest @ ..] => {
            let mut temp_state = state;
            Ok((
                *add_to_buffer(&mut temp_state, ASTNode::StringLiteral(s.value.clone())),
                rest,
            ))
        }
        [Token::Import(_), rest @ ..] => parse_import(state, rest),
        [Token::LParen(_), rest @ ..] => {
            let mut new_state = ParserState::new(
                StateAST::ParenExpression,
                Delimiter::Func(Box::new(|_, tokens: &[Token]| match tokens {
                    [Token::RParen(_), ..] => true,
                    _ => false,
                })),
            );
            (new_state).parent_state = Some(Box::new(state));
            return Ok((new_state, rest));
        }
        _ => {
            let mut temp_state = state;
            Ok((
                *add_to_buffer(&mut temp_state, ASTNode::Root(vec![])),
                &tokens[1..],
            ))
        }
    }
}

fn parse_import<'a>(mut state: ParserState, tokens: &'a [Token]) -> ParserReturn<'a> {
    match tokens {
        [Token::String(s), Token::NewLine(_), rest @ ..] => Ok((
            *add_to_buffer(
                &mut state,
                ASTNode::GoImport(GoImport {
                    module: s.value.clone(),
                    alias: None,
                }),
            ),
            rest,
        )),
        [Token::Identifier(t), Token::String(s), Token::NewLine(_), rest @ ..] => {
            let mut temp_state = state;
            let s = add_to_buffer(
                &mut temp_state,
                ASTNode::GoImport(GoImport {
                    module: s.value.clone(),
                    alias: Some(t.value.clone()),
                }),
            );
            Ok((*s, rest))
        }
        [Token::Identifier(t), Token::NewLine(_), rest @ ..] => Ok((
            *add_to_buffer(
                &mut state,
                ASTNode::GoferImport(GoferImport {
                    module: t.value.clone(),
                }),
            ),
            rest,
        )),
        _ => Err(format!(
            "Invalid import statement {:?}",
            tokens.first().unwrap()
        )),
    }
}
