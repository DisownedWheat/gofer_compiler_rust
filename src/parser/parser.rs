use super::ast::*;
use crate::lexer::tokens::{Token, TokenValue};

// Line, column
type State = (usize, usize);

type ParserReturn<'a, T> = Result<(T, &'a [Token], State), String>;
pub enum Delimiter {
    Func(Box<dyn Fn(&[Token]) -> bool>),
    None,
}

pub fn parse(tokens: &[Token]) -> Result<ASTNode, String> {
    match internal_parse((1, 1), tokens, Delimiter::None) {
        Ok((vec, _, _)) => Ok(ASTNode::Root(vec)),
        Err(e) => Err(e),
    }
}

fn ignore_newlines(tokens: &[Token]) -> &[Token] {
    let mut index = 0;
    loop {
        match tokens[index] {
            Token::NewLine(_) => index += 1,
            _ => break,
        }
    }
    &tokens[index..]
}

fn internal_parse(
    mut state: State,
    mut tokens: &[Token],
    delim: Delimiter,
) -> Result<(Vec<ASTNode>, &[Token], State), String> {
    let mut vec = Vec::<ASTNode>::new();
    loop {
        if tokens.len() <= 0 {
            break;
        }
        match delim {
            Delimiter::Func(ref f) => {
                if f(&tokens) {
                    return Ok((vec, &tokens[1..], state));
                }
            }
            _ => (),
        }

        match process(state, tokens) {
            Ok((new_node, new_tokens, new_state)) => {
                state = new_state;
                vec.push(new_node);
                tokens = new_tokens;
            }
            Err(e) => return Err(e),
        }
    }
    return Ok((vec, tokens, state));
}

fn update_state(token: &TokenValue) -> State {
    (token.line, token.column)
}

fn take_value(token: &TokenValue) -> ASTString {
    std::mem::take(&mut (*token.value).to_string())
}

fn process(state: State, tokens: &[Token]) -> ParserReturn<ASTNode> {
    let (node, new_tokens, new_state) = match tokens {
        [] => (ASTNode::EOF, tokens, state),
        [Token::EOF(_), rest @ ..] => (ASTNode::EOF, rest, state),
        [Token::NewLine(x), rest @ ..] => (ASTNode::NoOp, rest, update_state(x)),
        [Token::String(s), rest @ ..] => {
            (ASTNode::StringLiteral(take_value(s)), rest, update_state(s))
        }
        [Token::Number(s), rest @ ..] => (
            ASTNode::NumberLiteral(s.value.clone()),
            rest,
            update_state(s),
        ),
        [Token::Import(_), rest @ ..] => parse_import(state, rest)?,
        [Token::LParen(x), rest @ ..] => parse_paren_expression(update_state(x), rest)?,
        [Token::LBrace(x), rest @ ..] => parse_brace_expression(update_state(x), rest)?,
        [Token::LBracket(x), rest @ ..] => parse_array_literal(update_state(x), rest)?,
        [Token::Function(x) | Token::Let(x), rest @ ..] => parse_function(update_state(x), rest)?,
        _ => Err(format!("Not implemented: {:?}", tokens.first().unwrap())).unwrap(),
    };

    match new_tokens {
        [Token::Pipe(_), rest @ ..] => Ok((node, rest, new_state)),
        [Token::Assign(_), rest @ ..] => Ok((node, rest, new_state)),
        _ => Ok((node, new_tokens, new_state)),
    }
}

fn parse_paren_expression(state: State, tokens: &[Token]) -> ParserReturn<ASTNode> {
    let delim = Box::new(|tokens: &[Token]| match tokens {
        [Token::RParen(_), ..] => true,
        _ => false,
    });
    match internal_parse(state, tokens, Delimiter::Func(delim)) {
        Ok((mut nodes, remaining, state)) => {
            let first = std::mem::take(&mut nodes[0]);
            let new_node = ASTNode::ParenExpression(Some(Box::new(first)));
            return Ok((new_node, remaining, state));
        }
        Err(e) => Err(e),
    }
}

fn parse_brace_expression(state: State, tokens: &[Token]) -> ParserReturn<ASTNode> {
    let delim = Box::new(|tokens: &[Token]| match tokens {
        [Token::RBrace(_), ..] => true,
        _ => false,
    });
    match internal_parse(state, tokens, Delimiter::Func(delim)) {
        Ok((nodes, remaining, state)) => {
            let new_node = ASTNode::LogicBlock(nodes);
            return Ok((new_node, remaining, state));
        }
        Err(e) => Err(e),
    }
}

fn parse_array_literal(state: State, tokens: &[Token]) -> ParserReturn<ASTNode> {
    let delim = Box::new(|tokens: &[Token]| match tokens {
        [Token::RBracket(_), ..] => true,
        _ => false,
    });
    match internal_parse(state, tokens, Delimiter::Func(delim)) {
        Ok((nodes, remaining, state)) => {
            let new_node = ASTNode::ArrayLiteral(nodes);
            return Ok((new_node, remaining, state));
        }
        Err(e) => Err(e),
    }
}

fn parse_import(state: State, tokens: &[Token]) -> ParserReturn<ASTNode> {
    match tokens {
        [Token::String(s), Token::NewLine(_), rest @ ..] => Ok((
            ASTNode::GoImport(GoImport {
                module: s.value.clone(),
                alias: None,
            }),
            rest,
            update_state(s),
        )),
        [Token::Identifier(t), Token::String(s), Token::NewLine(_), rest @ ..] => Ok((
            ASTNode::GoImport(GoImport {
                module: s.value.clone(),
                alias: Some(t.value.clone()),
            }),
            rest,
            update_state(s),
        )),
        [Token::Identifier(t), Token::NewLine(_), rest @ ..] => Ok((
            ASTNode::GoferImport(GoferImport {
                module: t.value.clone(),
            }),
            rest,
            update_state(t),
        )),
        _ => Err(format!(
            "Invalid import statement at {:?}\n {:?}",
            state,
            tokens.first().unwrap()
        )),
    }
}

fn parse_function(state: State, tokens: &[Token]) -> ParserReturn<ASTNode> {
    let match_return_type = |state: State, tokens: &[Token]| match tokens {
        [Token::ReturnType(_), tail @ ..] => Some(parse_type_literal(state, tokens)),
        _ => None,
    };

    match tokens {
        [Token::Identifier(x), Token::LParen(t), rest @ ..] => {}
        [Token::LParen(_), rest @ ..] => match parse_struct_method_definition(state, tokens) {},
    }
}

fn parse_struct_method_definition(
    state: State,
    tokens: &[Token],
) -> ParserReturn<StructMethodDefinition> {
    match tokens {
        [Token::Identifier(t), rest @ ..] => {
            let (type_, tokens, new_state) = parse_type_literal(state, rest)?;
            let (tokens, new_state) = match tokens {
                [Token::RParen(x), rest @ ..] => (rest, update_state(x)),
                _ => (tokens, state),
            };
            return Ok(((take_value(t), type_), rest, new_state));
        }
        _ => Err(format!(
            "Invalid struct method definition at {:?}\n {:?}",
            state,
            tokens.first().unwrap()
        )),
    }
}

fn parse_function_args(
    state: State,
    tokens: &[Token],
) -> Result<(Vec<FunctionArgument>, &[Token], State), String> {
    let mut previous_was_comma = false;
    let mut mutable = false;
    let mut args = vec![];
    loop {
        match (tokens, (previous_was_comma || args.len() < 1)) {
            ([], _) => return Err(format!("Invalid function args at {:?}\n", state)),
            ([Token::RParen(x), rest @ ..], _) => return Ok((args, rest, update_state(x))),
        }
    }
}

fn parse_type_literal(
    state: State,
    tokens: &[Token],
) -> Result<(TypeDeclaration, &[Token], State), String> {
    let (is_slice, tokens) = match tokens {
        [Token::LBracket(_), rest @ ..] => (true, rest),
        _ => (false, tokens),
    };

    let (is_pointer, tokens) = match tokens {
        [Token::Deref(_), rest @ ..] => (true, rest),
        _ => (false, tokens),
    };

    match tokens {
        [Token::Identifier(t), Token::Dot(_), Token::Identifier(t2), rest @ ..] => {
            let type_ = TypeDeclaration {
                module: Some(t.value.clone()),
                name: t2.value.clone(),
                pointer: is_pointer,
                slice: is_slice,
            };
            Ok((type_, rest, update_state(t2)))
        }
        [Token::Identifier(t), rest @ ..] => {
            let type_ = TypeDeclaration {
                module: None,
                name: t.value.clone(),
                pointer: is_pointer,
                slice: is_slice,
            };
            Ok((type_, rest, update_state(t)))
        }
        _ => Err(format!(
            "Invalid type declaration at {:?}\n {:?}",
            state,
            tokens.first().unwrap()
        )),
    }
}
