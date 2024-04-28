use super::parser::*;

use crate::lexer::tokens::Token;

pub enum Delimiter {
    Func(Box<dyn Fn(&ParserState, &[Token]) -> bool>),
    None,
}

pub struct ParserState {
    pub ast_buffer: Vec<ASTNode>,
    pub delimiter: Delimiter,
    pub parent_state: Option<Box<ParserState>>,
    pub type_: StateAST,
}

impl ParserState {
    pub fn new(type_: StateAST, delimiter: Delimiter) -> Self {
        ParserState {
            ast_buffer: vec![],
            delimiter,
            parent_state: None,
            type_,
        }
    }

    pub fn add_to_buffer(&mut self, ast: ASTNode) -> &mut Self {
        self.ast_buffer.push(ast);
        return self;
    }
}

pub enum StateAST {
    Root,
    ParenExpression,
    ArrayLiteral,
    RecordDefinition,
    RecordDeclaration,
    LogicBlock,
}

type ASTNodeParserResult<'a> = Result<&'a [Token], String>;

pub trait ASTNodeParser<'a> {
    fn parse(&mut self, tokens: &'a [Token]) -> ASTNodeParserResult<'a>;
}

// AST NODES

type LogicBlock = Vec<ASTNode>;

#[derive(Debug)]
pub struct Type {
    pub module: Option<String>,
    pub value: String,
}

impl<'a> ASTNodeParser<'a> for Type {
    fn parse(&mut self, tokens: &'a [Token]) -> ASTNodeParserResult<'a> {
        match tokens {
            [] => Err("Invalid type format".to_string()),
            [Token::Identifier(module), Token::Dot(_), Token::Identifier(value), rest @ ..] => {
                self.module = Some(module.value.clone());
                self.value = value.value.clone();
                Ok(rest)
            }
            [Token::Identifier(value), rest @ ..] => {
                self.value = value.value.clone();
                Ok(rest)
            }
            _ => Err("Invalid type format".to_string()),
        }
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub value: String,
    pub type_: Option<Type>,
}

impl<'a> ASTNodeParser<'a> for Identifier {
    fn parse(&mut self, tokens: &'a [Token]) -> ASTNodeParserResult<'a> {
        match tokens {
            [] => Err("Invalid identifier format".to_string()),
            [Token::Identifier(value), Token::Colon(_), rest @ ..] => {
                self.value = value.value.clone();
                let mut type_ = Type {
                    module: None,
                    value: "".to_string(),
                };
                let rest = type_.parse(rest)?;
                self.type_ = Some(type_);
                Ok(rest)
            }
            [Token::Identifier(value), rest @ ..] => {
                self.value = value.value.clone();
                Ok(rest)
            }
            _ => Err("Invalid identifier format".to_string()),
        }
    }
}

enum ImportType {
    GoferImport(GoferImport),
    GoImport(GoImport),
}

impl<'a> ASTNodeParser<'a> for ImportType {
    fn parse(&mut self, tokens: &'a [Token]) -> ASTNodeParserResult<'a> {
        match tokens {
            [] => Err("Invalid import".to_string()),
            [Token::Identifier(value), Token::String(str), rest @ ..] => {
                let import = GoImport {
                    module: str.value,
                    alias: Some(value.value),
                };
                self = &mut ImportType::GoImport(import);
                return Ok(rest);
            }
        }
    }
}

#[derive(Debug)]
pub struct GoferImport {
    pub module: String,
}

#[derive(Debug)]
pub struct GoImport {
    pub module: String,
    pub alias: Option<String>,
}

#[derive(Debug)]
pub struct RecordDefinitionField {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug)]
pub struct RecordDefinition {
    pub fields: Vec<RecordDefinitionField>,
}

#[derive(Debug)]
pub struct RecordField {
    pub name: String,
    pub value: ASTNode,
}

#[derive(Debug)]
pub struct RecordLiteral {
    pub fields: Vec<RecordField>,
}

#[derive(Debug)]
pub struct LetExpression {
    pub identifier: Identifier,
    pub value: Box<ASTNode>,
}

#[derive(Debug)]
pub struct FunctionArgument {
    pub identifier: Identifier,
    pub type_: Type,
    pub scoped_name: Option<String>,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub arguments: Vec<FunctionArgument>,
    pub body: LogicBlock,
    pub pointer: Option<Identifier>,
}

#[derive(Debug)]
pub enum ASTNode {
    Root(Vec<ASTNode>),
    GoImport(GoImport),
    GoferImport(GoferImport),
    Identifier(Identifier),
    RecordDefinition(RecordDefinition),
    RecordLiteral(RecordLiteral),
    LetExpression(LetExpression),
    FunctionDefinition(FunctionDefinition),
    ParenExpression(Option<Box<ASTNode>>),
    ArrayLiteral(Vec<ASTNode>),
    StringLiteral(String),
    NoOp,
    EOF,
}
