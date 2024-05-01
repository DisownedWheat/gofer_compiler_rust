// AST NODES

pub type ASTString = String;
pub type LogicBlock = Vec<ASTNode>;
pub type StructMethodDefinition = (ASTString, TypeDeclaration);

// Imports
#[derive(Debug)]
pub struct GoferImport {
    pub module: ASTString,
}

#[derive(Debug)]
pub struct GoImport {
    pub module: ASTString,
    pub alias: Option<ASTString>,
}

// Identifiers

#[derive(Debug)]
pub struct Identifier {
    pub value: ASTString,
    pub type_: Option<Type>,
}

#[derive(Debug)]
pub enum IdentifierType {
    Identifier(Identifier),
    TypedIdentifier(Identifier),
    ArrayDestructure(Vec<Identifier>),
    RecordDestructure(Vec<Identifier>),
    TupleDestructure(Vec<Identifier>),
}

// Types
#[derive(Debug)]
pub struct Type {
    pub module: Option<ASTString>,
    pub value: ASTString,
}

#[derive(Debug)]
pub struct TypeDeclaration {
    pub name: ASTString,
    pub module: Option<ASTString>,
    pub pointer: bool,
    pub slice: bool,
}

// Records
#[derive(Debug)]
pub struct RecordDefinitionField {
    pub name: ASTString,
    pub type_: Type,
}

#[derive(Debug)]
pub struct RecordDefinition {
    pub fields: Vec<RecordDefinitionField>,
}

#[derive(Debug)]
pub struct RecordField {
    pub name: ASTString,
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

// Functions
#[derive(Debug)]
pub struct FunctionArgument {
    pub identifier: Identifier,
    pub type_: Type,
    pub scoped_name: Option<ASTString>,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: ASTString,
    pub arguments: Vec<FunctionArgument>,
    pub body: LogicBlock,
    pub pointer: Option<Identifier>,
}

#[derive(Debug)]
pub enum ASTNode {
    Root(Vec<ASTNode>),
    LogicBlock(LogicBlock),
    GoImport(GoImport),
    GoferImport(GoferImport),
    Identifier(IdentifierType),
    RecordDefinition(RecordDefinition),
    RecordLiteral(RecordLiteral),
    LetExpression(LetExpression),
    FunctionDefinition(FunctionDefinition),
    ParenExpression(Option<Box<ASTNode>>),
    ArrayLiteral(Vec<ASTNode>),
    StringLiteral(ASTString),
    NumberLiteral(ASTString),
    NoOp,
    EOF,
}

impl Default for ASTNode {
    fn default() -> Self {
        ASTNode::NoOp
    }
}
