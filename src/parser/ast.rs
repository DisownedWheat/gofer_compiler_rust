// AST NODES

pub type ASTString = String;
pub type LogicBlock = Vec<ASTNode>;
pub type StructMethodDefinition = (ASTString, TypeDeclaration);
pub type ASTNodeIndex = usize;

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
    pub mutable: bool,
}

#[derive(Debug)]
pub enum IdentifierType {
    Identifier((Identifier, Option<TypeDeclaration>)),
    ArrayDestructure((Vec<Identifier>, Option<TypeDeclaration>)),
    RecordDestructure((Vec<Identifier>, Option<TypeDeclaration>)),
    TupleDestructure((Vec<Identifier>, Option<TypeDeclaration>)),
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
    pub identifier: IdentifierType,
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
    pub name: Option<ASTString>,
    pub arguments: Vec<FunctionArgument>,
    pub return_type: Option<Type>,
    pub body: LogicBlock,
    pub pointer: Option<Identifier>,
}

#[derive(Debug)]
pub struct Tuple {
    length: usize,
    values: Vec<TypeDeclaration>,
}

#[derive(Debug)]
pub enum EnumBody {
    Tuple(Tuple),
    Type(TypeDeclaration),
    String(ASTString),
    Number(ASTString),
}

#[derive(Debug)]
pub struct Enum {
    pub name: ASTString,
    fields: Vec<(String, EnumBody)>,
}

#[derive(Debug)]
pub enum TopLevel {
    FunctionDefinition((bool, FunctionDefinition)),
    StructMethodDefinition((bool, FunctionDefinition)),
    RecordDefinition(RecordDefinition),
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
    Enum(Enum),
    Tuple(Tuple),
    TopLevel(bool, Box<ASTNode>),
    NoOp,
    EOF,
}

impl Default for ASTNode {
    fn default() -> Self {
        ASTNode::NoOp
    }
}
