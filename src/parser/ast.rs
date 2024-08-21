// AST NODES

pub type ASTString = String;
pub type LogicBlock = Vec<ASTNode>;
pub type StructMethodDefinition = (ASTString, Type);
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
    Identifier((Identifier, Option<Type>)),
    ArrayDestructure((Vec<Identifier>, Option<Type>)),
    RecordDestructure((Vec<Identifier>, Option<Type>)),
    TupleDestructure((Vec<Identifier>, Option<Type>)),
}

// Types

#[derive(Debug)]
pub struct Type {
    pub name: ASTString,
    pub module: Option<ASTString>,
    pub pointer: bool,
    pub slice: bool,
}

#[derive(Debug)]
pub enum TypeDef {
    Type(Type),
    Enum(Enum),
    RecordDefinition(RecordDefinition),
}

// Records
#[derive(Debug)]
pub struct RecordDefinitionField {
    pub name: ASTString,
    pub type_: TypeDef,
}

#[derive(Debug)]
pub struct RecordDefinition {
    pub fields: Vec<RecordDefinitionField>,
}

#[derive(Debug)]
pub struct RecordField {
    pub name: ASTString,
    pub value: ASTNode,
    pub public: bool,
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
    values: Vec<Type>,
}

#[derive(Debug)]
pub struct Enum {
    pub name: ASTString,
    fields: Vec<(String, Option<Type>)>,
}

#[derive(Debug)]
pub enum TopLevel {
    FunctionDefinition((bool, FunctionDefinition)),
    StructMethodDefinition((bool, FunctionDefinition)),
    TypeDef((bool, TypeDef)),
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
    TypeDef(TypeDef),
    TopLevel(bool, Box<ASTNode>),
    NoOp,
    EOF,
}

impl Default for ASTNode {
    fn default() -> Self {
        ASTNode::NoOp
    }
}
