// AST NODES

pub type ASTString = String;
pub type LogicBlock = Vec<ASTNode>;
pub type TokenPosition = (usize, usize);

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
    Type(ASTString, Type),
    EnumDefinition(ASTString, EnumDefiniton),
    RecordDefinition(ASTString, RecordDefinition),
}

// Records
#[derive(Debug)]
pub struct RecordDefinitionField {
    pub name: ASTString,
    pub type_: TypeDef,
    pub mutable: bool,
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
pub struct EnumDefiniton {
    // pub name: ASTString,
    pub fields: Vec<(ASTString, Option<Type>)>,
}

#[derive(Debug)]
pub enum TopLevel {
    FunctionDefinition(FunctionDefinition),
    StructMethodDefinition(ASTString, FunctionDefinition),
    TopLevelTypeDef(TypeDef),
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
    Enum(EnumDefiniton),
    Tuple(Tuple),
    TypeDefinition(TypeDef),
    TopLevel(bool, TopLevel),
    NoOp,
    EOF,
}

impl Default for ASTNode {
    fn default() -> Self {
        ASTNode::NoOp
    }
}
