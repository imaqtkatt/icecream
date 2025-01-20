#[derive(Debug)]
pub struct Program {
  pub top_levels: Vec<TopLevel>,
}

#[derive(Clone, Debug)]
pub enum TopLevel {
  Macro(crate::loc::Name, Vec<crate::loc::Name>, Tree),
  Tree(Tree),
}

#[derive(Clone)]
pub struct Tree {
  pub tree_kind: TreeKind,
  pub loc: crate::loc::Loc,
}

impl Tree {
  pub fn new(tree_kind: TreeKind, loc: crate::loc::Loc) -> Self {
    Self { tree_kind, loc }
  }
}

impl std::fmt::Debug for Tree {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:?}", self.tree_kind)
  }
}

#[derive(Clone, Debug)]
pub enum TreeKind {
  Variable(crate::loc::Name),
  Literal(Literal),
  Spine(Box<Tree>, Vec<Tree>),
  Binary(Op, Box<Tree>, Box<Tree>),
  Lambda(Box<Tree>, Box<Tree>),
  Let(Box<Tree>, Box<Tree>),
  Block(Vec<Tree>),
}

#[derive(Clone, Debug)]
pub enum Literal {
  Number(i32),
  Boolean(bool),
  String(crate::loc::Text),
}

#[derive(Clone, Copy, Debug)]
pub enum Op {
  Equal,
  NotEqual,
  And,
  Or,
  Add,
  Sub,
  Mul,
  Div,
  Rem,
}
