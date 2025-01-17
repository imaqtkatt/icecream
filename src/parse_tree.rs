// #[derive(Debug)]
pub struct Tree {
  pub tree_kind: TreeKind,
  pub loc: crate::loc::Loc,
}

impl std::fmt::Debug for Tree {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:?}", self.tree_kind)
  }
}

#[derive(Debug)]
pub struct Program {
  pub trees: Vec<Tree>,
}

#[derive(Debug)]
pub enum TreeKind {
  Variable(crate::loc::Name),
  Literal(Literal),
  Spine(Vec<Tree>),
  Binary(Op, Box<Tree>, Box<Tree>),
  Lambda(Box<Tree>, Box<Tree>),
  Let(Box<Tree>, Box<Tree>),
  Block(Vec<Tree>),
}

#[derive(Debug)]
pub enum Literal {
  Number(i32),
  Boolean(bool),
  String(crate::loc::Text),
}

#[derive(Debug)]
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
