#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
  Identifier,
  Number,
  String,
  True,
  False,
  Let,
  Macro,
  LParens,
  RParens,
  LBrace,
  RBrace,
  FatArrow,
  Equal,
  EqualEqual,
  NotEqual,
  And,
  Or,
  Plus,
  Minus,
  Star,
  Slash,
  Percent,
  Separator,
  Error,
  Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
  pub token_kind: TokenKind,
  pub lexeme: String,
  pub loc: crate::loc::Loc,
}

impl std::fmt::Display for TokenKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      TokenKind::Identifier => write!(f, "identifier"),
      TokenKind::Number => write!(f, "number"),
      TokenKind::String => write!(f, "string"),
      TokenKind::True => write!(f, "true"),
      TokenKind::False => write!(f, "false"),
      TokenKind::Let => write!(f, "let"),
      TokenKind::Macro => write!(f, "macro"),
      TokenKind::LParens => write!(f, "left parenthesis"),
      TokenKind::RParens => write!(f, "right parenthesis"),
      TokenKind::LBrace => write!(f, "left brace"),
      TokenKind::RBrace => write!(f, "right brace"),
      TokenKind::FatArrow => write!(f, "fat arrow"),
      TokenKind::Equal => write!(f, "equal"),
      TokenKind::EqualEqual => write!(f, "double equal"),
      TokenKind::NotEqual => write!(f, "not equal"),
      TokenKind::And => write!(f, "and"),
      TokenKind::Or => write!(f, "or"),
      TokenKind::Plus => write!(f, "plus"),
      TokenKind::Minus => write!(f, "minus"),
      TokenKind::Star => write!(f, "star"),
      TokenKind::Slash => write!(f, "slash"),
      TokenKind::Percent => write!(f, "percent"),
      TokenKind::Separator => write!(f, "separator (\\n or ;)"),
      TokenKind::Error => write!(f, "error"),
      TokenKind::Eof => write!(f, "end of file"),
    }
  }
}
