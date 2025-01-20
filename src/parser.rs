use crate::{
  lexer::Lexer,
  loc::{Name, Text},
  parse_tree::{Literal, TreeKind},
  token::{Token, TokenKind},
};

type ParseResult<T> = miette::Result<T>;

#[derive(thiserror::Error, Debug, miette::Diagnostic)]
#[error("expected '{expected}'")]
#[diagnostic(code(Parser::expected))]
pub struct Expected {
  pub expected: TokenKind,

  #[label("this token")]
  pub got: crate::loc::Loc,

  #[source_code]
  pub src: crate::loc::Source,
}

#[derive(thiserror::Error, Debug, miette::Diagnostic)]
#[error("unexpected '{token}'")]
#[diagnostic(code(Parser::unexpected))]
pub struct Unexpected {
  pub token: TokenKind,

  #[label("here")]
  pub loc: crate::loc::Loc,

  #[source_code]
  pub src: crate::loc::Source,
}

pub struct Parser<'src> {
  pub lexer: Lexer<'src>,
  pub token: Token,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
  None = 0,
  Lambda,
  Logic,
  Sum,
  Product,
  Call,
  End,
}

impl Precedence {
  fn left(self) -> Self {
    match self {
      Precedence::None => Precedence::Lambda,
      Precedence::Lambda => Precedence::Logic,
      Precedence::Logic => Precedence::Sum,
      Precedence::Sum => Precedence::Product,
      Precedence::Product => Precedence::Call,
      Precedence::Call => Precedence::End,
      Precedence::End => panic!(),
    }
  }
}

impl<'src> Parser<'src> {
  pub fn new(mut lexer: Lexer<'src>) -> Self {
    Self {
      token: lexer.next_token(),
      lexer,
    }
  }

  pub fn parse_program(&mut self) -> ParseResult<crate::parse_tree::Program> {
    let mut top_levels = vec![];
    while !self.is(TokenKind::Eof) {
      top_levels.push(self.top_level()?);
      if self.is(TokenKind::Eof) {
        break;
      }
      self.expect(TokenKind::Separator)?;
    }
    self.expect(TokenKind::Eof)?;
    Ok(crate::parse_tree::Program { top_levels })
  }
}

macro_rules! infix {
  ($self:expr, $lhs:expr, $token:ident, $op:ident, $assoc:ident) => {{
    let lhs_loc = $lhs.loc.clone();
    $self.expect(TokenKind::$token)?;
    let rhs = $self.expression(Precedence::$assoc.left())?;
    let rhs_loc = rhs.loc.clone();
    Ok(crate::parse_tree::Tree {
      tree_kind: TreeKind::Binary(crate::parse_tree::Op::$op, $lhs.into(), rhs.into()),
      loc: lhs_loc.merge(rhs_loc),
    })
  }};
}

impl<'src> Parser<'src> {
  fn is(&self, token_kind: TokenKind) -> bool {
    self.token.token_kind == token_kind
  }

  fn expected<T>(&self, token_kind: TokenKind) -> ParseResult<T> {
    Err(Expected {
      expected: token_kind,
      got: self.token.loc.clone(),
      src: self.lexer.source.clone(),
    })?
  }

  fn unexpected<T>(&self) -> ParseResult<T> {
    Err(Unexpected {
      token: self.token.token_kind,
      loc: self.token.loc.clone(),
      src: self.lexer.source.clone(),
    })?
  }

  fn expect(&mut self, token_kind: TokenKind) -> ParseResult<Token> {
    if self.is(token_kind) {
      Ok(self.advance())
    } else {
      self.expected(token_kind)
    }
  }

  fn peek(&self) -> TokenKind {
    self.token.token_kind
  }

  fn advance(&mut self) -> Token {
    let new_token = self.lexer.next_token();
    std::mem::replace(&mut self.token, new_token)
  }

  fn skip_separator(&mut self) {
    while self.is(TokenKind::Separator) {
      self.advance();
    }
  }

  fn skip_peek(&mut self) -> TokenKind {
    self.skip_separator();
    self.peek()
  }

  fn precedence(&self) -> Precedence {
    match self.peek() {
      TokenKind::RParens | TokenKind::RBrace | TokenKind::Separator => Precedence::End,

      // TODO: fixme
      TokenKind::Equal => Precedence::End,

      TokenKind::FatArrow => Precedence::Lambda,

      TokenKind::EqualEqual | TokenKind::NotEqual => Precedence::Logic,

      TokenKind::Plus | TokenKind::Minus => Precedence::Sum,

      TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Product,

      TokenKind::Identifier
      | TokenKind::Number
      | TokenKind::String
      | TokenKind::True
      | TokenKind::False
      | TokenKind::Let
      | TokenKind::Macro
      | TokenKind::LParens
      | TokenKind::LBrace => Precedence::Call,

      TokenKind::And | TokenKind::Or => todo!(),

      TokenKind::Error | TokenKind::Eof => Precedence::End,
    }
  }

  fn primary(&mut self) -> ParseResult<crate::parse_tree::Tree> {
    match self.skip_peek() {
      TokenKind::Identifier => self.identifier(),
      TokenKind::Number | TokenKind::String | TokenKind::True | TokenKind::False => self.literal(),
      TokenKind::LParens => {
        self.expect(TokenKind::LParens)?;
        let expr = self.expression(Precedence::None.left())?;
        self.expect(TokenKind::RParens)?;
        Ok(expr)
      }
      TokenKind::LBrace => {
        let start = self.expect(TokenKind::LBrace)?;
        let trees = self.many(TokenKind::RBrace)?;
        let end = self.expect(TokenKind::RBrace)?;
        Ok(crate::parse_tree::Tree {
          tree_kind: TreeKind::Block(trees),
          loc: start.loc.merge(end.loc),
        })
      }
      //
      _ => self.unexpected(),
      // TokenKind::Let => todo!(),
      // TokenKind::RParens => todo!(),
      // TokenKind::RBrace => todo!(),
      // TokenKind::FatArrow => todo!(),
      // TokenKind::Equal => todo!(),
      // TokenKind::EqualEqual => todo!(),
      // TokenKind::NotEqual => todo!(),
      // TokenKind::And => todo!(),
      // TokenKind::Or => todo!(),
      // TokenKind::Plus => todo!(),
      // TokenKind::Minus => todo!(),
      // TokenKind::Star => todo!(),
      // TokenKind::Slash => todo!(),
      // TokenKind::Percent => todo!(),
      // TokenKind::Separator => todo!(),
      // TokenKind::Error => todo!(),
      // TokenKind::Eof => todo!(),
    }
  }

  fn name(&mut self) -> ParseResult<(Name, crate::loc::Loc)> {
    let token = self.expect(TokenKind::Identifier)?;
    Ok((Name::from(token.lexeme), token.loc))
  }

  fn identifier(&mut self) -> ParseResult<crate::parse_tree::Tree> {
    let (name, loc) = self.name()?;
    Ok(crate::parse_tree::Tree {
      tree_kind: TreeKind::Variable(name),
      loc,
    })
  }

  fn literal(&mut self) -> ParseResult<crate::parse_tree::Tree> {
    let token = self.advance();
    let tree_kind = match token.token_kind {
      TokenKind::Number => TreeKind::Literal(Literal::Number(token.lexeme.parse().unwrap())),
      TokenKind::String => TreeKind::Literal(Literal::String(Text::from(token.lexeme))),
      TokenKind::True => TreeKind::Literal(Literal::Boolean(true)),
      TokenKind::False => TreeKind::Literal(Literal::Boolean(false)),
      _ => self.unexpected()?,
    };
    Ok(crate::parse_tree::Tree {
      tree_kind,
      loc: token.loc,
    })
  }

  fn infix(&mut self, prec: Precedence) -> ParseResult<crate::parse_tree::Tree> {
    let mut lhs = self.primary()?;

    loop {
      let p = self.precedence();
      if p >= prec && p != Precedence::End {
        let rule = match self.skip_peek() {
          TokenKind::FatArrow => Self::lambda,
          TokenKind::EqualEqual => todo!(),
          TokenKind::NotEqual => todo!(),
          TokenKind::Plus => Self::add,
          TokenKind::Minus => Self::sub,
          TokenKind::Star => Self::mul,
          TokenKind::Slash => Self::div,
          TokenKind::Percent => Self::rem,
          TokenKind::Eof | TokenKind::Error => self.unexpected()?,
          _ => Self::call,
        };
        lhs = rule(self, lhs)?;
      } else {
        break;
      }
    }

    Ok(lhs)
  }

  fn lambda(&mut self, lhs: crate::parse_tree::Tree) -> ParseResult<crate::parse_tree::Tree> {
    let lhs_loc = lhs.loc.clone();
    self.expect(TokenKind::FatArrow)?;
    let rhs = self.expression(Precedence::Lambda)?;
    let rhs_loc = rhs.loc.clone();
    Ok(crate::parse_tree::Tree {
      tree_kind: TreeKind::Lambda(lhs.into(), rhs.into()),
      loc: lhs_loc.merge(rhs_loc),
    })
  }

  fn add(&mut self, lhs: crate::parse_tree::Tree) -> ParseResult<crate::parse_tree::Tree> {
    infix!(self, lhs, Plus, Add, Sum)
  }

  fn sub(&mut self, lhs: crate::parse_tree::Tree) -> ParseResult<crate::parse_tree::Tree> {
    infix!(self, lhs, Minus, Sub, Sum)
  }

  fn mul(&mut self, lhs: crate::parse_tree::Tree) -> ParseResult<crate::parse_tree::Tree> {
    infix!(self, lhs, Star, Mul, Product)
  }

  fn div(&mut self, lhs: crate::parse_tree::Tree) -> ParseResult<crate::parse_tree::Tree> {
    infix!(self, lhs, Slash, Div, Product)
  }

  fn rem(&mut self, lhs: crate::parse_tree::Tree) -> ParseResult<crate::parse_tree::Tree> {
    infix!(self, lhs, Percent, Rem, Product)
  }

  fn call(&mut self, lhs: crate::parse_tree::Tree) -> ParseResult<crate::parse_tree::Tree> {
    let lhs_loc = lhs.loc.clone();
    let rhs = self.expression(Precedence::Call.left())?;
    let rhs_loc = rhs.loc.clone();

    let (callee, mut spine) = match lhs.tree_kind {
      TreeKind::Spine(callee, exprs) => (*callee, exprs),
      _ => (lhs, vec![]),
    };

    spine.push(rhs);

    Ok(crate::parse_tree::Tree {
      tree_kind: TreeKind::Spine(callee.into(), spine),
      loc: lhs_loc.merge(rhs_loc),
    })
  }

  fn expression(&mut self, prec: Precedence) -> ParseResult<crate::parse_tree::Tree> {
    match self.skip_peek() {
      TokenKind::Let => self.r#let(),
      // TokenKind::Macro => self.r#macro(),
      _ => self.infix(prec),
    }
  }

  fn r#let(&mut self) -> ParseResult<crate::parse_tree::Tree> {
    let start = self.expect(TokenKind::Let)?;

    let ident = self.primary()?;

    self.expect(TokenKind::Equal)?;

    let value = self.expression(Precedence::None.left())?;
    let end = value.loc.clone();

    Ok(crate::parse_tree::Tree {
      tree_kind: TreeKind::Let(ident.into(), value.into()),
      loc: start.loc.merge(end),
    })
  }

  fn top_level(&mut self) -> ParseResult<crate::parse_tree::TopLevel> {
    match self.skip_peek() {
      TokenKind::Macro => self.r#macro(),
      _ => self
        .expression(Precedence::None)
        .map(crate::parse_tree::TopLevel::Tree),
    }
  }

  fn r#macro(&mut self) -> ParseResult<crate::parse_tree::TopLevel> {
    self.expect(TokenKind::Macro)?;

    let (name, _) = self.name()?;

    let mut parameters = vec![];
    while !self.is(TokenKind::Equal) {
      let (name, _) = self.name()?;
      parameters.push(name);
    }
    self.expect(TokenKind::Equal)?;

    let body = self.expression(Precedence::None.left())?;

    let r#macro = crate::parse_tree::TopLevel::Macro(name, parameters, body);
    Ok(r#macro)
  }

  fn many(&mut self, end: TokenKind) -> ParseResult<Vec<crate::parse_tree::Tree>> {
    let mut trees = vec![];
    while !self.is(end) {
      trees.push(self.expression(Precedence::None)?);
      if self.is(end) {
        break;
      }
      self.expect(TokenKind::Separator)?;
    }
    Ok(trees)
  }
}

#[cfg(test)]
mod test {
  use crate::{lexer::Lexer, loc::Source};

  use super::Parser;

  #[test]
  fn parser_test() {
    let src = r#"
macro letfn name params body = {
  !
}
  let name = params => body

      {
    let f = x => y z => y 2 3
    4
    }
      "#;
    let source = Source::from(src);
    let peekable = &source.get_text();

    let lexer = Lexer {
      source,
      peekable: peekable.chars().peekable(),
      index: 0,
      start: 0,
    };
    let mut parser = Parser::new(lexer);
    match parser.parse_program() {
      Ok(tree) => println!("{tree:?}"),
      Err(e) => eprintln!("{e}"),
    }
  }
}
